rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
DataDir <-file.path(NuDir, "data", "nigeria")
ProjDir <- file.path(NuDir, "projects", "urban_malaria")
SampDir <- file.path(ProjDir, "sampling")
DHS_Dir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data')
Raster_Dir <- file.path(DHS_Dir,'Raster_files')
DHS_data <- file.path(DHS_Dir, 'DHS')
CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')

## -----------------------------------------
### libraries and functions 
## -----------------------------------------
library(sf)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(geojsonio)
library(tmap)
library(biscale)
library(cowplot)
library(mclust)
library(raster)
library(rgdal)
library(clustvarsel)

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=16, colour = 'black'), 
        legend.text =element_text(size = 16, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

con_gplot <-function(df,fill,label){
  ggplot()+
    geom_sf(data=df, mapping=aes(fill = !!fill)) +
    map_theme() +
    geom_text_repel(
      data = df,
      aes(label = !!label, geometry = geometry),color ='black',
      stat = "sf_coordinates", 
      min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

bar_fun = function(df, x, y, fill, scale_fill, size_x_text, size_y_text, size_title_x, size_title_y, xlab){
  ggplot(df, aes(x=!!x, y =!!y, fill=!!fill))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = scale_fill)+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = ggplot2::element_text(size =size_x_text),
          axis.text.y = ggplot2::element_text(size = size_y_text),
          axis.title.x = element_text(size = size_title_x),
          axis.title.y = element_text(size =size_title_y))+
    xlab(xlab)+
    ylab('')
}


# ----------------------------------------------
### Visualizing urban ward and LGA boundaries  
## ---------------------------------------------
## read ward shape files and show only wards in ibadan  
ib_wall = st_read(file.path(DataDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>% 
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) 

ib_wm=ggplot(ib_wall) +
  geom_sf(color=alpha('#8971B3', 0.5))+
  geom_text_repel(
    data = ib_wall,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  #labs(title= "Wards in Ibadan")+
  xlab("")+
  ylab("")

#ggsave(paste0(SampDir, '/results/', Sys.Date(), 'wards_ibadan_metro.pdf'), ib_wm, width = 10, height =8)

#read and visualize LGA shape files
ib_lall = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Ibadan_metro_fiveLGAshapes", "Ibadan_metro_fiveLGAshapes.shp"))

ib_lmall=ggplot(ib_lall) +
  geom_sf(color=alpha('#8971B3', 0.5))+
  geom_text_repel(
    data = ib_lall,
    aes(label =  LGAName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  #labs(title= "LGAs in Ibadan")+
  xlab("")+
  ylab("")

ggsave(paste0(SampDir, '/results/', Sys.Date(), 'LGAs_ibadan.pdf'), ib_lmall, width = 10, height =8)


#change crs to faciliate join 
st_crs(ib_wall)<- 4326
st_crs(ib_lall)<- 4326


#join ward files to LGAs files to obtain data from LGA files and visualize 
df_wall=st_join(ib_wall, ib_lall, join =st_within)

ib_wm_catall=ggplot(df_wall) +
  geom_sf(color=alpha('#8971B3', 0.5))+
  geom_text_repel(
    data = df_wall,
    aes(label =  LGAName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1)+
  map_theme() + 
  #labs(title= "All LGAs and wards in Ibadan")+
  xlab("")+
  ylab("")

#ggsave(paste0(SampDir, '/results/', Sys.Date(), 'LGAs_wards_ibadan.png'),ib_wm_catall, width = 10, height =5)



#####################Visualizing variables for visualization################################################################



# ---------------------------------------------------------------
### Visualizing population sizes for all wards  
## -------------------------------------------------------------

#read visualize Ward Population shape files
ib_wpall = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp"))%>%
  mutate(wrd_nm_x = ifelse(wrd_nm_x == 'Oranyan' & lga_cod == '31007', 'Oranyan_7', wrd_nm_x)) %>% 
  filter(wrd_nm_x %in% ib_wall$WardName)%>%
  filter(stat_cd=='OY')%>%
  filter(amap_cd!='NIE OYS GBH')%>%
  filter(amap_cd!='NIE OYS JND')%>%
  filter(amap_cd!='NIE OYS AJW')%>%
  filter(amap_cd!='NIE OYS FMT')%>%
  filter(amap_cd!='NIE OYS YYY')%>%
  filter(amap_cd!='NIE OYS RUW')%>%
  arrange(wrd_nm_x)

ib_wmall = con_gplot(ib_wpall,quo(mean), quo(wrd_nm_x))+
  scale_fill_continuous(name='Ward population \n count, Ibadan', low="thistle2", high="darkred", guide="colorbar",na.value="transparent")

#ggsave(paste0(SampDir, '/results/', Sys.Date(), 'wards_populate_ibadan.png'), ib_wmall, width = 12, height =8)



# ---------------------------------------------------------------
### Visualizing population density for all wards  
## -------------------------------------------------------------
#population density
raster <- raster(file.path(Raster_Dir, "NGA_pop_density", "gpw_v4_population_density_rev11_2020_1_deg.tif"))
val_popndens <- raster::extract(raster,ib_wall, buffer = buffer, fun = mean, df =TRUE)
val_popndens = val_popndens$gpw_v4_population_density_rev11_2020_1_deg
df_ward_popden = cbind(ib_wall, val_popndens)

ib_w_popden = con_gplot(df_ward_popden,quo( val_popndens), quo(WardName))+
  scale_fill_continuous(name='Ward population \n density, Ibadan', low="cornsilk", high="cornsilk4", guide="colorbar",na.value="transparent")


# ---------------------------------------------------------------
### Visualizing settlements  
## --------------------------------------------------------------

#settlement type overall 
df = st_read(file.path(DataDir, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Oyo', landuse =='Residential')

ib_w_block=st_join(ib_wall, df, join =st_overlaps)
ib_bar_dat = ib_w_block
st_geometry(ib_bar_dat) <- NULL

ib_bar_overall = ib_bar_dat %>% dplyr::select(type) %>%  group_by(type) %>%  summarise(number=n())
scale_fill = c("darkorchid1", "firebrick1", "forestgreen", "darksalmon", "burlywood4")
settle_type = bar_fun(ib_bar_overall, quo(type), quo(number), quo(type), scale_fill, 20, 20, 20, 20, 'Settlement type per neighborhood')

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_overall_bars.png'),settle_type, width = 6, height =3)


# ---------------------------------------------------------------
### Visualizing informality 
## --------------------------------------------------------------
df_inform <- geojson_read(file.path(DataDir, 'kano_ibadan_informality', "kano_ibadan_settlements.geojson"),  what = "sp")
inform_sf <- st_as_sf(df_inform) %>%  filter(regions == 'Ibadan')
df_wall = ib_wall%>%dplyr::select(-c(StateCode, Timestamp, AMAPCODE))
ib_w_inform=st_join(df_wall, inform_sf, join =st_overlaps)%>% 
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) %>% 
  dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity))

# all wards 
settle_type_w = ggplot() + 
  geom_sf(data =ib_w_inform, mapping = aes(fill = mean_k)) +
  scale_fill_gradient(name = 'K complexity', low = 'mintcream', high = 'plum')+
  geom_text_repel(
    data = ib_w_inform,
    aes(label = WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", 
    min.segment.length = 0, size = 5, force = 1)+
  map_theme()+
  xlab('')+
  ylab('')

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_all_wards.pdf'),settle_type_w) 

#creating a population and informality visualization combined 
st_geometry(ib_w_inform)<- NULL
pop_inform = left_join(ib_wpall, ib_w_inform, by = c('wrd_nm_x' = 'WardName'))
dt = bi_class(pop_inform, x = mean, y = mean_k, style = "fisher", dim = 3)
map <- ggplot() +
  geom_sf(data = dt, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_text_repel(
    data = dt,
    aes(label = wrd_nm_x, geometry = geometry),color ='black',
    stat = "sf_coordinates", 
    min.segment.length = 0, size = 5, force = 1)+
  xlab('')+
  ylab('')+
  bi_theme()
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "High population ",
                    ylab = "High K complexity ",
                    size = 7.5)
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .65, 0.2, 0.2)

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_population_all_wards.pdf'),finalPlot)


#------------------------------------------------------------------------------
###Visualizing EVI data 
#------------------------------------------------------------------------------
files = list.files(path = file.path(DataDir, "EVI_nigeria_2020"), pattern = ".tif", full.names = TRUE)
files= sapply(files, raster, simplify = F)
EVI = files %>% map(~extract(., ib_wall, buffer = buffer, fun = mean, df =TRUE))
EVI_all = EVI  %>%  purrr::reduce(left_join, by =c("ID"))
EVI_all$EVI_mean = rowMeans(EVI_all[ , c(2:13)], na.rm=TRUE)
df_walld_EVI = cbind(df_ward_popden, EVI_all$EVI_mean)

ib_w_EVI = con_gplot(df_walld_EVI,quo(EVI_all.EVI_mean), quo(WardName))+
  scale_fill_continuous(name='Ward EVI, Ibadan', low="darkseagreen1", high="darkseagreen4", guide="colorbar",na.value="transparent")


#------------------------------------------------------------------------------
###Visualizing housing data 
#------------------------------------------------------------------------------
#housing 2015 
raster <- raster(file.path(Raster_Dir, "housing/2019_Nature_Africa_Housing_2015_NGA.tiff"))
val_housing <- raster::extract(raster,ib_wall, buffer = buffer, fun = mean, df =TRUE)
val_housing_15 = val_housing$X2019_Nature_Africa_Housing_2015_NGA
df_wall_house = cbind(df_walld_EVI, val_housing_15)

ib_w_house= con_gplot(df_wall_house,quo(val_housing_15), quo(WardName))+
  scale_fill_continuous(name='% of people with improved \n housing, Ibadan metro wards', low="azure", high="azure4", guide="colorbar",na.value="transparent")

ggsave(paste0(SampDir, '/results/', Sys.Date(), '_housing_quality_wards.pdf'),ib_w_house)

#morphological data - not using this since several wards have no data 
#raster <- raster(file.path(DataDir, "kano_ibadan_morphological_types_rasters", "ibadan_settlement_model.tif"))
#val_morph <- raster::extract(raster,ib_wall, buffer = buffer, fun = mean, df =TRUE)
#val_morph = val_morph$ibadan_settlement_model



#------------------------------------------------------------------------------
###Visualizing dump site data 
#------------------------------------------------------------------------------

# dump site
df_dump <- geojson_read(file.path(DataDir, 'oyo_dump_sites', "dump-sites.geojson"),  what = "sp")
dump_sf <- st_as_sf(df_dump)
st_crs(dump_sf)<- 4326

ib_w_dump=st_join(ib_wall, dump_sf, join =st_contains)
dump_n_ward = ib_w_dump %>% drop_na(name) %>% group_by(WardName) %>% summarise(num_dumpsites = n())
st_geometry(dump_n_ward) <- NULL

dump_dat = left_join(df_wall_house, dump_n_ward, by = c("WardName")) %>% 
  mutate_at(vars(num_dumpsites), ~replace(., is.na(.), 0))

ib_w_dumps= con_gplot(dump_dat,quo(num_dumpsites), quo(WardName))+
  scale_fill_continuous(name='Number of dumpsites,\n Ibadan metro wards', low="antiquewhite", high="antiquewhite3", guide="colorbar",na.value="transparent")

ggsave(paste0(SampDir, '/results/', Sys.Date(), '_dump_sites_wards.pdf'),ib_w_dumps) 



#------------------------------------------------------------------------------
###prepare dataset for model-based clustering 
#------------------------------------------------------------------------------

#prepare settlement data (convert settlement data to proportion)
settle_type_all = ib_w_block %>% 
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) %>% 
  dplyr::select(WardName, settle_type=type, LGACode)%>% group_by(WardName, settle_type)%>% summarise(number=n())
st_geometry(settle_type_all) <- NULL

settle_type_wide_all = settle_type_all %>%  pivot_wider(names_from = settle_type, values_from=number)   
settle_type_wide_all[is.na(settle_type_wide_all)]= 0
colnames(settle_type_wide_all) = paste('settlement', colnames(settle_type_wide_all), sep = '_')
colnames(settle_type_wide_all)[1] = 'WardName'
settle_type_wide_all$prop_A_settlement = settle_type_wide_all$settlement_A/rowSums(settle_type_wide_all[, c(2:6)]) 
settle_prop = settle_type_wide_all %>%  dplyr::select(WardName, prop_A_settlement)


#make dataset for population density, EVI, housing, dump site 
df_all_var = dump_dat %>%  dplyr::select(WardName, val_popndens, EVI_all.EVI_mean, val_housing_15, num_dumpsites)


#join A settlement proportion and k complexity data 
inform_k = left_join(settle_prop, ib_w_inform, by =c("WardName"))

#join all data 
df_all_var = left_join(df_all_var, inform_k, by = c("WardName")) 
all_ib_wards = df_all_var$WardName

df_all_var= df_all_var%>%  dplyr::select(-c(WardName))
st_geometry(df_all_var)  <- NULL

#------------------------------------------------------------------------------
###Variable Selection using clustvarsel packagae
#------------------------------------------------------------------------------
out<- clustvarsel(df_all_var)
summary(out$model) #suggests the presence of 4 clusters 

#getting clusters for mapping 
all_clusters=data.frame(out$model$classification) %>% rownames_to_column()
all_= df_all_var%>% rownames_to_column() 
ib_all_class = left_join(all_, all_clusters)
ib_all_class_1 = cbind(all_ib_wards, ib_all_class) %>%  dplyr::select(WardName = all_ib_wards,
                                                                      out.model.classification)
st_geometry(ib_all_class_1) <- NULL

#make a map of all clusters 
map_df_all = df_wall%>% left_join(ib_all_class_1)
ib_all_map_selected =con_gplot(map_df_all,quo(factor(out.model.classification)), quo(WardName))+
  scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1"))

ggsave(paste0(SampDir, '/results/', Sys.Date(), 'urban_clusters_var_selection_based.pdf'),ib_all_map_selected)

# ---------------------------------------------------------------
### Clustering using all the variables 
## --------------------------------------------------------------

#clustering for all wards
BIC <- mclustBIC(df_all_var)  
plot(BIC)
summary(BIC) #model with the lowest BIC is EII but clustering chooses wrong model

mod1 <- Mclust(df_all_var, x=BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

#getting cluster classes for mapping 
all_clusters=data.frame(mod1$classification) %>% rownames_to_column()
all_= df_all_var%>% rownames_to_column() 
ib_all_class = left_join(all_, all_clusters)
ib_all_class_1 = cbind(all_ib_wards, ib_all_class) %>%  dplyr::select(WardName = all_ib_wards,
                                                                      mod1.classification)
st_geometry(ib_all_class_1) <- NULL

#make a map of all clusters 
map_df_all = df_wall%>% left_join(ib_all_class_1)
ib_all_map_no_sel =con_gplot(map_df_all,quo(factor(mod1.classification)), quo(WardName))+
  scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "forestgreen"))

ggsave(paste0(SampDir, '/results/', Sys.Date(), 'urban_clusters_no_var_selection.pdf'),ib_all_map_no_sel)




# ---------------------------------
### Adding the DHS prevalence data 
## --------------------------------

sf18 = st_read(file.path(DHS_data, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  filter(ADM1NAME == 'OYO')

dhs = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>%
  filter(shstate == 'oyo', dhs_year == '2018') %>%  dplyr::select(v001, p_test, child_6_59_tested_malaria)

dhs_oyo = left_join(sf18, dhs, by=c('DHSCLUST' = 'v001')) %>%drop_na(p_test)


points = st_join(dhs_oyo, map_df_all_n, join = st_intersects)

all_wards = ggplot(map_df_all_n) + 
  geom_sf(color = 'lightgray') +
  geom_point(data = points, aes(fill=p_test,  geometry = geometry, size =child_6_59_tested_malaria),
             stat = "sf_coordinates", alpha = 0.45,  shape=21)+
  scale_fill_gradient(name = 'malaria prevalence', low = 'cornsilk', high = 'red')+
  geom_text_repel(
    data = map_df,
    aes(label = WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", 
    min.segment.length = 0, size = 5, force = 1)+
  map_theme()+
  xlab('')+
  ylab('')


###-----------------------------------------------------------------------------
#Reading Health Facility 
--------------------------------------------------------------------------------
  df_hf_ib_all <- geojson_read(file.path(DataDir, 'oyo_health_facilities', "health-care-facilities-primary-secondary-and-tertiary.geojson"),  what = "sp")
View(df_hf_ib)
df_sf_ib_all <- st_as_sf(df_hf_ib_all) 

view(df_sf_ib)

df_sf_ib_n_all = df_sf_ib_all %>% dplyr::select(c(id, latitude,longitude, category, global_id, name,ward_code, ward_name, lga_code, lga_name, geometry)) 

df_sf_ib_n0 = df_sf_ib_n %>% dplyr::filter(lga_name == 'Ibadan North')


filter(lga_name == 'Atiba')%>%
  filter(lga_name == 'Atisbo')%>%
  filter(lga_name == 'Ibarapa Central')%>%
  filter(lga_name == 'Ibarapa East')%>%
  filter(lga_name == 'Ibarapa North')%>%
  filter(lga_name == 'Irepo')%>%
  filter(lga_name == 'Iseyin')%>%
  filter(lga_name == 'Itesiwaju')%>%
  filter(lga_name == 'Iwajowa')%>%
  filter(lga_name == 'Kajola')%>%
  filter(lga_name == 'Ogbomosho North')%>%
  filter(lga_name == 'Ogbomosho SOuth')%>%
  filter(lga_name == 'Ogo Oluwa')%>%
  filter(lga_name == 'Olorunsogo')%>%
  filter(lga_name == 'Orelope')%>%
  filter(lga_name == 'Ori ire')%>%
  filter(lga_name == 'Oyo East')%>%
  filter(lga_name == 'Oyo West')%>%
  filter(lga_name == 'Saki East')%>%
  filter(lga_name == 'Saki West')%>%
  filter(lga_name == 'Surulere')%>%
  arrange(lga_name)


df_sf_ib_nwall = st_join(df_wall, df_sf_ib_n_all, join =st_intersects)

ib_hf=ggplot(df_sf_ib_n) +
  geom_sf(color=alpha('#8971B3', 0.5))+
  geom_text_repel(
    data = df_sf_ib_n,
    aes(label =  ward_name, geometry = geometry),color ='orchid',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  #labs(title= "HFs in Ibadan")+
  xlab("")+
  ylab("")

