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
      min.segment.length = 0, size = 5, force = 1, max.overlaps = Inf)+
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
## read ward shape files, filter to urban areas only and show only wards  
ib_w = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Ibadan_metro_fiveLGAs", "Ibadan_metro_fiveLGAs.shp"))
  
table(ib_w$Urban)

ib_wmap=ggplot(ib_w) +
  geom_sf(color=alpha('#8971B3', 0.5))+
  geom_text_repel(
    data = ib_w,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  #labs(title= "Urban wards in Ibadan")+
  xlab("")+
  ylab("")

ggsave(paste0(SampDir, '/results/', Sys.Date(), '_urban_LGAs_wards_ibadan.pdf'), ib_wmap, width = 10, height =5)


#read and visualize LGA shape files
urban_LGA = c('Ibadan North', 'Ibadan North East', 'Ibadan North West', 'Ibadan South East', 'Ibadan South West')

ib_l = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Ibadan_metro_fiveLGAshapes", "Ibadan_metro_fiveLGAshapes.shp")) 

ib_lm=con_gplot(ib_l, "seashell", quo(LGAName))
  
#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_urban_LGAs_ibadan.png'), ib_lm, width = 10, height =8)

#do all wards fall into the LGAs?
ib_lwm=con_gplot(ib_l,"seashell", quo(LGAName))+
    geom_sf(data=ib_w, color=alpha('darkorchid', 0.5), fill = 'transparent') 

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_urban_LGAs_wards_ibadan.png'), ib_lwm, width = 10, height =5)

head(ib_l)
head(ib_w)

#join ward files to LGAs files to obtain data from LGA files and visualize 
df_wl=st_join(ib_w, ib_l, join =st_within)

ib_wm_cat=con_gplot(df_wl,"seashell", quo(LGAName))

 

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_urban_LGAs_wards_ibadan_category_urbanicity.png'),ib_wm_cat, width = 10, height =5)

# ---------------------------------------------------------------
### Visualizing population sizes 
## -------------------------------------------------------------

#read visualize Ward Population shape files
ib_wp = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp"))%>%
  filter(wrd_nm_x %in% ib_w$WardName)%>%
  filter(stat_cd=='OY')%>%
  filter(urban=='1',amap_cd!='NIE OYS GBH')%>%
  filter(urban=='1',amap_cd!='NIE OYS JND')%>%
  arrange(wrd_nm_x)

ib_wm = con_gplot(ib_wp,quo(mean), quo(wrd_nm_x))+
  scale_fill_continuous(name='Ward population \n count, Ibadan', low="thistle2", high="darkred", guide="colorbar",na.value="transparent")

ggsave(paste0(SampDir, '/results/', Sys.Date(), 'ibadan_metro_wards_population_size.pdf'),ib_wm, width = 4, height =6)

# ---------------------------------------------------------------
### Visualizing population density
## -------------------------------------------------------------

#read and visualize population density
raster <- raster(file.path(Raster_Dir, "NGA_pop_density", "gpw_v4_population_density_rev11_2020_1_deg.tif"))
val_popndens <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
val_popndens = val_popndens$gpw_v4_population_density_rev11_2020_1_deg

ib_w_popden=cbind(ib_w, val_popndens)

ib_popdens = con_gplot(ib_w_popden,quo(val_popndens), quo(WardName))+
  scale_fill_continuous(name='Ward population density \n count, Ibadan', low="thistle2", high="darkred", guide="colorbar",na.value="transparent")

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_urban_wards_populate_ibadan.png'), ib_wm, width = 12, height =8)

#let's get the ward grouping and add to the population data and visualize scale by group
# df_wg = df_wl %>% dplyr::select(wrd_nm_x=WardName, group, lga_cod=LGACode.x) 
# st_geometry(df_wg)<- NULL
# 
# ib_wp_ = left_join(ib_wp, df_wg) 
# ib_wp_urban = ib_wp_ %>%  filter(group == 'urban')
# ib_wp_semi_urban = ib_wp_ %>%  filter(group == 'semi-urban')
# 
# #population map for semi-urban areas 
# ib_w_semi = con_gplot(ib_wp_semi_urban,quo(mean), quo(wrd_nm_x))+
#   scale_fill_continuous(name='Semi-urban ward population \n count, Ibadan', low="antiquewhite", high="darkgoldenrod1", guide="colorbar",na.value="transparent")
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_strictly_semi-urban_wards_populate_ibadan.pdf'),ib_w_semi, width = 12, height =8)
# 
# #population map for urban areas 
# ib_w_urban = con_gplot(ib_wp_urban,quo(mean), quo(wrd_nm_x))+
#   scale_fill_continuous(name='Urban ward population \n count, Ibadan', low="cyan", high= "cyan4", guide="colorbar",na.value="transparent")
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_strictly_urban_wards_populate_ibadan.pdf'),ib_w_urban, width = 12, height =8)
# ####------------------------------------------------------------


# ---------------------------------------------------------------
### Visualizing settlements  
## --------------------------------------------------------------
#settlement type overall 
df = st_read(file.path(DataDir, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Oyo', landuse =='Residential')
st_crs(ib_w)<- 4326

ib_w_block=st_join(ib_w, df, join =st_overlaps)
ib_bar_dat = ib_w_block
st_geometry(ib_bar_dat) <- NULL

ib_bar_overall = ib_bar_dat %>% dplyr::select(type) %>%  group_by(type) %>%  summarise(number=n())
scale_fill = c("darkorchid1", "firebrick1", "forestgreen", "darksalmon", "burlywood4")
settle_type = bar_fun(ib_bar_overall, quo(type), quo(number), quo(type), scale_fill, 20, 20, 20, 20, 'Settlement type per neighborhood')

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_overall_bars.png'),settle_type, width = 6, height =3)

# #settlement types by urban vs semi-urban wards 
# st_crs(df_wl)<- 4326
# 
# ib_w_block=st_join(df_wl, df, join =st_overlaps)
# semiU_settle = ib_w_block %>% filter(group == 'semi-urban')
settle_grp_ward = ib_w_block %>% dplyr::select(WardName,type) %>%  group_by(WardName, type) %>%  summarise(number=n())
# 
# all wards
settle_type_ward = bar_fun(settle_grp_ward, quo(type), quo(number), quo(type), scale_fill, 12, 12, 12, 12, 'Settlement type per neighborhood')+
 facet_wrap(~WardName)

ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_wards_bars.pdf'),settle_type_ward, width = 8, height =9)

# 
# #urban
# 
# U_settle = ib_w_block %>% filter(group == 'urban')
# U_settle_ = U_settle %>% dplyr::select(WardName,type) %>%  group_by(WardName, type) %>%  summarise(number=n())
# 
# settle_type_w = bar_fun(U_settle_, quo(type), quo(number), quo(type), scale_fill, 12, 12, 12, 12, 'Settlement type per neighborhood')+
#   facet_wrap(~WardName)
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_urban_wards_bars.pdf'),settle_type_w)

# ---------------------------------------------------------------
### Visualizing informality 
## --------------------------------------------------------------
df_inform <- geojson_read(file.path(DataDir, 'kano_ibadan_informality', "kano_ibadan_settlements.geojson"),  what = "sp")

inform_sf <- st_as_sf(df_inform) %>%  filter(regions == 'Ibadan')

df_wl = df_wl %>%  dplyr::select(-c(LGACode.y, StateCode.x, Timestamp.x, AMAPCODE.x))

ib_w_inform=st_join(df_wl, inform_sf, join =st_overlaps)
inform  = ib_w_inform %>% mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode.x == '31007', 'Oranyan_7', WardName)) %>% 
  dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity))

View(ib_w_inform)

# all wards 
settle_type_w = ggplot() + 
  geom_sf(data =inform, mapping = aes(fill = mean_k)) +
  scale_fill_gradient(name = 'K complexity', low = 'mintcream', high = 'plum')+
  geom_text_repel(
    data = inform,
    aes(label = WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", 
    min.segment.length = 0, size = 5, force = 1)+
  map_theme()+
  xlab('')+
  ylab('')

#ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_all_wards.pdf'),settle_type_w) 

##Adding population to informality
st_geometry(inform)<- NULL
pop_inform = left_join(ib_wp, inform, by = c('wrd_nm_x' = 'WardName'))
dt = bi_class(pop_inform, x = mean, y = mean_k, style = "equal", dim = 3)
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

# #semi_urban
# semiU_inform = ib_w_inform %>% filter(group == 'semi-urban')
# semiU_inform  = semiU_inform %>% dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity))
# 
# settle_type_w = ggplot() + 
#   geom_sf(data =semiU_inform, mapping = aes(fill = mean_k)) +
#   scale_fill_gradient(name = 'K complexity', low = 'bisque1', high = 'red')+
#   geom_text_repel(
#     data = semiU_inform,
#     aes(label = WardName, geometry = geometry),color ='black',
#     stat = "sf_coordinates", 
#     min.segment.length = 0, size = 5, force = 1, max.overlaps = Inf)+
#   map_theme()+
#   xlab('')+
#   ylab('')
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_semi_urban.png'),settle_type_w)  
# 
# st_geometry(semiU_inform)<- NULL
# pop_inform = left_join(ib_wp_semi_urban, semiU_inform, by = c('wrd_nm_x' = 'WardName'))
# dt = bi_class(pop_inform, x = mean, y = mean_k, style = "equal", dim = 3)
# map <- ggplot() +
#   geom_sf(data = dt, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#   bi_scale_fill(pal = "DkBlue", dim = 3) +
#   geom_text_repel(
#     data = dt,
#     aes(label = wrd_nm_x, geometry = geometry),color ='black',
#     stat = "sf_coordinates", 
#     min.segment.length = 0, size = 5, force = 1, max.overlaps = Inf)+
#   xlab('')+
#   ylab('')+
#   bi_theme()
# legend <- bi_legend(pal = "DkBlue",
#                     dim = 3,
#                     xlab = "High population ",
#                     ylab = "High K complexity ",
#                     size = 7.5)
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0, .65, 0.2, 0.2)
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_population_semi_urban.pdf'),finalPlot)
# 
# #urban 
# 
# U_inform = ib_w_inform %>% filter(group == 'urban')
# U_inform  = U_inform %>% dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity, na.rm=T))
# 
# settle_type_w = ggplot() + 
#   geom_sf(data =U_inform, mapping = aes(fill = mean_k)) +
#   scale_fill_gradient(name = 'K complexity', low = 'cornsilk', high = 'seagreen')+
#   geom_text_repel(
#     data = U_inform,
#     aes(label = WardName, geometry = geometry),color ='black',
#     stat = "sf_coordinates", 
#     min.segment.length = 0, size = 5, force = 1)+
#   map_theme()+
#   xlab('')+
#   ylab('')
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_urban.pdf'),settle_type_w)  
# 
# st_geometry(U_inform) <- NULL
# pop_inform = left_join(ib_wp_urban, U_inform, by = c('wrd_nm_x' = 'WardName'))
# dt = bi_class(pop_inform, x = mean, y = mean_k, style = "equal", dim = 3)
# map <- ggplot() +
#   geom_sf(data = dt, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#   bi_scale_fill(pal = "DkBlue", dim = 3) +
#   geom_text_repel(
#     data = dt,
#     aes(label = wrd_nm_x, geometry = geometry),color ='black',
#     stat = "sf_coordinates", 
#     min.segment.length = 0, size = 5, force = 1, max.overlaps = Inf)+
#   xlab('')+
#   ylab('')+
#   bi_theme()
# legend <- bi_legend(pal = "DkBlue",
#                     dim = 3,
#                     xlab = "High population ",
#                     ylab = "High K complexity ",
#                     size = 7.5)
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0, .65, 0.2, 0.2)
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_population_urban.pdf'),finalPlot)


# ---------------------------------------------------------------
### Clustering 
## --------------------------------------------------------------

#prepare population data 
pop_df = ib_wp %>%  mutate(wrd_nm_x = ifelse(wrd_nm_x == 'Oranyan' & lga_cod == '31007', 'Oranyan_7', wrd_nm_x)) %>% 
  dplyr::select(WardName=wrd_nm_x, population_size=mean)
sf::st_geometry(pop_df) <- NULL

#prepare settlement data (convert settlement data to proportion)
settle_type = ib_w_block %>%  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) %>% 
  dplyr::select(WardName, settle_type=type, LGACode) %>%  group_by(WardName, settle_type) %>%  summarise(number=n())
sf::st_geometry(settle_type) <- NULL

settle_type_wide = settle_type %>%  pivot_wider(names_from = settle_type, values_from=number)   
settle_type_wide[is.na(settle_type_wide)]= 0
colnames(settle_type_wide) = paste('settlement', colnames(settle_type_wide), sep = '_')
colnames(settle_type_wide)[1] = 'WardName'

#settle_type_wide = settle_type_wide %>%  mutate(settle_num = settlement_A + settlement_B + settlement_D + settlement_F + settlement_M)
#settle_df = settle_type_wide %>%  mutate(across(c(1:5), .fns = ~./settle_num)) %>%  dplyr::select(WardName,settlement_A)

#join population, settlement and informality data 
df = left_join(pop_df, settle_type_wide)
colnames(inform)[2] = 'mean_k_complexity'
df = left_join(df, inform)

all_ib_wards = df$WardName

df= df%>%  dplyr::select(-c(WardName)) 

#clustering for all wards
BIC <- mclustBIC(df)
plot(BIC)
summary(BIC)

mod1 <- Mclust(df, x=BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

#getting cluster classes for mapping 
u_clusters=data.frame(mod1$classification) %>% rownames_to_column()
uib_= df%>% rownames_to_column() 
ib_class = left_join(uib_, u_clusters)
ib_class_1 = cbind(all_ib_wards, ib_class) %>%  dplyr::select(WardName = all_ib_wards,
                                                                      mod1.classification)

#make a map of all clusters 
map_df_u = df_wl%>% left_join(ib_class_1)
ib_map =con_gplot(map_df_u,quo(factor(mod1.classification)), quo(WardName))+
  scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "forestgreen", "darksalmon", "orchid4", "linen", "khaki4"))


# #split by group
# df_split = split(df, df$group)
# df_split[[1]]= df_split[[1]] 
# semi_urban_wards = df_split[[1]]$WardName
# df_split[[1]]= df_split[[1]]%>%  dplyr::select(-c(WardName, group)) 
# 
# #clustering for semi-urban  
# BIC <- mclustBIC(df_split[[1]])
# plot(BIC)
# summary(BIC)
# 
# mod1 <- Mclust(df_split[[1]], x=BIC)
# summary(mod1, parameters = TRUE)
# plot(mod1, what = "classification")
# 
# #getting cluster classes for mapping 
# semi_urban_clusters=data.frame(mod1$classification) %>% rownames_to_column()
# semi_= df_split[[1]]%>% rownames_to_column() 
# semi_urban_class = left_join(semi_, semi_urban_clusters)
# semi_urban_class = cbind(semi_urban_wards, semi_urban_class) %>%  dplyr::select(WardName = semi_urban_wards,
#                                                                                 mod1.classification)
# 
# #make a map of semi-urban clusters 
# 
# map_df_semi = df_wl %>%  filter(group == 'semi-urban') %>% left_join(semi_urban_class)
#   semi_map =con_gplot(map_df_semi,quo(factor(mod1.classification)), quo(WardName))+
#   scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "forestgreen", "darksalmon"))
# 
# 
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'semi_urban_clusters.png'),semi_map)
# 
# 
# 
# #clustering for urban 
# urban_wards = df_split[[2]]$WardName
# df_split[[2]]= df_split[[2]]%>%  dplyr::select(-c(WardName, group))
# 
# BIC <- mclustBIC(df_split[[2]])
# plot(BIC)
# summary(BIC)
# 
# mod1 <- Mclust(df_split[[2]], x=BIC)
# summary(mod1, parameters = TRUE)
# plot(mod1, what = "classification")
# 
# #getting cluster classes for mapping 
# urban_clusters=data.frame(mod1$classification) %>% rownames_to_column()
# urban_w= df_split[[2]]%>% rownames_to_column() 
# urban_class = left_join(urban_w, urban_clusters)
# urban_class = cbind(urban_wards, urban_class) %>%  dplyr::select(WardName = urban_wards,
#                                                                  mod1.classification)
# 
# #make a map of urban clusters 
# map_df = df_wl %>%  filter(group == 'urban') %>% left_join(urban_class)
# urban_map=con_gplot(map_df,quo(factor(mod1.classification)), quo(WardName))+
#   scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "forestgreen", "darksalmon"))
# 
# 
# #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'urban_clusters.pdf'),urban_map)
# 

# -------------------------------------------------------------------------------------------------
### Adding other covariates to the clustering
## ------------------------------------------------------------------------------------------------
# distance to water bodies 
raster <- raster(file.path(Raster_Dir, "distance_to_water_bodies/distance_to_water.tif"))
ib_w <- st_transform(x = ib_w, crs = st_crs(raster))
val <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
val_water = val$distance_to_water

#EVI (need to get updated from Dan Weiss)
raster <- raster(file.path(Raster_Dir, "EVI/EVI_v6.2018.12.mean.1km.tif"))
val_EVI <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
val_EVI=val_EVI$EVI_v6.2018.12.mean.1km

#housing 
raster <- raster(file.path(Raster_Dir, "housing/2019_Nature_Africa_Housing_2015_NGA.tiff"))
val_housing <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
val_housing_15 = val_housing$X2019_Nature_Africa_Housing_2015_NGA

# dump site
df_dump <- geojson_read(file.path(DataDir, 'oyo_dump_sites', "dump-sites.geojson"),  what = "sp")

dump_sf <- st_as_sf(df_dump)%>%
  st_transform(crs = 4326)

st_crs(dump_sf)<- 4326

######
df_walld = ib_wall%>%dplyr::select(-c(LGACode, StateCode, Timestamp, AMAPCODE))

ib_w_dump=st_join(dump_sf, ib_w, join =st_intersection)

%>% 
  dplyr::select(WardName, WardCode) %>%  group_by(WardName)


head(df)
# #population density
# raster <- raster(file.path(Raster_Dir, "NGA_pop_density", "gpw_v4_population_density_rev11_2020_1_deg.tif"))
# val_popndens <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
# val_popndens = val_popndens$gpw_v4_population_density_rev11_2020_1_deg

# str(val_popndens)
# #ggpl#ggpl#ggplot() +
#   
# geom_raster(data = val_popndens , aes(x = x, y = y,
#                                        fill = fct_elevation_2))
# + 
#   scale_fill_manual(values = , name = "Elevation") + 
#   coord_quickmap()#
#   
#   plot(ib_w, val_popndens %>% 
#        xlim = c(0, ncol(ib_w), ylim = c(0, nrow(x)),
#        xaxs = "i", yaxs = "i",
#        asp = 1, add = FALSE)
#   
# #housing 2010
# # raster <- raster(file.path(Raster_Dir, "housing/2019_Nature_Africa_Housing_2000_NGA.tiff"))
# # val_housing <- raster::extract(raster,ib_w, buffer = buffer, fun = mean, df =TRUE)
# # val_housing_00 = val_housing$X2019_Nature_Africa_Housing_2000_NGA

# -------------------------------------------------------------------------------------------------
### Reclustering urban wards again
## ------------------------------------------------------------------------------------------------
#cbind all the data 
df_all_ib = cbind(df, val_water, val_EVI, val_housing_15, val_popndens)
urban_wards = df$WardName


# df_all_ib =df_all_ib %>%  dplyr::select(-c(WardName, group)) %>%  dplyr::select(-c(starts_with("settlement")))

BIC <- mclustBIC(df_all_ib)
plot(BIC)
summary(BIC)

mod1 <- Mclust(df_all_ib, x=BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")

#getting cluster classes for mapping 
u_clusters=data.frame(mod1$classification) %>% rownames_to_column()
u_w= df_all_ib%>% rownames_to_column() 
u_class = left_join(u_w, u_clusters)
u_class_1 = cbind(all_ib_wards, u_class) %>%  dplyr::select(WardName = all_ib_wards,
                                                                 mod1.classification)

#make a map of urban clusters 
map_df = df_wl %>% left_join(u_class_1)
ib_nw_map =con_gplot(map_df,quo(factor(mod1.classification)), quo(WardName))+
  scale_fill_manual(name='Cluster Number',
                    values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "forestgreen", "darksalmon",
                               "cyan", "darkseagreen1", "dodgerblue1"))


ggsave(paste0(SampDir, '/results/', Sys.Date(), 'urban_clusters.png'),urban_map)

#------------------------------------------------------------------------------
###Variable Selection using clustvarsel packagae
#------------------------------------------------------------------------------

clustvarsel(df_all_ib,
            G = 1:9,
            search = c("greedy"),
            direction = c("backward"),
            emModels1 = c("V"),
            emModels2 = mclust.options("emModelNames"),
            samp = FALSE,
            sampsize = round(nrow(data)/2),
            hcModel = "VVV",
            allow.EEE = TRUE,
            forcetwo = TRUE,
            BIC.diff = 0,
            BIC.upper = 0,
            BIC.lower = -10,
            itermax = 100,
            parallel = FALSE,
            fit = TRUE,
            verbose = interactive())

# #------------------------------------------------------------------------------
# #Exploring K:-clustering Elbow-plot
# ##-----------------------------------------------------------------
# 
# library(tidyverse)  # data manipulation
# library(cluster)    # clustering algorithms
# library(factoextra) # clustering algorithms & visualization
# 
# #Visualixzing popu;ation density
# ib_pop_ = left_join(ib_wp, val_popndens, by = wrd_nm_x) 
# ib_wp_urban = ib_wp_ %>%  filter(group == 'urban')
# ib_wp_semi_urban = ib_wp_ %>%  filter(group == 'semi-urban')
# 
# #population map for semi-urban areas 
# ib_w_semi = con_gplot(ib_wp_semi_urban,quo(mean), quo(wrd_nm_x))+
#   scale_fill_continuous(name='Semi-urban ward population \n count, Ibadan', low="antiquewhite", high="darkgoldenrod1", guide="colorbar",na.value="transparent")



# -------------------------------------------------------------------------------------------------
### Adding the DHS data 
## ------------------------------------------------------------------------------------------------
# ---------------------------------
### Adding the DHS prevalence data 
## --------------------------------

sf18 = st_read(file.path(DHS_data, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% filter(ADM1NAME == "OYO")

View(sf18)

dhs = read.csv(file.path(CsvDir, "all_DHS_variables_urban_malaria.csv"), header = T, sep = ',') %>%
  filter(shstate == 'oyo', dhs_year == '2018') %>%  dplyr::select(v001, p_test, child_6_59_tested_malaria)
View(dhs)
dhs_oyo = left_join(sf18, dhs, by=c('DHSCLUST' = 'v001')) %>%drop_na(p_test)

view(dhs_oyo)


#urban wards
points = st_join(dhs_oyo, map_df, join = st_intersects) %>% drop_na(group)
malaria_urban_wards = ggplot(map_df) + 
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
ggsave(paste0(SampDir, '/results/', Sys.Date(), 'urban_clusters_malaria_prevalence.png'),malaria_urban_wards)


#semi-urban wards
points = st_join(dhs_oyo, map_df_semi, join = st_intersects) %>% drop_na(group)
malaria_semi_wards = ggplot(map_df_semi) + 
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
ggsave(paste0(SampDir, '/results/', Sys.Date(), 'semi_urban_clusters_malaria_prevalence.png'),malaria_semi_wards)


# --------------------------------------
###Visualizing health facility locations
## -------------------------------------

case_df = read.csv(file.path(DataDir, 'nigeria_hmis', 'nigerian_hmis_totals.csv')) 
case_df  = case_df %>%  filter(str_detect(ward, "^oy"))
case_df$state[case_df$state==""] <- NA
case_df$lga[case_df$lga==""] <- NA
case_df$ward[case_df$ward==""] <- NA
case_df = case_df %>%  fill(state, lga, ward)


df = case_df %>%  filter(grepl('oy', state))
df = df %>%  filter(grepl('Ward Total', ward))


df = df %>% mutate(ward_name_1 = str_split(ward, " ", simplify = T)[, 2], ward_name_2 = str_split(ward, " ", simplify = T)[, 3] )
df = df %>%  mutate(ward_name_2_clean = ifelse(ward_name_2 == 'Ward', '', ward_name_2))
df = df %>%  mutate(ward_name = trimws(paste0(ward_name_1, ' ', ward_name_2_clean)))



#urban wards 
df_urban = df  %>%  filter(ward_name %in% map_df$WardName) %>% 
  filter(lga != "oy Atiba Local Government Area", lga !="oy Saki West Local Government Area")
write.csv(df_urban, file=paste0(SampDir, '/results/', Sys.Date(), 'cases_urban_wards.csv'))


df$tpr = (df$Total.Sum.of.tpos_u6  + df$Total.Sum.of.tpos_a6)/(df$Total.Sum.of.tfever_u6 + df$Total.Sum.of.tfever_a6)
df_ = df %>%dplyr::select(lga, ward_name, tpr))
plot_df = left_join(map_df, df_urban, by=c("WardName" = "ward_name"))

