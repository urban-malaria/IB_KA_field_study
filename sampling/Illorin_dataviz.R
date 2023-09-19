rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths and functions 
## -----------------------------------------
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  ProjectDir <- file.path(NuDir, "data", 'nigeria')
  DHS_Dir <- file.path(ProjectDir, 'nigeria_dhs', 'data_analysis', 'data')
  Raster_Dir <- file.path(ProjectDir, 'Raster_files')
  DataDir <- file.path(ProjectDir, 'Ilorin')
  ResultDir <-file.path(NuDir, "projects", "Kwara-microstratification", "plots")
}

source("functions.R")
library(raster)
# ----------------------------------------------
### Visualizing urban ward and LGA boundaries  
## ---------------------------------------------

## read ward shape files
df = st_read(file.path(DataDir, "ward", "Ilorin_3LGA.shp")) 

p1=ggplot(df) +
  geom_sf(aes(fill = WardName))+
  geom_text_repel(
  data = df,
  aes(label =  WardName, geometry = geometry),color ='black',
  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")
ggsave(paste0(ResultDir, Sys.Date(), '_illorin_metro_colored_by_wards.pdf'), p1, width = 10, height =8)


#read and visualize LGA shape files
df_LGA = st_read(file.path(DataDir, "LGA", "illorin_metro_LGA.shp"))

p2=ggplot(df_LGA ) +
  geom_sf(aes(fill= LGAName))+
  geom_text_repel(
    data = df_LGA,
    aes(label =  LGAName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  labs(title= "LGAs in Illorin")+
  xlab("")+
  ylab("")



#make LGA and ward map
p3 <- ggplot(data =  df_LGA) +
  geom_sf(data=df, aes(fill = LGACode))+
  geom_text_repel(
    data = df,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("") +
  ylab("")
ggsave(paste0(ResultDir,"/", Sys.Date(), '_illorin_wards_colored_by_LGA.pdf'), p3, width = 10, height =8)

#urban/rural class
p4=ggplot(df) +
  geom_sf(aes(fill = Urban))+
  geom_text_repel(
    data = df,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_ResidenceType.pdf'), p4, width = 10, height =8)


# ---------------------------------------------------------------
### Visualizing settlements  
## --------------------------------------------------------------

#illorin
#settlement type overall 
df_settle = st_read(file.path(ProjectDir, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Kwara')
df_settle=st_join(df, df_settle, join =st_overlaps)
df_res = df_settle %>%   filter(landuse == 'Residential')
ana_df = df_res %>%  dplyr::select(WardName, settle_type=type, LGACode) %>%  group_by(WardName, settle_type)%>% summarise(number=n())

st_geometry(ana_df) <- NULL 

ana_df = ana_df %>%  pivot_wider(names_from = settle_type, values_from=number)   
ana_df[is.na(ana_df)]= 0
colnames(ana_df) = paste('settlement', colnames(ana_df), sep = '_')
colnames(ana_df)[1] = 'WardName'
ana_df$prop_poor_settlement = (ana_df$settlement_A + ana_df$settlement_B + ana_df$settlement_M)/rowSums(ana_df[, c(2:6)]) 
ana_df$prop_rich_settlement = (ana_df$settlement_D + ana_df$settlement_F)/rowSums(ana_df[, c(2:6)]) 
ana_df =ana_df %>%  dplyr::select(WardName, prop_poor_settlement, prop_rich_settlement)


#map 
pdat = left_join(df, ana_df)

p1=ggplot(pdat) +
  geom_sf(aes(fill = prop_rich_settlement))+
  scale_fill_gradient(low = "red", high = "#ebc084", na.value = NA) +
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_rich.pdf'), p1, width = 10, height =8)

#agricultural work 
df_agric = df_settle %>%   filter(landuse == 'Residential')


#----------------------------------------------------------

### Visualizing population density for all wards

##---------------------------------------------------------

raster <- raster("gpw_v4_population_density_rev11_2020_30_sec.tif")
pop_den <- raster::extract(raster, df, buffer = buffer, fun = mean, df =TRUE)
df_popd = cbind(df, pop_den)


p1=ggplot(df_popd) +
  geom_sf(aes(fill = gpw_v4_population_density_rev11_2020_30_sec))+
  scale_fill_gradient(low = "#eed6f1", high = "#ba5dc8", na.value = NA) +
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_density.pdf'), p1, width = 10, height =8)


#----------------------------------------------------------

### Visualizing distance to water bodies for all wards

##---------------------------------------------------------

raster <- raster("distance_to_water.tif")
dw <- raster::extract(raster, df, buffer = buffer, fun = mean, df =TRUE)
df_w= cbind(df, dw)


p1=ggplot(df_w) +
  geom_sf(aes(fill = distance_to_water))+
  scale_fill_gradient(low = "#29efde", high = "#36736d", na.value = NA) +
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_distance_water.pdf'), p1, width = 10, height =8)

