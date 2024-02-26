rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
NuCDir <- file.path(Drive, "my_stuff")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')

#=======
user <- Sys.getenv("USER")
if ("yousouphe" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  NuDPDir <- file.path(Drive, "Desktop")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "EA_data")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  PresDir <- file.path(NuDir,"presentations", "team member archive_Ifeoma", "2023", "230816_Kano_microstratification", "raw_pictures")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}

##----------------------------------------------------------------------------
#Libraries--------------------------------------------------------------------
##----------------------------------------------------------------------------

library(readxl)
library(sf)
library(vcd)
library(ggplot2)
library(tmap)
library(ggrepel)
library(tidyverse)
library(geometry)
library(dplyr)
library(rgdal)
library(fun)
library(patchwork)
library(rgeos)
library(maptools)
library(purrr)
library(DescTools)
library(conflicted)
library(stringr)


map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
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
      min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

##----------------------------------------------------------------------------
#CSV files--------------------------------------------------------------------
##----------------------------------------------------------------------------
files <- list.files(path = EADat , pattern = ".csv", full.names = TRUE, recursive = F)
dat <- sapply(files, read_csv, simplify = F)
names(dat)

## read Kano ward shape files
df_ko = st_read(file.path(shapepath, "Kano_metro_ward_sixLGAs", "Kano_metro_ward_sixLGAs.shp")) 

p <- ggplot(df_ko) +
  geom_sf(fill = "khaki") +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = dplyr::filter(cdc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 2.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano ")+
  coord_sf()

#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_kano2.png'), p, width = 8, height = 6)


##Mapping of EAs

#Spliting Kano Shapefile

df_kn_z <- df_ko %>%
  dplyr::filter(WardName == 'Zango')

df_kn_t <- df_ko %>%
  dplyr::filter(WardName == 'Tudun Wazurchi')

df_kn_f <- df_ko %>%
  dplyr::filter(WardName == 'Fagge D2')

df_kn_d <- df_ko %>%
  dplyr::filter(WardName == 'Dorayi')

df_kn_g <- df_ko %>%
  dplyr::filter(WardName == 'Gobirawa')

##Kano
EA_Kano <- dat[[4]]

##Zango
zn_hh <- EA_Kano %>% dplyr::filter(Ward=="Zango")

zn_df <- sf::st_as_sf(zn_hh, coords=c('Longitude', 'Latitude'), crs=4326)

# # Perform st transformation
st_crs(df_kn_z) <- 4326

st_crs(zn_df) <- 4326
 
# intersects_a <- st_intersection(agu_df, df_ib_a)

p <- ggplot(df_kn_z)+
  geom_sf(fill = NA) +
  geom_point(data = zn_df,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = zn_df,
    aes(label =  `EA_Code`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Zango Ward in Kano showing selected enumeration areas that fall within the ward")+
  coord_sf()

ggsave(paste0(PresDir,"/", Sys.Date(), '_Zango EAs overall.pdf'), p, width = 8, height = 6)
#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Zango EAs overall.pdf'), p, width = 8, height = 6)


##Tudun Wazurchi

# td_hh <- EA_Kano %>% dplyr::filter(Ward=="Tudun Wazurchi")
# 
# td_hh_df <- sf::st_as_sf(td_hh, coords=c('Longitude', 'Latitude'), crs=4326)
# 
# # Perform st transformation
# st_crs(df_kn_t) <- 4326
# 
# st_crs(td_hh_df) <- 4326
# 
# #intersects_b <- st_intersection(bas_hh_df, df_ib_b)
# 
# #intersect_sel <- st_intersection(agu_sel_df, df_ib_a)
# 
# p <- ggplot(df_kn_t) +
#   geom_sf(fill = NA) +
#   geom_point(data = td_hh_df,  aes(geometry = geometry, size = 2.0, col = Settlement), stat= "sf_coordinates")+
#   scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
#   geom_text_repel(
#     data = td_hh_df,
#     aes(label =  `EA Code`, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   guides(size = FALSE)+
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Tudun Wazurchi Ward in Kano showing EAs for listing")+
#   coord_sf()

#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Tudun Wazurchi EAs overall.pdf'), p, width = 8, height = 6)



##Faggae

fg_hh <- EA_Kano %>% dplyr::filter(Ward=="Faggae D2")

fg_hh_df <- sf::st_as_sf(fg_hh, coords=c('Longitude', 'Latitude'), crs=4326)

# Perform st transformation
st_crs(df_kn_f) <- 4326

st_crs(fg_hh_df) <- 4326

st_crs(df_ko) <- 4326

fg_hh_df <- st_intersection(fg_hh_df, df_kn_f)

p <- ggplot(df_kn_f) +
  geom_sf(fill = NA) +
  geom_point(data = fg_hh_df,  aes(geometry = geometry, size = 2.0,col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = fg_hh_df,
    aes(label =  `EA_Code`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Fagge Ward in Kano showing EAs for listing")+
  coord_sf()

ggsave(paste0(PresDir,"/", Sys.Date(), '_Faggae EAs overall.pdf'), p, width = 8, height = 6)
#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Faggae EAs overal.pdf'), p, width = 8, height = 6)


##Dorayi

dr_hh <- EA_Kano %>% dplyr::filter(Ward=="Dorayi")

dr_hh_df <- sf::st_as_sf(dr_hh, coords=c('Longitude', 'Latitude'), crs=4326)

# Perform st transformation
st_crs(df_kn_d) <- 4326

st_crs(dr_hh_df) <- 4326

st_crs(df_ko) <- 4326

dr_hh_df <- st_intersection(dr_hh_df, df_kn_d)

p <- ggplot(df_kn_d) +
  geom_sf(fill = NA) +
  geom_point(data = dr_hh_df,  aes(geometry = geometry, size = 2.0,col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = dr_hh_df,
    aes(label =  `EA_Code`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Dorayi Ward in Kano showing EAs for listing")+
  coord_sf()

ggsave(paste0(PresDir,"/", Sys.Date(), '_Dorayi EAs overall.pdf'), p, width = 8, height = 6)
ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Dorayi EAs overal.pdf'), p, width = 8, height = 6)


##Gobirawa

go_hh <- EA_Kano %>% dplyr::filter(Ward=="Gobirawa")

go_hh_df <- sf::st_as_sf(go_hh, coords=c('Longitude', 'Latitude'), crs=4326)

# Perform st transformation
st_crs(df_kn_g) <- 4326

st_crs(go_hh_df) <- 4326

st_crs(df_ko) <- 4326

go_hh_df <- st_intersection(go_hh_df, df_kn_g)

p <- ggplot(df_kn_g) +
  geom_sf(fill = NA) +
  geom_point(data = go_hh_df,  aes(geometry = geometry, size = 2.0,col = Settlement), stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
    data = go_hh_df,
    aes(label =  `EA_Code`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  guides(size = FALSE)+
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Gobirawa Ward in Kano showing EAs for listing")+
  coord_sf()

ggsave(paste0(PresDir,"/", Sys.Date(), '_Gobirawa EAs overall.pdf'), p, width = 8, height = 6)
ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Gobirawa EAs overal.pdf'), p, width = 8, height = 6)

