#EA mapping for Ilorin

rm(list=ls())

## -----------------------------------------
### Paths
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
  DataDir <- file.path(NuDir,"data", "nigeria", "ilorin")
  ShapePath <- file.path(DataDir, "Ilorin_shape_files", "ward")
  EAPath <- file.path(DataDir, "Ilorin_EA_maps")
  PrintPath <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "2023", "230920_Ilorin_field_findings")
}

## -----------------------------------------
### functions
## -----------------------------------------
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


library(sf)
library(tidyverse)
library(ggplot2)
library(ggrepel)

## --------------------------------------------------------
### Read in shape file for Are2 and Akanbi 4 and EA data
## ---------------------------------------------------------

files <- list.files(path = ShapePath , pattern = "Are_2.shp|Akanbi_4.shp", full.names = TRUE, recursive = F)
dat <- sapply(files, st_read, simplify = F)


EA_dat <- read_csv(file.path(EAPath, "EA_plot_dat.csv"))

#Are 2  
Are <- EA_dat %>%  dplyr::filter(`NAME OF WARD` == "Are 2") %>%  st_as_sf(coords =c("_COMMUNITY COORDINATES_longitude", "_COMMUNITY COORDINATES_latitude"), crs=4326)


saidu = Are %>%  filter() 

#plot
p1<- ggplot(dat[[2]])+
  geom_sf(fill = NA) +
  geom_point(data = Are,  aes(geometry = geometry, size = 2.0, col = Impression), alpha = 0.7, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  guides(size = "none")+
  geom_text_repel(
    data = Are,
    aes(label =  `NAME OF COMMUNITY/SETTLEMENT`, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme()+
  theme(legend.title = element_blank())+
  ylab("")+
  xlab("")+
  labs(title= "Communities in Are 2 classified by settlement type")+
  coord_sf()
ggsave(paste0(PrintPath,"/", Sys.Date(), '_Are_2_classification_communities.pdf'), p1, width = 8, height = 6)

#Akanbi 4
Ak <- EA_dat %>%  dplyr::filter(`NAME OF WARD` == "Akanbi 4") %>%  st_as_sf(coords =c("_COMMUNITY COORDINATES_longitude", "_COMMUNITY COORDINATES_latitude"), crs=4326)


#plot
p2<- ggplot(dat[[1]])+
  geom_sf(fill = NA) +
  geom_point(data = Ak,  aes(geometry = geometry, size = 2.0, col = Impression), alpha = 0.7, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  guides(size = "none")+
  map_theme()+ 
  theme(legend.title = element_blank())+
  ylab("")+
  xlab("")+
  labs(title= "Communities in Akanbi 4 classified by settlement type")+
  coord_sf()
ggsave(paste0(PrintPath,"/", Sys.Date(), '_Akanbi_4_classification_communities.pdf'), p2, width = 8, height = 6)
