
# # Reading in the necessary packages 
list_of_packages <- c("sf", "tidyverse", "ggplot2", "ggrepel", 
                      "patchwork", "geojsonio", "tmap", 
                      "biscale", "cowplot", "mclust",
                      "raster", "rgdal", "clustvarsel", "glm2", 
                      "tidyr", "dplyr", "leaflet", "viridis", 
                      "INLABMA", "INLA")


read_install_pacakges <- function(packages = list_of_packages
){
  
  # load and install packages 
  
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) install.packages(new_packages)
  
  return(sapply(list_of_packages, require, character.only = TRUE))
  
}

read_install_pacakges()


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

bar_fun = function(df, x, y, fill, scale_fill, size_x_text,
                   size_y_text, size_title_x, size_title_y, xlab){
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


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}



gmap_fun <- function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21
    ) +
    viridis::scale_fill_viridis(option='C', discrete=TRUE, labels=labels, 
                                na.value ='grey', 
                                limits=c('[0,0.2]', '(0.2,0.4]', 
                                         '(0.4,0.6]', '(0.6,0.8]', '(0.8,1]', NA)) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 5)))+
    xlab("")+
    ylab("")
}

