rm(list=ls())
## -----------------------------------------
### Paths
## -----------------------------------------
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
NuDir <- file.path(Drive, "urban_malaria")
DataDir <-file.path(NuDir, "data", "nigeria")
ProjDir <- file.path(NuDir, "projects")
SampDir <- file.path(ProjDir, "sampling")
DHS_CDir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data', 'DHS')
Raster_Dir <- file.path(NuDir,'data', 'Raster_files')
DHS_data <- file.path(DHS_Dir, 'DHS')
#CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')
mod_file = file.path(ProjDir, "mathematical_model")


# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("functions.R")
library(ggridges)

gmap_fun <- function(polygon_name, point_data, labels, fill, legend_title){
  ggplot(polygon_name) +
    geom_sf(color='lightgrey')+
    geom_point(data = point_data,
               aes(fill=fill,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21
    ) +
    viridis::scale_fill_viridis(option='C', discrete=TRUE, labels=labels, na.value ='grey', limits=c('[0,0.2]', '(0.2,0.4]', '(0.4,0.6]', '(0.6,0.8]', '(0.8,1]', NA)) +
    map_theme() + 
    guides(fill = guide_legend(title=legend_title, override.aes = list(size = 5)))+
    xlab("")+
    ylab("")
}
##Reading in dataset
multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")

multivariate_dataset_2018 <- multivariate_dataset %>% 
  filter(dhs_year == 2018)
view(multivariate_dataset_2018)

write.csv(multivariate_dataset_2018, file.path(DHS_CDir, "multivariatedataset_clustering_2018.csv"))

#load spatial points
sf18 = st_read(file.path(DHS_CDir, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  filter(URBAN_RURA == "U") %>% 
  inner_join(multivariate_dataset_2018, by = c("DHSCLUST" = "v001"))

view(sf18)
# sf18_summary <- sf18 %>%
#    summarize(total = n(),
#              urban = sum(ifelse(URBAN_RURA == "U", 1, 0)),
#              rural =total -  urban)

#st_geometry(sf18) <- NULL

##load shapefile of Nigeria with states
stateshp_dhs = readOGR(file.path(NuDir, "data", "shapefiles","gadm36_NGA_shp", "gadm36_NGA_1.shp")) 

#, layer ="gadm36_NGA_1",use_iconv=TRUE, encoding= "UTF-8")

#Visualizing cluster points
state_sf_dhs = st_as_sf(stateshp_dhs)

p1d <- ggplot(state_sf_dhs) +
     geom_sf(color='lightgrey')+
     geom_point(data = sf18, aes(color=DHSCLUST,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.25, size=1.5, shape=21)+
             xlab("")+
             ylab("")+theme(legend.position = "none")

# p2d= ggplot(state_sf_dhs) +
#   geom_sf(color='lightgrey')+
#   geom_point(data = sf18, aes(color=housing_q,  geometry = geometry),
#              stat = "sf_coordinates", alpha = 0.25, size=1.5, shape=21)+
#   xlab("")+
#   ylab("")+theme(legend.position = "none")


#Visualinz Housing Quality
sf18_mod <- sf18 %>% 
    mutate(quality = cut(housing_q,seq(min(housing_q), max(housing_q), 25), right = T)) %>% 
  filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
  drop_na(quality)

hous_plot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = sf18_mod,
             aes(fill = quality,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Housing Quality of dhs clusters.pdf'), hous_plot, width = 10, height =8)  
  
  
#Visualizing Population Density
sf18_pop <- sf18 %>% 
    mutate(quality_pop = cut(pop_density_0m, quantile(pop_density_0m, na.rm = TRUE), right = TRUE)) %>% 
    filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
    drop_na(quality_pop)

Pop_plot <- ggplot(state_sf_dhs) +
    geom_sf(color='lightgrey')+
    geom_point(data = sf18_pop,
               aes(fill = quality_pop,  geometry = geometry),
               stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
    theme(legend.position = "right", legend.background = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    labs(x = "", y  = "",  fill = "")
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Population Density of DHS clusters.pdf'), Pop_plot, width = 10, height =8)  

##Visualizing EVI
sf18_evi <- sf18 %>% 
  mutate(quality_evi = cut(EVI_0m, quantile(EVI_0m, na.rm = TRUE), right = TRUE)) %>% 
  filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
  drop_na(quality_evi)

EVI_plot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = sf18_evi,
             aes(fill = quality_evi,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Vegetation Index(EVI) of DHS clusters.pdf'), Pop_plot, width = 10, height =8)  

##Visualizing Distance to Water Bodies
sf18_water <- sf18 %>% 
  mutate(quality_water = cut(dist_water_bodies_0m, na.rm = TRUE, quantile(dist_water_bodies_0m, na.rm = TRUE), right = TRUE)) %>% 
  filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
  drop_na(quality_water)

WaterB_plot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = sf18_water,
             aes(fill = quality_water,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.background = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")  
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Distance to water bodies of DHS clusters.pdf'), Pop_plot, width = 10, height =8)  

##Visualizing Travel to healthcare
sf18_health <- sf18 %>% 
  mutate(quality_health = cut(motorized_travel_healthcare_2019_2000m, na.rm = TRUE, 
                              quantile(motorized_travel_healthcare_2019_2000m, na.rm = TRUE), 
                              right = TRUE)) %>% 
  filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
  drop_na(quality_health)

HealthC_plot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = sf18_health,
             aes(fill = quality_health,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")  
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Travel Distance to Health Care by DHS clusters.pdf'), Pop_plot, width = 10, height =8)  



#Combine all plots on same pdf
library(cowplot)
bottom_plots <- plot_grid (WaterB_plot, HealthC_plot, labels = c("Distance to water bodies", "Travel Distance to Health Care"))
middle_plots <- plot_grid(EVI_plot, labels = c("Vegetation Index"))
top_plots <- plot_grid(hous_plot, Pop_plot, labels = c("housing qaulity", "population density"))
All_clus_plots <- plot_grid(top_plots,bottom_plots, middle_plots, label_size = 12, ncol = 1)

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Variables for DHS clustering.pdf'), All_clus_plots, width = 10, height =8)  

# p1d=ggplot(state_sf_dhs) +
#   geom_sf(fill='sienna1')+
#   geom_text_repel(
#     data = sf18,
#     aes(label =  ADM1NAME, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
#   map_theme() + 
#   #labs(title= "Wards in Ibadan")+
#   xlab("")+
#   ylab("")
# ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'wards_ibadan_metro.pdf'), p1, width = 10, height =8)

#Getting data set for clustering...##

#------------------------------------------------------------------------------
###prepare dataset for model-based clustering 
#------------------------------------------------------------------------------

#ibadan 
#make dataset for population density, EVI, housing, dump site 
all_dhs_var = sf18 %>%  
  dplyr::select(DHSCLUST, housing_q, motorized_travel_healthcare_2019_2000m,
                pop_density_0m, dist_water_bodies_0m,EVI_0m)%>%
  drop_na()

#join A settlement proportion and k complexity data 
#inform_k = left_join(settle_prop, w_inform, by =c("WardName"))

# dfnn <- data.frame (df_all_var)
# str(dfnn)
# #join all data 
# df_all_var= left_join(dfnn, inform_k, by = c("WardName")) 
# str(df_all_var_c)
all_clusters = all_dhs_var$DHSCLUST
# 
# df_all_var_c= df_all_var %>% dplyr::select(-c(WardName, geometry.x, geometry.y))

st_geometry(all_dhs_var) <- NULL

# write.csv(df_all_var_c, file.path(mod_file, "Ibadan_variables.cluster.csv"), row.names = FALSE)
# df_all_var_cn = read.csv (file.path(mod_file, "Ibadan_variables.cluster.csv")) %>% dplyr::select(pop_den, EVI_all.EVI_mean, 
#                                                                                                  val_housing_15, num_dumpsites, mean_k, prop_A_settlement)
 
view(all_dhs_var)
#------------------------------------------------------------------------------
###Variable Selection using clustvarsel packagae
#------------------------------------------------------------------------------

out<- clustvarsel(all_dhs_var)
summary(out$model) #VEV modelsuggests the presence of 7 clusters 

# #getting clusters for mapping 
all_dhs_clusters=data.frame(out$model$classification) %>% rownames_to_column()
all_c= all_dhs_var%>% rownames_to_column() 
all_class_c = left_join(all_c, all_dhs_clusters)
all_dhs_class_1 = cbind(all_clusters, all_class_c) %>%  dplyr::select(DHSCLUT = all_clusters,
                                                              out.model.classification)

# #adding archetype wards 
# ward_arch = all_class_1 %>%  mutate(Archetype = case_when(
#   out.model.classification == 1 ~ "Olopomewa",
#   out.model.classification == 2 ~ "Challenge",
#   out.model.classification == 3 ~ "Agugu",
#   TRUE ~ "Asanke")) 
# 
# pop_ibadan =read.csv(file.path(mod_file, "Ibadan_ward_pop_1.csv")) %>%  dplyr::select(-X, LGA=WardName) %>%  mutate(State = 'Oyo')
# sim_dat = left_join(pop_ibadan, ward_arch)
# write.csv(sim_dat, file.path(mod_file, "Ibadan_ward_pop.csv"), row.names = FALSE)
# 
# #make a map of all clusters 
 map_df_dhs_all = sf18%>% left_join(all_dhs_class_1, by = c("DHSCLUST" = "DHSCLUT")) %>% 
                filter(!is.na(out.model.classification))
 
 C_plot <- ggplot(state_sf_dhs) +
   geom_sf(color='lightgrey')+
   geom_point(data = map_df_dhs_all,
              aes(fill = as.factor(out.model.classification),  geometry = geometry),
              stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
   theme(legend.position = "right", legend.background = element_blank()) + 
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_blank(), 
         axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
         axis.text.y=element_blank(), axis.ticks.y=element_blank())+
   labs(x = "", y  = "",  fill = "")  
 
 ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'DHS Clusters.pdf'), C_plot, width = 10, height =8)  
 
 
# all_map_dhs_selected =con_gplot(map_df_dhs_all,quo(factor(out.model.classification)), quo(DHSCLUST))+
#scale_fill_manual(name='Cluster Number', values = c("seashell", "lightpink1", "darkorchid1", "firebrick1", "pink", "ivory", "orchid"))
# 
# df_bic = data.frame(dumpsites = df_all_var$num_dumpsites, housing = df_all_var$val_housing_15)

# BIC plot clustering for all wards with variable selection 
BIC <- mclustBIC(all_dhs_var, G=1:9)  
pdf(paste0(SampDir, '/newresults/', Sys.Date(), 'model_selection_BIC_var_selection.pdf'))
plot(BIC)
dev.off()
summary(BIC) #model with the lowest BIC is EII but clustering chooses wrong model


##End of Clustering##------------------------------------------------------------------------------------------  

##Adding prevalence data to clustering data.

all_dhs_mprev = sf18 %>%
  ungroup() %>%
  dplyr :: transmute(DHSCLUST, positives_prop) %>%
  drop_na(positives_prop)

all_dhs_prev_class_1 = all_dhs_mprev%>% left_join(all_dhs_class_1, by = c("DHSCLUST" = "DHSCLUT"))%>%
  filter(!is.na(out.model.classification))
##make a map of all clusters with prevalence 
##map_df_dhs_all = sf18%>% left_join(all_dhs_class_1, by = c("DHSCLUST" = "DHSCLUT")) %>% 
  filter(!is.na(out.model.classification))

Prev_plot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = all_dhs_prev_class_1,
             aes(fill = as.factor(positives_prop), geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
     theme(legend.position = "right", legend.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")  
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'DHS Malaria Prevalence in Clusters.pdf'), Prev_plot, width = 10, height =8)  


#Combine DHS plots on same pdf
DHS_plots <- plot_grid (C_plot, Prev_plot, 
                        labels = c("DHS Clustering Using Malaria Variable", 
                                   "Proportion of Malaria Positives in Clusters"), 
                        label_size = 12, ncol = 1)

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'DHS Malaria Prevalence and Clusters Classification.pdf'), DHS_plots, width = 10, height =8)  
