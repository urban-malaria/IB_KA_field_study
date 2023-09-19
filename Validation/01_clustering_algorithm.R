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
 CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')
# mod_file = file.path(ProjDir, "mathematical_model")

source("Validation/load_paths.R", echo= F)

# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("Validation/functions.R")
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
#view(multivariate_dataset_2018)

write.csv(multivariate_dataset_2018, file.path(DHS_CDir, "multivariatedataset_clustering_2018.csv"))

#load spatial points
sf18 = st_read(file.path(DHS_CDir, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  filter(URBAN_RURA == "U") %>% 
  inner_join(multivariate_dataset_2018, by = c("DHSCLUST" = "v001"))


##load shapefile of Nigeria with states
stateshp_dhs = readOGR(file.path(NuDir, "data", "nigeria", "shapefiles","gadm36_NGA_shp", "gadm36_NGA_1.shp"))


#Visualizing dhs cluster points
state_sf_dhs = st_as_sf(stateshp_dhs)

p1d <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = sf18, aes(color=DHSCLUST,  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.25, size=1.5, shape=21)+
  xlab("")+
  ylab("")+theme(legend.position = "none")


##----------------------------------------------------------------
#------------------Visualizing covariates-------------------------
##----------------------------------------------------------------


#Visualing Housing Quality
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
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Vegetation Index(EVI) of DHS clusters.pdf'), EVI_plot, width = 10, height =8)  


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
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Distance to water bodies of DHS clusters.pdf'), WaterB_plot, width = 10, height =8)  

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

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Travel Distance to Health Care by DHS clusters.pdf'), HealthC_plot, width = 10, height =8)  



#Combine all plots on same pdf

library(cowplot)
bottom_plots <- plot_grid (WaterB_plot, HealthC_plot, labels = c("Distance to water bodies", "Travel Distance to Health Care"))
middle_plots <- plot_grid(EVI_plot, labels = c("Vegetation Index"))
top_plots <- plot_grid(hous_plot, Pop_plot, labels = c("housing quality", "population density"))
All_clus_plots <- plot_grid(top_plots,bottom_plots, middle_plots, label_size = 12, ncol = 1)

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Variables for DHS clustering.pdf'), All_clus_plots, width = 10, height =8)  


#Getting data set for clustering##

#------------------------------------------------------------------------------
###prepare dataset for model-based clustering 
#------------------------------------------------------------------------------

#dhs
#make dataset for population density, EVI, housing, dump site, distance to water bodies 
all_dhs_var = sf18 %>%  
  dplyr::select(DHSCLUST, housing_q, motorized_travel_healthcare_2019_2000m,
                pop_density_0m, dist_water_bodies_0m,EVI_0m)%>%
  drop_na()

all_clusters = all_dhs_var$DHSCLUST

st_geometry(all_dhs_var) <- NULL

all_dhs_var = all_dhs_var %>%  dplyr::select(-DHSCLUST)
head(all_dhs_var)
mean(all_dhs_var$EVI_0m)

#------------------------------------------------------------------------------
###Variable Selection using clustvarsel packagae
#------------------------------------------------------------------------------

out <- clustvarsel(all_dhs_var)
summary(out$model) #VEV model suggests the presence of 7 clusters 

# BIC plot clustering for all wards with variable selection 

BIC <- mclustBIC(all_dhs_var, G=1:9)  

pdf(paste0(SampDir, '/new_results/', Sys.Date(), 'model_selection_BIC_var_selection.pdf'))

plot(BIC)

dev.off()

summary(BIC) 


# #getting clusters for mapping 

all_dhs_clusters=data.frame(out$model$classification) %>% rownames_to_column()
all_c= all_dhs_var%>% rownames_to_column() 
all_class_c = left_join(all_c, all_dhs_clusters)
all_dhs_class_1 = cbind(all_clusters, all_class_c) %>%  dplyr::select(DHSCLUST = all_clusters,
                                                                      covariate_clusters=out.model.classification)

# #make a map of all clusters 

map_df_dhs_all = sf18%>% left_join(all_dhs_class_1, by = c("DHSCLUST")) %>% 
  filter(!is.na(covariate_clusters))

save(map_df_dhs_all, file  = "Validation/clustering_covariates.RData")

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

ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'DHS Clusters.pdf'), 
       C_plot, width = 10, height =8)  


##End of Clustering##------------------------------------------------------------------------------------------  




##Adding Prevalence##

all_dhs_var_p = sf18 %>%  
  dplyr::select(DHSCLUST, housing_q, motorized_travel_healthcare_2019_2000m,
                pop_density_0m, dist_water_bodies_0m,EVI_0m, malaria_status = positives,
                Total_tested = num_child_6_59) %>%
  drop_na() %>% 
  mutate(prevalence = malaria_status/Total_tested)%>%
  drop_na()

all_clustersp = all_dhs_var_p$DHSCLUST

#------------------------------------------------------------------------------
###Variable Selection using clustvarsel packagae
#------------------------------------------------------------------------------
st_geometry(all_dhs_var_p) <- NULL

out<- clustvarsel(all_dhs_var_p)
summary(out$model) #VII model suggests the presence of 9 clusters 

--------------------------------------------------------------------------------

all_dhs_clusters_p=data.frame(out$model$classification) %>% rownames_to_column()
all_cp= all_dhs_var_p%>% rownames_to_column() 
all_class_cp = left_join(all_cp, all_dhs_clusters_p)
all_dhs_class_1p = cbind(all_clustersp, all_class_cp) %>%  dplyr::select(DHSCLUST = all_clustersp,
                                                                      covariate_clusters=out.model.classification)

# #make a map of all clusters 

map_df_dhs_allp = sf18%>% left_join(all_dhs_class_1p, by = c("DHSCLUST")) %>% 
  filter(!is.na(covariate_clusters))

save(map_df_dhs_all, file  = "Validation/clustering_covariates.RData")

C_pplot <- ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = map_df_dhs_allp,
             aes(fill = as.factor(covariate_clusters),  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.title = "Clusters", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(title= "Clustering classification of dhs data with prevalence")+
  labs(x = "", y  = "",  fill = "")


clust_dhs <- all_class_cp %>%
  group_by(out.model.classification) %>%
  summarize(MaxValuebycluster = max(prevalence, na.rm = T),
            Minvaluebycluster= min(prevalence, na.rm = T)) %>%
  arrange(out.model.classification)

clust_dhs_boxpl <- ggplot(data=all_class_cp, aes(x=as.factor(out.model.classification), 
                                                 y=prevalence, color= as.factor(out.model.classification))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Distribution of clustering classification of dhs data with prevalence")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")
