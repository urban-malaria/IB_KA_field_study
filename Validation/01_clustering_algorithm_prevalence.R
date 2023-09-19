rm(list=ls())
## -----------------------------------------
### Paths
## -----------------------------------------
source("Validation/load_paths.R", echo= F)


# -----------------------------------------
## Required functions and settings
## -----------------------------------------

source("Validation/functions.R")

# Reading in dataset
multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")

multivariate_dataset_2018 <- multivariate_dataset %>% 
  filter(dhs_year == 2018)

#load spatial points
sf18 = st_read(file.path(DHS_CDir, "Downloads",
                         "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  filter(URBAN_RURA == "U") %>% 
  inner_join(multivariate_dataset_2018, 
             by = c("DHSCLUST" = "v001"))



# Visualizing Population Density
# Getting data set for clustering##

#------------------------------------------------------------------------------
# prepare dataset for model-based clustering 
#------------------------------------------------------------------------------

# dhs
# make dataset with prevalence 
all_dhs_var = sf18 %>% 
  filter(LATNUM != 0.0000 & LONGNUM != 0.0000) %>% 
  dplyr::select(DHSCLUST, malaria_status = positives,
                Total_tested = num_child_6_59) %>%
  drop_na() %>% 
  mutate(prevalence = malaria_status/Total_tested) %>% 
  dplyr::select(DHSCLUST, prevalence)


all_clusters = all_dhs_var$DHSCLUST

st_geometry(all_dhs_var) <- NULL

all_dhs_var<- all_dhs_var %>% dplyr::select(-DHSCLUST)


#------------------------------------------------------------------------------
###clustering with mclust
#------------------------------------------------------------------------------

out<- Mclust(all_dhs_var)
summary(out) #E model with 8 components 


# #getting clusters for mapping 

all_dhs_clusters = data.frame(DHSCLUST=all_clusters, prev_cluster=out$classification) #%>% 
  #rownames_to_column()

all_c = data.frame(DHSCLUST=all_clusters, prevalence=all_dhs_var$prevalence) %>% left_join(all_dhs_clusters)

save(all_c, file = "Validation/dhs_prevalence_clustering.RData")

# all_c= all_dhs_var%>%
#   rownames_to_column() %>%
#   left_join(all_dhs_clusters)
# 
# all_dhs_class_1 = cbind(all_clusters, all_c) %>%  
#   dplyr::select(DHSCLUT = all_clusters, out.model.classification)



map_df_dhs_prev= sf18 %>%
  left_join(all_c, by = c("DHSCLUST")) %>% 
  #left_join(all_dhs_var) %>% 
  filter(!is.na(prev_cluster)) %>% 
  mutate(cluster_cat = cut(prevalence, seq(min(prevalence), 
                                           max(prevalence)+ 0.001,
                                           length.out = 9),
                           right = FALSE))  
  
  
map_df_dhs_prevcount <- map_df_dhs_prev %>%
  group_by(cluster_cat, prev_cluster) %>% 
  summarise(number  = n()) 

map_df_dhs_prevcat_cluster <- map_df_dhs_prev %>% 
  ungroup() %>% 
  group_by(out.model.classification) %>% 
  mutate(lower_bound = round(min(prevalence), 2),
         upper_bound = round(max(prevalence), 2), 
         cluster_prev = paste0(out.model.classification,": (",
                               lower_bound, "-", upper_bound, ")"))


save(map_df_dhs_prevcount,map_df_dhs_prevcat_cluster, file  = "Validation/clustering_prevalence.RData")

# make a map of all clusters 

 ggplot(state_sf_dhs) +
  geom_sf(color='lightgrey')+
  geom_point(data = map_df_dhs_prevcat_cluster,
             aes(fill = as.factor(cluster_prev),  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")  


 
 ggplot(map_df_dhs_prev, aes(fill= cluster_cat, 
                            y=number, 
                            x=out.model.classification)) + 
   geom_bar(position='stack', stat='identity')+ 
   scale_x_continuous(breaks = seq(1,7,1) )+
   theme(legend.position = "right", legend.background = element_blank(),
         panel.grid.major = element_blank()) +
   labs(x = "clustering classification", fill = "prevalence classes",
       y = "Frequency")
   

 
 
