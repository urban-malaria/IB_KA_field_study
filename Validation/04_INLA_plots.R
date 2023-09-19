rm(list = ls())

library(RColorBrewer);library(ggplot2)

# data used in this script saved as Rdata and
# should run independent of the others scripts 
# the code produces the stacked bar graphs

load("Validation/dhs_data_files.RData")
load("Validation/predicted_values.RData")
load("Validation/clustering_covariates.RData")


spatial_data$mean<-p.prd
spatial_data$lower_bound = p.prd_lb  
spatial_data$upper_bound = p.prd_ub
  
min_max <- range(spatial_data$mean)
min_max_lb <- range(spatial_data$lower_bound)
min_max_ub <- range(spatial_data$upper_bound)

new_data <- spatial_data %>% 
  # divided the columns mean, lower_bound and 
  # upper_bound into the respective classes 
  # produced by the clustering algorithm 
  # (produces 7 groups)
  mutate(cluster_cat = cut(mean, seq(min_max[1], 
                                     min_max[2]+ 0.001,
                                     length.out = 8),
                           right = FALSE), 
         cluster_cat_lb = cut(lower_bound, seq(min_max_lb[1], 
                                              min_max_lb[2]+ 0.001,
                                     length.out = 8),
                              right = FALSE),
         cluster_cat_ub = cut(upper_bound, seq(min_max_ub[1], 
                                           min_max_ub[2]+ 0.001,
                                           length.out = 8),
                              right = FALSE)) 
  
# Decide on the color sche 
myColors <- brewer.pal(7, "Accent")
cat_num = 1:7

# create labels for the maps 
category = sort(unique(new_data$cluster_cat))
category_lb = sort(unique(new_data$cluster_cat_lb))
category_ub = sort(unique(new_data$cluster_cat_ub))

cat_labels = paste(cat_num,":", category)
cat_labels_lb = paste(cat_num,":", category_lb)
cat_labels_ub = paste(cat_num,":", category_ub)



ggplot(state_sf_dhs) +
  # lower bound map for Inla CI
  geom_sf(color='white')+
  geom_point(data = new_data,
             aes(fill = as.factor(cluster_cat_lb),  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
  scale_fill_manual(labels = cat_labels_lb, values = myColors)+
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")



 ggplot(state_sf_dhs) +
   # mean bound map for Inla CI
  geom_sf(color='white')+
  geom_point(data = new_data,
             aes(fill = as.factor(cluster_cat),  geometry = geometry),
             stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
   scale_fill_manual(labels = cat_labels, values = myColors)+
  theme(legend.position = "right", legend.background = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y  = "",  fill = "")
 
 
 ggplot(state_sf_dhs) +
   # Upper bound map for Inla CI
   geom_sf(color='white')+
   geom_point(data = new_data,
              aes(fill = as.factor(cluster_cat_ub),  geometry = geometry),
              stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
   scale_fill_manual(labels = cat_labels_ub, values = myColors)+
   theme(legend.position = "right", legend.background = element_blank()) + 
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_blank(), 
         axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
         axis.text.y=element_blank(), axis.ticks.y=element_blank())+
   labs(x = "", y  = "",  fill = "")
 
 
 
###################################################
# Data reshaping for plotting the stacked bar 
###################################################
 
 new_data_dhs <- new_data %>%
   # calculate prevalence and cut into 7 groups 
   mutate(prevalence = malaria_status/Total_tested,
          cluster_cat = cut(prevalence, seq(min(prevalence),
                                      max(prevalence),
                            length.out = 8),
                            # labels = c(1,2,3,4,5,6,7), 
                            include.lowest = T,
                            right = FALSE))
 
# plot labels
category = sort(unique(new_data_dhs$cluster_cat))
cat_num = 1:7
cat_labels = paste(cat_num,":", category)
 
 ggplot(state_sf_dhs) +
   # clustering algorithm Map for prevalence only 
   geom_sf(color='lightgrey')+
   geom_point(data = new_data_dhs,
              aes(fill = as.factor(cluster_cat),  geometry = geometry),
              stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
   scale_fill_manual(labels = cat_labels, values = myColors)+
   # scale_fill_discrete(breaks = cat_labels)+
   theme(legend.position = "right", legend.background = element_blank()) + 
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_blank(), 
         axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
         axis.text.y=element_blank(), axis.ticks.y=element_blank())+
   labs(x = "", y  = "",  fill = "") 
   
 
 spatialdata <- new_data %>%
   # data prep for stacked bar plot 
   dplyr::select(cluster_numbers, latitude,
                 longitude, Zone, mean, cluster_cat,
                 cluster_cat_ub, cluster_cat_lb)
 
 
combined_data <- inner_join(map_df_dhs_all,spatialdata,  
                            by = c("LONGNUM"  = "longitude",
                                   "LATNUM"= "latitude"))


combineddata <- combined_data %>%
  # data prep for stacked plot
  dplyr::select(cluster_numbers,mean,
                out.model.classification, cluster_cat,
                cluster_cat_ub, cluster_cat_lb) %>% 
  # dplyr::filter(out.model.classification == 7) %>% 
  group_by(cluster_cat, out.model.classification,) %>% 
  summarise(number  = n())





ggplot(combineddata, aes(fill= cluster_cat, y=number,
                         x=out.model.classification)) + 
  # Stacked plot  code 
  geom_bar(position='stack', stat='identity')+ 
  scale_x_continuous(breaks = seq(1,7,1) )+
  theme(legend.position = "right", legend.background = element_blank(),
        panel.grid.major = element_blank())+
  labs(x = "clustering classification", fill = "inla prevalence classes",
       y = "Frequency") 

## Ploting the clusters


clPairs(combined_data[,1:2], cl = combined_data$out.model.classification)

combined_data$out.model.classification

library(ggpubr)
library(factoextra)


if(require("mclust")){
  
  # Compute model-based-clustering 
  require("mclust")
  data("diabetes")
  mc <- Mclust(diabetes[, -1])
  
  # Visaulize BIC values
  fviz_mclust_bic(mc)
  
  # Visualize classification
  fviz_mclust(mc, "classification", geom = "point")

  
  }#> Le chargement a nécessité le package : mclust#> Package 'mclust' version 5.3#> Type 'citation("mclust")' for citing this R package in publications.

