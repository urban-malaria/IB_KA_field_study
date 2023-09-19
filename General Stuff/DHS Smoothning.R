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
#DHS_data <- file.path(DHS_Dir, 'DHS')
#CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')
mod_file = file.path(ProjDir, "mathematical_model")


library(glm2)
library(tidyr)
library(leaflet)
library(viridis)
library(INLABMA)
library(INLA)

multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")

multivariate_dataset_2018 <- multivariate_dataset %>% 
  filter(dhs_year == 2018)
view(multivariate_dataset_2018)

write.csv(multivariate_dataset_2018, file.path(DHS_CDir, "multivariatedataset_clustering_2018.csv"))

#load spatial points
sf18 = st_read(file.path(DHS_CDir, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  filter(URBAN_RURA == "U") %>% 
  inner_join(multivariate_dataset_2018, by = c("DHSCLUST" = "v001"))


state_coordinates <- sf18 %>% 
  dplyr::select(cluster_numbers = DHSCLUST, 
                longitude = LONGNUM, 
                latitude = LATNUM,
                state = shstate,
                Zone = region)

spatial_data <- multivariate_dataset_2018 %>% 
  dplyr::select(cluster_numbers = v001, housing_quality = housing_q, 
                malaria_status = positives, Total_tested = num_child_6_59,
                pop_density = pop_density_0m, vegetation_index = EVI_0m,
                waterbodies_distance = dist_water_bodies_0m, popn_size = clust_pop,
                healthcare_distance = motorized_travel_healthcare_2019_2000m) %>% 
  inner_join(state_coordinates)%>%
  filter(longitude != 0.0000 & latitude != 0.0000)%>%
  drop_na(malaria_status)
  

spatial_data_sf <- st_as_sf(spatial_data)

# ##Bivariate plot of pop_size and density
# 
# dhst = bi_class(spatial_data, x = popn_size , y = pop_density, style = "fisher", dim = 3)
# 
# dhst_np = left_join(sf18, dhst, by = c('DHSCLUST' = 'cluster_numbers'))%>%
#   rename(class = bi_class)
# 
# # map <- ggplot() +
# #   geom_sf(data = dhst_np, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
# #   bi_scale_fill(pal = "DkBlue", dim = 3) +
# #   geom_text_repel(
# #     data = dhst_np,
# #     aes(label = DHSCLUST, geometry = geometry.x),color ='black',
# #     stat = "sf_coordinates", 
# #     min.segment.length = 0, size = 5, force = 1)+
# #   xlab('')+
# #   ylab('')+
#   
# bi_plot <- ggplot(state_sf_dhs) +
#   geom_sf(color='lightgrey')+
#   geom_point(data = dhst_np,
#              aes(fill = bi_class,  geometry = geometry.x),
#              stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
#   # theme(legend.position = "right", legend.background = element_blank()) + 
#   # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#   #       panel.background = element_blank(), axis.line = element_blank(), 
#   #       axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
#   #       axis.text.y=element_blank(), axis.ticks.y=element_blank())+
#   # labs(x = "", y  = "",  fill = "")
# 
#   bi_theme()
# legend <- bi_legend(pal = "DkBlue",
#                     dim = 3,
#                     xlab = "High population size ",
#                     ylab = "High population density",
#                     size = 4)
# finalPlot <- ggdraw() +
#   draw_plot(map, 0.1, 0, 1, 1) +
#   draw_plot(legend, 0, 0.65, 0.2, 0.2)
# 
# spatial_data_modified_n <- dhst_np %>% 
#   group_by(Zone) %>% 
#   mutate(sampled_status = ifelse( test = class =="1-1"|class == "1-2"| class == "2-1",  # drop last 20 where pop size and density values 
#                                   yes = sample(x = c(1,0), size = 1,prob = c(0.75,0.25)),
#                                   no = sample(x = c(1,0), size = 1,prob = c(0.25,0.75))))
# 
# 
# ##sum(df_pop_size$urban)


# Creation of column for sampled data set using the mutate function
spatial_data_modified <- spatial_data %>%
  group_by(Zone) %>%
  mutate(sampled_status = ifelse( test = pop_density > 635.8114 & pop_density < max(pop_density,na.rm = TRUE),  # using the mean population density to max to dichotomize
                                  sample(x = c(1,0), size = 1,prob = c(0.75,0.25)),
                                  sample(x = c(1,0), size = 1,prob = c(0.25,0.75)))) 


##Extraction of fitting and estimation dataset
spatial_filtered_data_regression <- spatial_data_modified %>%
  filter(sampled_status == 1)
  
view(spatial_filtered_data_regression)
spatial_filtered_data_predicting <- spatial_data_modified %>%
  filter(sampled_status == 0)
  
  
# ##Fitting of the Spatial Model
# dim(spatial_filtered_data_regression)
# 
# dim(unique(spatial_filtered_data_regression[, c("longitude", "latitude")]))
# 
# 
# 
# coo_ng <- cbind(spatial_data_sf$longitude, spatial_data_sf$latitude)
# mesh_ng <- inla.mesh.2d(
#   loc = coo_ng, max.edge = c(0.1, 5),
#   cutoff = 0.01
# )
# 
# plot(mesh_ng)
# points(coo, col = "red")
# 
# mesh$n
# 
# 
# 
# 
# sps <- SpatialPoints(spatial_data[, c("longitude", "latitude")],
#                      proj4string = CRS("+proj=utm +zone=28")
# )
# spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
# 
# 
# pall <- colorBin("viridis", bins = c(0, 0.25, 0.5, 0.75, 1))
# leaflet(d) %>%
#   addProviderTiles(spatial_data_sf) %>%
#   addCircles(lng = ~longitude, lat = ~latitude, color = ~ pal(malaria_status)) %>%
#   addLegend("bottomright",
#             pal = pal, values = ~prev,
#             title = "Prev."
#   ) %>%
#   addScaleBar(position = c("bottomleft"))
# 



#smoothning_data <- sample(unique(spatial_data$cluster_numbers), 20)#


#formula = "malaria_status~ housing_quality +
  pop_density + 
  waterbodies_distance +
  healthcare_distance+
  vegetation_index"


# reg_mod <- glm2(data = spatial_filtered_data_regression,
#                 formul = formula)
# 
# reg_mod1 <- glm2(data = spatial_filtered_data_predicting,
#                 formul = formula)

# y <- log(malaria_status/1-malaria_status)
# formula <- (y ~ housing_quality +
#   pop_density + 
#   waterbodies_distance +
#   healthcare_distance+
#   vegetation_index)
#   





# %>% 
#   group_by(cluster_numbers, housing_quality,
#            vegetation_index, pop_density, waterbodies_distance, 
#            healthcare_distance) %>% 
#   summarise(total = n())#,
#             positive  = sum(malaria_status, na.rm = T),
#             negatives = n() - positive)



# algorithim to sytematically delete clusters  
# Sample unique values between in unique(spatial_data$cluster_numbers)
# filter the data which is not in the second step
# Fit a spatial regression model to the filtered data and choose the model of best fit 
# predict the prevalence of the unfitted data points ******
# compare the prevalence obtained to the prevalence from a model that includes the test dataset 
# run the algorithing in a loop 10000 times.

  

