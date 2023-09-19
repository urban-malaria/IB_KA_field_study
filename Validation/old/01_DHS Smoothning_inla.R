rm(list = ls())

## -----------------------------------------
### Paths
## -----------------------------------------
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  user_path <- file.path("/Users", user, "Library", "CloudStorage")
  Drive <- file.path(user_path, "OneDrive-NorthwesternUniversity")
  NuDir <- file.path(Drive, "urban_malaria")
  DataDir <-file.path(NuDir, "data", "nigeria")
  ProjDir <- file.path(NuDir, "projects")
  SampDir <- file.path(ProjDir, "sampling")
  DHS_CDir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data', 'DHS')
  Raster_Dir <- file.path(NuDir,'data', 'Raster_files')
  #DHS_data <- file.path(DHS_Dir, 'DHS')
  #CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')
  mod_file = file.path(ProjDir, "mathematical_model")
  
} else {
  
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
}

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


library(glm2)
library(tidyr)
library(dplyr)
library(leaflet)
library(viridis)
library(INLABMA)
library(INLA)
library(sf)


## -----------------------------------------
### Creating analysis data 
## -----------------------------------------

multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")

multivariate_dataset_2018 <- multivariate_dataset %>%dplyr::filter(dhs_year == 2018)


write.csv(multivariate_dataset_2018, file.path(DHS_CDir, "multivariatedataset_clustering_2018.csv"))

#load spatial points
sf18 = st_read(file.path(DHS_CDir, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  dplyr::filter(URBAN_RURA == "U") %>% 
  dplyr::inner_join(multivariate_dataset_2018, by = c("DHSCLUST" = "v001"))


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
  dplyr::inner_join(state_coordinates)%>%
  dplyr::filter(longitude != 0.0000 & latitude != 0.0000)%>%
  drop_na(malaria_status)



spatial_data_modified <- spatial_data %>% 
  group_by(Zone) %>%
  mutate(sampled_status = ifelse( test = pop_density > 635.8114 & pop_density < max(pop_density,na.rm = TRUE),  # drop last 20 where pop size and density values
                                  sample(x = c(1,0), size = 1,prob = c(0.75,0.25)),
                                  sample(x = c(1,0), size = 1,prob = c(0.25,0.75)))) 


## -----------------------------------------
### Fitting model to data 
## -----------------------------------------

coo <- cbind(spatial_data_modified$longitude, spatial_data_modified$latitude)


mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)

plot(mesh)

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)


indexs <- inla.spde.make.index("s", spde$n.spde)

lengths(indexs)

projection_matrix <- inla.spde.make.A(mesh = mesh, loc = coo)

raster_nigeria <- getData(name = "alt", country = "NG", mask = TRUE)



spatial_data_modified$alt <- raster::extract(raster_nigeria,spatial_data_modified[, c("longitude", "latitude")])

prediction_data_points <- rasterToPoints(raster_nigeria)


coop <- prediction_data_points[, c("x", "y")]

mesh_nodes_predictions_locations <- inla.spde.make.A(mesh = mesh, loc = coop)


stack.estimation <- inla.stack(
  tag = "est",
  data = list(y = spatial_data_modified$malaria_status, 
              numtrials = spatial_data_modified$Total_tested),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data_modified$alt,
                            housing_quality = spatial_data_modified$housing_quality,
                            pop_density = spatial_data_modified$pop_density,
                            vegetation_index = spatial_data_modified$vegetation_index,
                            waterbodies_distance = spatial_data_modified$waterbodies_distance,
                            pop_density = spatial_data_modified$pop_density), s = indexs)
)

stack.prediction <- inla.stack(
  tag = "prd",
  data = list(y = NA, 
              numtrials = NA),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data_modified$alt,
                            housing_quality = spatial_data_modified$housing_quality,
                            pop_density = spatial_data_modified$pop_density,
                            vegetation_index = spatial_data_modified$vegetation_index,
                            waterbodies_distance = spatial_data_modified$waterbodies_distance,
                            pop_density = spatial_data_modified$pop_density), s = indexs)
)



stk.full <- inla.stack(stack.estimation, stack.prediction)


formula <- y ~ 0 + b0 + altitude + housing_quality + pop_density + vegetation_index +
  waterbodies_distance + waterbodies_distance + pop_density + f(s, model = spde)
 
## Fitting the model

res_all <- inla(formula,
            family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(
              compute = TRUE, link = 1,
              A = inla.stack.A(stk.full)
            )
)


str(res)


estimated_data <- data.frame (res_all$summary.fitted.values) %>%
  dplyr::select(mean, sd)

##Extraction of fitting and estimation dataset
spatial_filtered_data_regression <- spatial_data_modified %>%
  filter(sampled_status == 1)

view(spatial_filtered_data_regression)
spatial_filtered_data_predicting <- spatial_data_modified %>%
  filter(sampled_status == 0)
view(spatial_filtered_data_predicting)



##Option B
stack.estimation_B <- inla.stack(
  tag = "est",
  data = list(y = spatial_data_modified$malaria_status, 
              numtrials = spatial_data_modified$Total_tested),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data_modified$alt,
                            housing_quality = spatial_data_modified$housing_quality,
                            pop_density = spatial_data_modified$pop_density,
                            vegetation_index = spatial_data_modified$vegetation_index,
                            waterbodies_distance = spatial_data_modified$waterbodies_distance,
                            pop_density = spatial_data_modified$pop_density), s = indexs)
)

stk.full_B <- inla.stack(stack.estimation_B)


formula <- y ~ 0 + b0 + altitude + housing_quality + pop_density + vegetation_index +
  waterbodies_distance + waterbodies_distance + pop_density + f(s, model = spde)

## Fitting the model

res_B <- inla(formula,
                family = "binomial", Ntrials = numtrials,
                control.family = list(link = "logit"),
                data = inla.stack.data(stk.full_B),
                control.predictor = list(
                  compute = TRUE, link = 1,
                  A = inla.stack.A(stk.full_B)
                )
)

str(res_B)
## Predicted Values
library(inlabru)

Predicted_values= predict(res_B , 
                          data = spatial_data_modified_prediction, 
                          ~ exp-(0 + b0 + altitude + housing_quality + pop_density + vegetation_index +  waterbodies_distance + waterbodies_distance + pop_density +
                                   f(s, model = spde)))


## Plot the posterior mean:
plot(mesh, rgl=TRUE,
     estimated_data$field[,"mean"],
     color.palette = col.pal)

## Plot residual field:
plot(mesh, rgl=TRUE,
     res$summary.fitted.values$field[,"mean"]-field.fcn(mesh$loc),
     color.palette = col.pal)


##Troubleshooting##


  
