rm(list = ls())
setwd(getwd())
source("Validation/load_data.R", echo= F)


#-----------------------------------------
##LGA coordinates
#-----------------------------------------

lga_co_ordinates <- cbind(lga_spatial_points_sf[,1], 
                          lga_spatial_points_sf[,2])

lga_mesh <- inla.mesh.2d(
  loc =  lga_co_ordinates, max.edge = c(1),
  cutoff = 1/5) 

plot(lga_mesh); points(lga_spatial_points$longitude, 
                       lga_spatial_points$latitude, col = "blue")

spde_lga <- inla.spde2.matern(mesh = lga_mesh, alpha = 2, constr = TRUE)


indexs_lga <- inla.spde.make.index("s", spde_lga$n.spde)

lengths(indexs_lga)

lga_projection_matrix <- inla.spde.make.A(mesh = lga_mesh, loc = lga_co_ordinates)

raster_nigeria <- getData(name = "alt", country = "NG", mask = TRUE)

view(multivariate_dataset_2018)

spatial_data$alt <- raster::extract(raster_nigeria, 
                                    spatial_data[, c("longitude", "latitude")])

prediction_data_points <- rasterToPoints(raster_nigeria)


coop_lga <- prediction_data_points[, c("x", "y")]

lga_mesh_nodes_predictions_locations <- inla.spde.make.A(mesh = lga_mesh, loc = coop_lga)




stack.estimation <- inla.stack(
  tag = "est",
  data = list(y = spatial_data$malaria_status, 
              numtrials = spatial_data$Total_tested),
  A = list(1, lga_projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data$alt,
                            housing_quality = spatial_data$housing_quality,
                            pop_density = spatial_data$pop_density,
                            vegetation_index = spatial_data$vegetation_index,
                            waterbodies_distance = spatial_data$waterbodies_distance,
                            pop_density = spatial_data$pop_density), s = indexs_lga)
)


stack.prediction <- inla.stack(
  tag = "prd",
  data = list(y = NA, 
              numtrials = NA),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data$alt,
                            housing_quality = spatial_data$housing_quality,
                            pop_density = spatial_data$pop_density,
                            vegetation_index = spatial_data$vegetation_index,
                            waterbodies_distance = spatial_data$waterbodies_distance,
                            pop_density = spatial_data$pop_density), s = indexs)
)



stk.full <- inla.stack(stack.estimation, stack.prediction)


formula <- y ~ 0 + b0 + altitude + housing_quality + pop_density + vegetation_index +
  waterbodies_distance + waterbodies_distance + pop_density + f(s, model = spde)

## Fitting the model

res <- inla(formula,
            family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(
              compute = TRUE, link = 1,
              A = inla.stack.A(stk.full)
            )
)

data <- res$summary.fitted.values
