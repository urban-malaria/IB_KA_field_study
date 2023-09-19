##Option A

# fit the model to dataset for spatial_filtered_data_regression ##

coo <- cbind(spatial_filtered_data_regression$longitude, spatial_filtered_data_regression$latitude)


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



spatial_filtered_data_regression$alt <- raster::extract(raster_nigeria,spatial_filtered_data_regression[, c("longitude", "latitude")])

prediction_data_points <- rasterToPoints(raster_nigeria)


coop <- prediction_data_points[, c("x", "y")]

mesh_nodes_predictions_locations <- inla.spde.make.A(mesh = mesh, loc = coop)


stack.estimation_A <- inla.stack(
  tag = "est",
  data = list(y = spatial_filtered_data_regression$malaria_status, 
              numtrials = spatial_filtered_data_regression$Total_tested),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_filtered_data_regression$alt,
                            housing_quality = spatial_filtered_data_regression$housing_quality,
                            pop_density = spatial_filtered_data_regression$pop_density,
                            vegetation_index = spatial_filtered_data_regression$vegetation_index,
                            waterbodies_distance = spatial_filtered_data_regression$waterbodies_distance,
                            pop_density = spatial_filtered_data_regression$pop_density), s = indexs)
)


# fit the model to dataset for spatial_filtered_data_predicting ##

coo_pred <- cbind(spatial_filtered_data_predicting$longitude, spatial_filtered_data_predicting$latitude)


mesh_pred <- inla.mesh.2d(
  loc = coo_pred, max.edge = c(0.1, 5),
  cutoff = 0.01
)

plot(mesh_pred)

spde_pred <- inla.spde2.matern(mesh = mesh_pred, alpha = 2, constr = TRUE)


indexs <- inla.spde.make.index("s", spde_pred$n.spde)

lengths(indexs)

projection_matrix_pred <- inla.spde.make.A(mesh = mesh_pred, loc = coo_pred)

raster_nigeria <- getData(name = "alt", country = "NG", mask = TRUE)



spatial_filtered_data_predicting$alt <- raster::extract(raster_nigeria,spatial_filtered_data_predicting[, c("longitude", "latitude")])

prediction_data_points_pred <- rasterToPoints(raster_nigeria)


coop_pred <- prediction_data_points_pred[, c("x", "y")]

mesh_nodes_predictions_locations_pred <- inla.spde.make.A(mesh = mesh_pred, loc = coop_pred)

stack.prediction_A <- inla.stack(
  tag = "prd",
  data = list(y = NA, 
              numtrials = NA),
  A = list(1, projection_matrix_pred),
  effects = list(data.frame(b0 = 1, altitude = spatial_filtered_data_predicting$alt,
                            housing_quality = spatial_filtered_data_predicting$housing_quality,
                            pop_density = spatial_filtered_data_predicting$pop_density,
                            vegetation_index = spatial_filtered_data_predicting$vegetation_index,
                            waterbodies_distance = spatial_filtered_data_predicting$waterbodies_distance,
                            pop_density = spatial_filtered_data_predicting$pop_density), s = indexs)
)



stk.full_A <- inla.stack(stack.estimation_A, stack.prediction_A)


formula <- y ~ 0 + b0 + altitude + housing_quality + pop_density + vegetation_index +
  waterbodies_distance + waterbodies_distance + pop_density + f(s, model = spde)


## Fitting the model

res_A <- inla(formula,
              family = "binomial", Ntrials = numtrials,
              control.family = list(link = "logit"),
              data = inla.stack.data(stk.full_A),
              control.predictor = list(
                compute = TRUE, link = 1,
                A = inla.stack.A(stk.full_A)
              )
)
##Trials##...not running

index <- inla.stack.index(stack = stk.full_A, tag = "prd")$data

prev_mean <- res_A$summary.fitted.values[index, "mean"]
prev_ll <- res_A$summary.fitted.values[index, "0.025quant"]
prev_ul <- res_A$summary.fitted.values[index, "0.975quant"]

library(leaflet)

pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = coop[, 1], lat = coop[, 2],
    color = pal(prev_mean)
  ) %>%
  addLegend("bottomright",
            pal = pal, values = prev_mean,
            title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))
