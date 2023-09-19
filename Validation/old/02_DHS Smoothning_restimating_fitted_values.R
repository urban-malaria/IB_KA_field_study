rm(list = ls())

source("~/GitHub/urban-malaria-fieldstudy-data-analysis/sampling/DHS Smoothning.R") 


spatial_data_modified <- spatial_data %>% 
  group_by(Zone) %>%
  mutate(sampled_status = ifelse( test = pop_density > 635.8114 & pop_density < max(pop_density,na.rm = TRUE),  # drop last 20 where pop size and density values
                                  sample(x = c(1,0), size = 1,prob = c(0.75,0.25)),
                                  sample(x = c(1,0), size = 1,prob = c(0.25,0.75)))) 


##Extraction of fitting and estimation dataset
spatial_filtered_data_regression <- spatial_data_modified %>%
  filter(sampled_status == 1)

view(spatial_filtered_data_regression)

spatial_filtered_data_predicting <- spatial_data_modified %>%
  filter(sampled_status == 0)


# fit the model to all the dataset 

coo <- cbind(spatial_filtered_data_regression$longitude, spatial_filtered_data_regression$latitude)


mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)



spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)


indexs <- inla.spde.make.index("s", spde$n.spde)

lengths(indexs)

projection_matrix <- inla.spde.make.A(mesh = mesh, loc = coo)

raster_nigeria <- getData(name = "alt", country = "NG", mask = TRUE)


spatial_filtered_data_regression$alt <- raster::extract(raster_nigeria,spatial_filtered_data_regression[, c("longitude", "latitude")])

prediction_data_points <- rasterToPoints(raster_nigeria)


coop <- prediction_data_points[, c("x", "y")]

mesh_nodes_predictions_locations <- inla.spde.make.A(mesh = mesh, loc = coop)


stack.estimation <- inla.stack(
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



stk.full <- inla.stack(stack.estimation)


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


str(res)


estimated_data <- data.frame (res$summary.fitted.values) %>%
  dplyr::select(mean, sd)

## Plot the posterior mean:
plot(mesh, rgl=TRUE,
     res$summary.fitted.values$field[,"mean"],
     color.palette = col.pal)

## Plot residual field:
plot(mesh, rgl=TRUE,
     result$summary.random$field[,"mean"]-field.fcn(mesh$loc),
     color.palette = col.pal)




