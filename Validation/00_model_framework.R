rm(list = ls())
setwd(getwd())

source("Validation/load_paths.R", echo= F)
source("Validation/create_analysis_data.R", echo= F)


## -----------------------------------------
### Fitting model to data using DHS points
## -----------------------------------------

#------------------------------------------
##Mesh Construction to be used to build the SPDE model
#------------------------------------------

#Setting up location coordinates used as initial mesh vertices
co_ordinates <- cbind(spatial_data$longitude, 
             spatial_data$latitude)

#Setting up the mesh
mesh <- inla.mesh.2d(
  loc = co_ordinates, max.edge = c(1),
cutoff = 1/5) 

# Checking number of mesh vertices
mesh$n

#Plotting the mesh
plot(mesh); points(spatial_data$longitude, 
                   spatial_data$latitude, col = "blue")


#-----------------------------------------------------
##Building the SPDE model on the mesh
#------------------------------------------------------

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)


#Generating the index set for the SPDE model

indexs <- inla.spde.make.index("s", spde$n.spde)

lengths(indexs)

#Build a projection matrix from observations to mesh nodes

projection_matrix <- inla.spde.make.A(mesh = mesh, loc = co_ordinates)



#Setting up the prediction data


#Getting the altitude data from Nigeria Raster
raster_nigeria <- getData(name = "alt", country = "NG", mask = TRUE)


#Adding altitude data to current dataframe(spatial_data)
spatial_data$alt <- raster::extract(raster_nigeria, 
                                             spatial_data[, c("longitude", "latitude")])


#Construction of matrix that projects from the prediction locations to mesh nodes
prediction_data_points <- rasterToPoints(raster_nigeria)

coop <- prediction_data_points[, c("x", "y")]

mesh_nodes_predictions_locations <- inla.spde.make.A(mesh = mesh, loc = coop)#



#Organizing data using stack

#Construct stack with data for estimation(using the data vector, projection matrix and effects)
stack.estimation <- inla.stack(
  tag = "est",
  data = list(y = spatial_data$malaria_status, 
              numtrials = spatial_data$Total_tested),
  A = list(1, projection_matrix),
  effects = list(data.frame(b0 = 1, altitude = spatial_data$alt,
                            housing_quality = spatial_data$housing_quality,
                            pop_density = spatial_data$pop_density,
                            vegetation_index = spatial_data$vegetation_index,
                            waterbodies_distance = spatial_data$waterbodies_distance,pop_density = spatial_data$pop_density), s = indexs)
)

#Construct stack with data for prediction(response vector is set to NA, data is specified at prediction locations)

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


#Combining stack estimation and prediction

stk.full <- inla.stack(stack.estimation, stack.prediction)


#Specifying model formula
formula <- y ~ 0 + b0 + altitude + housing_quality + pop_density + vegetation_index +
  waterbodies_distance + waterbodies_distance + pop_density + f(s, model = spde)



## Fitting the model and predicting inside the inla framework to fit the model

res <- inla(formula,
            family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(
              compute = TRUE, link = 1,
              A = inla.stack.A(stk.full)
            )
)


index.prd<-inla.stack.index(stk.full, "prd")$data

post.mean.prd.logit<-res$summary.linear.predictor[index.prd,"mean"]
post_lower_bound = res$summary.linear.predictor[index.prd, "0.025quant"]
post_upper_bound = res$summary.linear.predictor[index.prd, "0.975quant"]

p.prd<-exp(post.mean.prd.logit)/(1 + exp(post.mean.prd.logit))
p.prd_lb <- exp(post_lower_bound)/(1 + exp(post_lower_bound))
p.prd_ub <- exp(post_upper_bound)/(1 + exp(post_upper_bound))

inla_predictions = data.frame(spatial_data$cluster_numbers, p.prd,p.prd_lb, p.prd_ub)
save(inla_predictions, file  = "Validation/inla_predictions.RData")


save(p.prd, p.prd_lb, p.prd_ub,  data_predicted, file  = "Validation/predicted_values.RData")


path <- file.path(Drive, "/urban_malaria/data/nigeria/Raster_files/housing")
reference.image <- raster(file.path(path, "2019_Nature_Africa_Housing_2000_NGA.tiff"))

x <- as.matrix(coop)
z <- as.matrix(p.prd)
pr.mdg.in<-rasterize(x, reference.image, 
                     field=z, fun='last', background=0)
par(mfrow=c(1,1))
plot(pr.mdg.in, main = 'Prediction using INLA')

#get predicted values for which mesh 
spatial_data$predicted_values <- raster::extract(pr.mdg.in, 
                                    spatial_data[, c("longitude", "latitude")], 
                                    fun = mean)
summary(spatial_data$predicted_values)

#aggregating up to state level to see what model predicts 
spath <- file.path(Drive, "/urban_malaria/data/nigeria/shapefiles/gadm36_NGA_shp")
state_shp = readOGR(file.path(spath, "gadm36_NGA_1.shp"))
state_shp = st_as_sf(state_shp)

reference.image <- raster(file.path(path, "2019_Nature_Africa_Housing_2000_NGA.tiff"))
state_shp$predicted_values <- raster::extract(pr.mdg.in, state_shp, fun=max, buffer=0)








# ##Mapping the malaria prevalence
# 
# index <- inla.stack.index(stack = stk.full, tag = "prd")$data
# 
# spatial_data$prev_mean <- res$summary.fitted.values[index, "mean"]
# 
# spatial_data$prev_sd <- res$summary.fitted.values[index, "sd"]
# 
# 
# spatial_data$prev_ll <- res$summary.fitted.values[index, "0.025quant"]
# spatial_data$prev_ul <- res$summary.fitted.values[index, "0.975quant"]
# 
# 
# ##--------------------------------------------------
# #Converting from logit
# ##--------------------------------------------------
# library(locfit)
# 
# data_v <- as.vector(data)
# 
# values <- lapply(data_v, function(x){cbind (x, saep.est= expit(x[["0.5quant"]]), saep.up= expit(x[["0.975quant"]]), saep.low= expit(x[["0.025quant"]]))})
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Below are other trouble shooting methods have tried but do not want to delete them yet#-----------
# 
# 
# ##-------------------
# #Ploting method 1
# ##-------------------
# 
# head(spatial_data)
# 
# nn1 <- pivot_longer(spatial_data, cols = c("prev_mean", "prev_11", "prev_ul"))
# 
# ggplot(nn, aes(x="longitude", y="latitude", fill=value)) +
#   geom_raster() +
#   facet_wrap(~name) +
#   scale_fill_continuous(type = "viridis")
# 
# nns <- pivot_longer(data, cols = c("sd", "0.025quant", "0.975quant"))
# 
# ggplot(nns, aes(x="longitude", y="latitude", fill=value)) +
#   geom_point()+
#   geom_raster() +
#   facet_wrap(~name) +
#   scale_fill_continuous(type = "viridis")
# 
# 
# class(spatial_data)
# pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")
# 
# #Plot in raster
# str(prev_mean_df)
# 
# prev_mean_df <- as.data.frame(prev_mean, xy = TRUE)
# 
# ggplot()+
#   geom_raster(data = prev_mean_df, aes(x="longitude", y="latitude", fill=prev_sd)) +
#   scale_fill_continuous(type = "viridis")+
#   coord_quickmap()
# 
# 
# 
# ##----others--------#
# 
# library(reshape2)
# 
# dpm <- melt(spatial_data,
#             id.vars = c("longitude", "latitude"),
#             measure.vars = c("pred_mean", "pred_ll", "pred_ul")
# )
# head(dpm)
# 
# 
# #Converting to Raster
# 
# 
# r <- raster(xmn = min(spatial_data$longitude), xmx = max(spatial_data$longitude),
#             ymn = min(spatial_data$latitude), ymx = max(spatial_data$latitude),
#             resolution = 25)
# 
# r_prev_mean <- rasterize(
#   x = coop, y = r, field = prev_mean,
#   fun = mean
# )
# 
# plot(r_prev_mean)
# 
# r_prev_sd <- rasterize(
#   x = coop, y = r, field = prev_sd,
#   fun = mean
# )
# 
# plot(r_prev_sd)
# 
# ggplot(data, aes(x="longitude", y="latitude", fill=value)) +
#   geom_raster() +
#   facet_wrap(~name) +
#   scale_fill_continuous(type = "viridis")
# 
# 
# 
# pal <- colorNumeric("viridis", c(0, 1), na.color = "transparent")
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addRasterImage(r_prev_mean, colors = pal, opacity = 0.5)%>%
#   addLegend("bottomright",
#             pal = pal,
#             values = values(r_prev_mean), title = "Prev.") %>%
#   addScaleBar(position = c("bottomleft"))
# 
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircles(
#     lng = coop[, 1], lat = coop[, 2],
#     color = pal(prev_mean)
#   ) %>%
#   addLegend("bottomright",
#             pal = pal, values = prev_mean,
#             title = "Prev."
#   ) %>%
#   addScaleBar(position = c("bottomleft"))
# 
# 
# ## Plot the posterior mean:
# plot(mesh, rgl=TRUE,
#      estimated_data$field[,"mean"],
#      color.palette = col.pal)
# 
# ## Plot residual field:
# plot(mesh, rgl=TRUE,
#      res$summary.fitted.values$field[,"mean"]-field.fcn(mesh$loc),
#      color.palette = col.pal)