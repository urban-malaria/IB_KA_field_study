library(glm2)
library(tidyr)
library(leaflet)
library(viridis)
library(INLABMA)
library(INLA)

##Practice with INLA
head(gambia)
d <- group_by(gambia, x, y) %>%
  summarize(
    total = n(),
    positive = sum(pos),
    prev = positive / total
  )
head(d)

total <- aggregate(
  gambia$pos,
  by = list(gambia$x, gambia$y),
  FUN = length
)
positive <- aggregate(
  gambia$pos,
  by = list(gambia$x, gambia$y),
  FUN = sum
)
prev <- positive$x / total$x

d <- data.frame(
  x = total$Group.1,
  y = total$Group.2,
  total = total$x,
  positive = positive$x,
  prev = prev
)
sps <- SpatialPoints(d[, c("x", "y")],
                     proj4string = CRS("+proj=utm +zone=28")
)
spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))

d[, c("long", "lat")] <- coordinates(spst)
head(d)

pal <- colorBin("viridis", bins = c(0, 0.25, 0.5, 0.75, 1))
leaflet(d) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~ pal(prev)) %>%
  addLegend("bottomright",
            pal = pal, values = ~prev,
            title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))

##Creating mesh vertices and visualization
library(INLA)
coo <- cbind(d$long, d$lat)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)

# optional 
plot(mesh)
points(coo, col = "red")
mesh$n

##Building SPDE model

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

##Generate the index dataset for the SPDE model
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

##Developing the projection matrix
A <- inla.spde.make.A(mesh = mesh, loc = coo)

##Prediction data generation by setting location to points of the raster.
# Get the raster(r)
library(raster)
r <- getData(name = "alt", country = "GMB", mask = TRUE)

##Add altitude values to data frame d
d$alt <- raster::extract(r,d[, c("long", "lat")])
view(d)
dp <- rasterToPoints(r)
dim(dp)
view(dp)
ra <- aggregate(r, fact = 5, fun = mean)

dp <- rasterToPoints(ra)
dim(dp)

coop <- dp[, c("x", "y")]

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)

##Stack or arrange data for estimation and prediction

# stack for estimation stk.e
stk.e <- inla.stack(
  tag = "est",
  data = list(y = d$positive, numtrials = d$total),
  A = list(1, A),
  effects = list(data.frame(b0 = 1, altitude = d$alt), s = indexs)
)

# stack for prediction stk.p
stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA, numtrials = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, altitude = dp[, 3]),
                 s = indexs
  )
)

# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

##Write model formula
formula <- y ~ 0 + b0 + altitude + f(s, model = spde)

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

##Mapping predicted malaria predictions

##rows of res$summary.fitted.values that correspond to the prediction locations can be obtained by selecting the indices of the stack
index <- inla.stack.index(stack = stk.full, tag = "pred")$data

#Creation of vectors
prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]

##Plotting of predicted prevalence map using mean
pal2 <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = coop[, 1], lat = coop[, 2],
    color = pal2(prev_mean)
  ) %>%
  addLegend("bottomright",
            pal = pal2, values = prev_mean,
            title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))

##Plotting of prevalence as rasters
r_prev_mean <- rasterize(
  x = coop, y = ra, field = prev_mean,
  fun = mean
)

pal3 <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_prev_mean, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_prev_mean), title = "Prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))


##This might not be necessary
#####Exceedance Probabilities(probability greater than a particular threshold)

index <- inla.stack.index(stack = stk.full, tag = "pred")$data

marg <- res$marginals.fitted.values[index][[1]]

1 - inla.pmarginal(q = 0.20, marginal = marg)

excprob <- sapply(res$marginals.fitted.values[index],
                  FUN = function(marg){1-inla.pmarginal(q = 0.20, marginal = marg)})

head(excprob)

r_excprob <- rasterize(
  x = coop, y = ra, field = excprob,
  fun = mean
)

pal4 <- colorNumeric("viridis", c(0, 1), na.color = "transparent")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(r_excprob, colors = pal, opacity = 0.5) %>%
  addLegend("bottomright",
            pal = pal,
            values = values(r_excprob), title = "P(p>0.2)"
  ) %>%
  addScaleBar(position = c("bottomleft"))





#####Extra Trial
# Prev_plot2 <- ggplot(state_sf_dhs) +
#   geom_sf(color='lightgrey')+
#   geom_point(data = spatial_data,
#              aes(fill = as.factor(malaria_status), geometry = geometry),
#              stat = "sf_coordinates", alpha = 0.45, size=3, shape=21) +
#   theme(legend.position = "right", legend.background = element_blank(),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_blank(), 
#         axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
#         axis.text.y=element_blank(), axis.ticks.y=element_blank())+
#   labs(x = "", y  = "",  fill = "") 

spatial_data_sf <- st_as_sf(spatial_data)

# st_geometry(spatial_data) <- NULL

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


formula = "malaria_status~ housing_quality +
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



