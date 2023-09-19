rm(list = ls())
setwd(getwd())
source("Validation/load_data.R", echo= F)


## -----------------------------------------
### Mesh creation and Selection 
## -----------------------------------------

co_ordinates <- cbind(spatial_data$longitude, 
                      spatial_data$latitude)

mesh <- inla.mesh.2d(
  loc = co_ordinates, max.edge = c(1),
  cutoff = 1/5) 

plot(mesh); points(spatial_data$longitude, 
                   spatial_data$latitude, col = "blue")


#-----------------------------------------
##LGA 
#-----------------------------------------

lga_co_ordinates <- cbind(lga_spatial_points_sf[,1], 
                      lga_spatial_points_sf[,2])

lga_mesh <- inla.mesh.2d(
  loc =  lga_co_ordinates, max.edge = c(1),
  cutoff = 1/5) 

plot(lga_mesh); points(spatial_data$longitude, 
                   spatial_data$latitude, col = "blue")


#-----------------------------------------
##LGA max.edge = c(2)
# cutoff = 1/5
#-----------------------------------------

lga_mesh2 <- inla.mesh.2d(
  loc =  lga_co_ordinates, max.edge = c(2),
  cutoff = 1/5) 

plot(lga_mesh2); points(spatial_data$longitude, 
                       spatial_data$latitude, col = "blue")


#-----------------------------------------
##LGA add offset 
#-----------------------------------------

lga_mesh_offset <- inla.mesh.2d(loc =  lga_co_ordinates,
                                max.edge = c(1),
                                cutoff = 1/5, 
                                offset = c(0.1, 0.4)) 



plot(lga_mesh_offset); points(spatial_data$longitude, 
                              spatial_data$latitude, col = "blue")

#-----------------------------------------
##LGA add offset and a nonconex hull
#-----------------------------------------

bnd <- inla.nonconvex.hull(as.matrix(lga_co_ordinates),
                           convex = 0.5, concave = -0.15)

lga_mesh_bnd_cutoff <- inla.mesh.2d(loc =  lga_co_ordinates,
                                    boundary =  bnd, max.edge = c(1),
                                    cutoff = 1/5 #, offset = c(0.1, 0.04)
                                    ) 



plot(lga_mesh_bnd_cutoff); points(spatial_data$longitude, 
                       spatial_data$latitude, col = "green")


#-----------------------------------------
##LGA add offset and boundary =  lga_co_ordinates
#-----------------------------------------

# lga_mesh_bnd_cutoff <- inla.mesh.2d(boundary =  lga_co_ordinates, 
#                                     max.edge = c(1),
#                                     cutoff = 1/5, 
#                                     offset = c(0.1, 0.04)) 

lga_mesh_bnd_cutoff <- inla.mesh.2d(loc = co_ordinates,
                                    boundary = bnd,
                                    max.edge = c(1),
                                    cutoff = 1/5,
                                    offset = c(0.1, 0.4))

plot(lga_mesh_bnd_cutoff); points(spatial_data$longitude, 
                                  spatial_data$latitude, col = "blue")


#-------------------------------------------------
##LGA add domain and offset
#-------------------------------------------------

lga_mesh_dmn_offset <- inla.mesh.2d(loc.domain = lga_co_ordinates,
                                    max.edge = c(0.3, 0.5),
                                    offset = c(0.03, 0.5)) 

plot(lga_mesh_dmn_offset); points(spatial_data$longitude, 
                                  spatial_data$latitude, col = "blue")


#--------------------------------------------------
##LGA add domain, offset and cutoff
#--------------------------------------------------

lga_mesh_dmn_offset_cut <- inla.mesh.2d(loc.domain = lga_co_ordinates,
                                        max.edge = c(0.3, 0.5),
                                        offset = c(0.03, 0.5),
                                        cutoff = 0.1)

plot(lga_mesh_dmn_offset_cut); points(spatial_data$longitude, 
                                  spatial_data$latitude, col = "blue")


##Others

bnd <- inla.nonconvex.hull(lga_co_ordinates)
meshb <- inla.mesh.2d(
  boundary = bnd, offset = c(0.1, 0.4),
  cutoff = 1/5, max.edge = c(1)
)
plot(meshb)
points(spatial_data$longitude, 
       spatial_data$latitude, col = "red")