source("Validation/functions.R", echo= F)
source("Validation/load_paths.R", echo= F)

## -----------------------------------------
### Creating analysis data 
## -----------------------------------------

multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")

multivariate_dataset_2018 <- multivariate_dataset %>%
  dplyr::filter(dhs_year == 2018)

stateshp_dhs = readOGR(file.path(NuDir, "data", "nigeria", "shapefiles","gadm36_NGA_shp", "gadm36_NGA_1.shp")) 
state_sf_dhs = st_as_sf(stateshp_dhs)

#load spatial points
spatial_points = st_read(file.path(DHS_CDir, "Downloads", 
                                   "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")) %>% 
  dplyr::filter(URBAN_RURA == "U") %>% 
  dplyr::inner_join(multivariate_dataset_2018, by = c("DHSCLUST" = "v001"))


#load spatial points with LGA shape file

lga_spatial_points = st_read(file.path(DataDir, "shapefiles", "Nigeria_LGAs_shapefile_191016",
                                       "NGA_LGAs.shp"))

lga_spatial_points_sf <- st_coordinates(lga_spatial_points)

## -----------------------------------------
### Creating analysis data 
## -----------------------------------------

spatial_points <- spatial_points %>% 
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
  dplyr::inner_join(spatial_points)%>% # makes dataset with all rows in spatial data that is also in spatial_points
  dplyr::filter(longitude != 0.0000 & latitude != 0.0000)%>%
  drop_na(malaria_status)




# save(state_sf_dhs, 
#      spatial_data,
#      lga_spatial_points_sf, 
#      file = "Validation/dhs_data_files.RData")



