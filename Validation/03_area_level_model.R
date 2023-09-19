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
source("C:/Users/lml6626/Documents/urban-malaria-fieldstudy-data-analysis/sampling/combine_data_shapefile.R", echo=F)


library(shapefiles)
library(sp)
library(spdep)

library(glm2)
library(tidyr)
library(leaflet)
library(viridis)
library(INLABMA)
library(INLA)

multivariate_dataset <- read.csv("https://raw.githubusercontent.com/numalariamodeling/urban-malaria-dhs-publication-2022/main/data/multivariate_analysis_dataset.csv")
nigeria_state_shp <- st_read("C:/Users/lml6626/OneDrive - Northwestern University/urban_malaria/projects/mathematical_model/Nigeria_states_maps/NGA_adm1.shp")



multivariate_dataset_2018 <- multivariate_dataset %>% 
  dplyr::filter(dhs_year == 2018)



spatial_data <- multivariate_dataset_2018 %>% 
  dplyr::select(State = shstate,cluster_numbers = v001,
                housing_quality = housing_q, 
                malaria_status = positives, Total_tested = num_child_6_59,
                pop_density = pop_density_0m, vegetation_index = EVI_0m,
                waterbodies_distance = dist_water_bodies_0m, popn_size = clust_pop,
                healthcare_distance = motorized_travel_healthcare_2019_2000m) %>% 
  group_by(State) %>% 
  summarise(housing_quality = mean(housing_quality, na.rm = T), 
            malaria_status = sum(malaria_status, na.rm = T),
            Total_tested = sum(Total_tested, na.rm = T),
            pop_density = mean(pop_density, na.rm = T),
            vegetation_index = mean(vegetation_index, na.rm = T),
            waterbodies_distance = mean(waterbodies_distance, na.rm = T), 
            healthcare_distance = mean(healthcare_distance, na.rm = T))



spatial_data$NAME_1 <- nigeria_state_shp$NAME_1[-36] # needs checking 


spatial_data_modified <- merge(x = nigeria_state_shp,
                               y = spatial_data, 
                               by = "NAME_1")

spatial_data_modified_sf <- st_transform( x= spatial_data_modified, 
                                          crs='+proj=longlat +datum=WGS84 +no_defs')


library(leaflet)

colours <- colorNumeric(palette = "YlOrRd", domain = (spatial_data_modified_sf$malaria_status/spatial_data_modified_sf$Total_tested ))

leaflet(data=spatial_data_modified_sf) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(spatial_data_modified_sf$malaria_status/spatial_data_modified_sf$Total_tested ), color="", weight=1,
              fillOpacity = 0.7) %>%
  addLegend(pal = colours, values = (spatial_data_modified_sf$malaria_status/spatial_data_modified_sf$Total_tested ), opacity = 1,
            title="raw prevalence") %>% 
  addScaleBar(position="bottomleft")


W.nb <- poly2nb(spatial_data_modified_sf, row.names = spatial_data_modified_sf$NAME_1)
W <- nb2mat(W.nb, style="B")


vegetation_index  <- spatial_data_modified_sf$vegetation_index  # vegetation could ne the disimilarity


vegetation_index  <- as.matrix(dist(vegetation_index , diag=TRUE, upper=TRUE))

formula <- (malaria_status ~ housing_quality + pop_density + vegetation_index +
              waterbodies_distance + pop_density + offset(spatial_data_modified_sf$malaria_status/spatial_data_modified_sf$Total_tested))


# Using S.CARleroux

model.spatial2 = S.CARleroux(as.formula(formula), 
                             family="poisson", #trials = spatial_data_modified_sf$Total_tested,
                             data = spatial_data_modified_sf,
                             W = W, burnin = 20000, 
                             n.sample = 500000, 
                             thin = 100,verbose=TRUE)


model.spatial2
plot(model.spatial2$samples$rho)
plot(model.spatial2$samples$tau)



# Using S.CARdissimilarity