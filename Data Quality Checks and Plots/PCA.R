library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ggplot2)
df1<-read_csv('/Users/user/Downloads/all_malaria_data.csv')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)

#Replace those not sharing compounds with 1 
df1$number_hh_sharing_compound <- ifelse(is.na(df1$number_hh_sharing_compound.x), 1, df1$number_hh_sharing_compound.x)

unique_water_sources <- unique(df1$drinking_water_source.x)
unique_water_sources_tasks <- unique(df1$basic_hh_tasks_water_source.x)
unique_main_energy_sources <- unique(df1$basic_hh_tasks_energy_source.x)
unique_toilet_facilities <- unique(df1$toilet_facility.x)
unique_home_ownership <- unique(df1$home_ownership.x)
unique_house_type <- unique(df1$house_type.x)
#unique_floor_resident <- unique(df1$floor_resident.x)
unique_place_residing <- unique(df1$place_residing.x)
unique_number_bedrooms <- unique(df1$number_bedrooms.x)
unique_shared_toilets <- unique(df1$shared_toilets.x)
unique_bathroom_location <- unique(df1$bathroom_location.x)
unique_number_hh_sharing_compound <- unique(df1$number_hh_sharing_compound.x)
unique_ceiling_presence <- unique(df1$ceiling_presence.x)








prefixdrinking <- "drinking_water_source_"
prefixtasks <- "tasks_water_source_"
prefixenergy <- "energy_sources_"
prefixtoilet <- "toilet_facility_"
prefixhouse_type <- "house_type_"
prefixhome_ownership <- "home_ownership_"
#prefixfloor_resident <- "floor_resident_"
prefixplace_residing <- "place_residing_"
prefixnumber_bedrooms <- "number_bedrooms_"
prefixshared_toilets <- "shared_toilets_"
prefixbathroom_location <- "bathroom_location_"
prefix_number_hh_sharing_compound <- "number_hh_sharing_compound_"
prefix_ceiling_presence <- "ceiling_presence_"



# Create a new column for each unique water source
for (source in unique_water_sources) {
  
  col_name <- paste0(prefixdrinking, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$drinking_water_source.x == source, 1, 0)

}

for (source in unique_water_sources_tasks) {
  
  col_name <- paste0(prefixtasks, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$basic_hh_tasks_water_source.x == source, 1, 0)
  
}

for (source in unique_main_energy_sources) {
  
  col_name <- paste0(prefixenergy, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$basic_hh_tasks_energy_source.x == source, 1, 0)
  
}

for (source in unique_toilet_facilities) {
  
  col_name <- paste0(prefixtoilet, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$toilet_facility.x == source, 1, 0)
  
}


for (source in unique_home_ownership) {
  
  col_name <- paste0(prefixhome_ownership, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$home_ownership.x == source, 1, 0)
  
}


for (source in unique_house_type) {
  
  col_name <- paste0(prefixhouse_type, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$house_type.x == source, 1, 0)
  
}




for (source in unique_place_residing) {
  
  col_name <- paste0(prefixplace_residing, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$place_residing.x == source, 1, 0)
  
}


#for (source in unique_number_bedrooms) {
  
#  col_name <- paste0(prefixnumber_bedrooms, gsub("\\s+", "_", source))
#  df1[[col_name]] <- ifelse(df1$number_bedrooms.x == source, 1, 0)
  
#}



#for (source in unique_shared_toilets) {
  
 # col_name <- paste0(prefixshared_toilets, gsub("\\s+", "_", source))
#  df1[[col_name]] <- ifelse(df1$shared_toilets.x == source, 1, 0)
  
#}

#for (source in unique_bathroom_location) {
  
 # col_name <- paste0(prefixbathroom_location, gsub("\\s+", "_", source))
#  df1[[col_name]] <- ifelse(df1$bathroom_location.x == source, 1, 0)
  
#}


#for (source in unique_number_hh_sharing_compound) {
  
 # col_name <- paste0(prefix_number_hh_sharing_compound, gsub("\\s+", "_", source))
#  df1[[col_name]] <- ifelse(df1$number_hh_sharing_compound.x == source, 1, 0)
  
#}


for (source in unique_ceiling_presence) {
  
  col_name <- paste0(prefix_ceiling_presence, gsub("\\s+", "_", source))
  df1[[col_name]] <- ifelse(df1$ceiling_presence.x == source, 1, 0)
  
}

df1$radio_owned <- 0
df1$radio_owned[df1$radio.x == "Checked"] <- 1


df1$television_owned <- 0
df1$television_owned[df1$television.x == "Checked"] <- 1


df1$cellphone_owned <- 0
df1$cellphone_owned[df1$cellphone.x == "Checked"] <- 1


df1$phone_owned <- 0
df1$phone_owned[df1$phone.x == "Checked"] <- 1


df1$desktop_owned <- 0
df1$desktop_owned[df1$desktop.x == "Checked"] <- 1


df1$laptop_owned <- 0
df1$laptop_owned[df1$laptop.x == "Checked"] <- 1


df1$refrigerator_owned <- 0
df1$refrigerator_owned[df1$refrigerator.x == "Checked"] <- 1

df1$table_owned <- 0
df1$table_owned[df1$table.x == "Checked"] <- 1

df1$chair_owned <- 0
df1$chair_owned[df1$chair.x == "Checked"] <- 1

df1$bed_owned <- 0
df1$bed_owned[df1$bed.x == "Checked"] <- 1

df1$cupboard_owned <- 0
df1$cupboard_owned[df1$cupboard.x == "Checked"] <- 1

df1$air_conditioner_owned <- 0
df1$air_conditioner_owned[df1$air_conditioner.x == "Checked"] <- 1

df1$electric_iron_owned <- 0
df1$electric_iron_owned[df1$electric_iron.x == "Checked"] <- 1


df1$generator_owned <- 0
df1$generator_owned[df1$generator.x == "Checked"] <- 1


df1$fan_owned <- 0
df1$fan_owned[df1$fan.x == "Checked"] <- 1

df1$livestock_owned <- 0
df1$livestock_owned[df1$own_livestock.x == "Yes"] <- 1						

df1$shared_compound <- 0
df1$shared_compound[df1$shared_compound.x == "Yes"] <- 1						





View(df1)





  

household_data <- df1 |>
  select(271:350)

View(household_data)




#Problematic columns Household Data
constant_columns <- sapply(household_data, function(x) length(unique(x))) == 1
zero_variance_columns <- sapply(household_data, var) == 0
problematic_columns <- colnames(household_data)[constant_columns | zero_variance_columns]
print(problematic_columns)


household_data_detailed <- df1 |>
  select(1,135,142,143,151,152,271:350) 

View(household_data_detailed)

#Problematic columns Household Data Detailed
constant_columns_hh_data_detailed <- sapply(household_data_detailed, function(x) length(unique(x))) == 1
zero_variance_columns_hh_data_detailed <- sapply(household_data_detailed, var) == 0
problematic_columns_hh_data_detailed <- colnames(household_data_detailed)[constant_columns_hh_data_detailed | zero_variance_columns_hh_data_detailed]
print(problematic_columns_hh_data_detailed)



package_name <- "factoextra"


if (!require(package_name, character.only = TRUE)) {
  
  install.packages(package_name)
  library(package_name, character.only = TRUE)
} else {
  message(sprintf("Package '%s' is already installed.", package_name))
  library(package_name, character.only = TRUE)
}



# Adapt this code for the PCA and you can also add a script on the  major task is to 
# get the data in the right shape to feed to this pipeline. Your method or this one still requires us to 
# code the data that we going to use 

household_data1 <- data.frame(
  # replace this data with the HH data keep the long, 
  # lat, unique ID  ea_number new  etc 
  TV = c(1, 0, 1, 1, 0),
  Radio = c(1, 1, 0, 1, 0),
  Water_Source = c(1, 0, 1, 1, 0),
  Bathroom_Type = c(1, 0, 1, 1, 0),
  Shared_Bathroom = c(0, 1, 0, 0, 1)
)


pca_result <- prcomp(household_data_detailed[,-c(1:5)], center = TRUE, scale. = TRUE)
pc_scores <- pca_result$x 

household_data_detailed$wealth_index <- pc_scores[,1]

View(household_data_detailed)

ggplot(household_data_detailed,aes(x=settlement_type_new, y=wealth_index))+
  geom_boxplot(data=household_data_detailed, aes(x=settlement_type_new, y=wealth_index), outlier.shape = NA) #+
 # geom_jitter(aes(color=settlement_type_new , size = wealth_index), width=0.08)



#### 
# End here 
#### 

# below is just an aside to view the PCA output

View(summary(pca_result))


fviz_pca_ind(pca_result,
             geom.ind = "point", 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cos2"
)


plot(pc_scores[,1], pc_scores[,2], xlab="PC1", ylab="PC2", main="PCA of Household Data",
     pch=19, col=rainbow(nrow(household_data)))
text(pc_scores[,1], pc_scores[,2], labels = row.names(household_data), pos=4)



print(pc_scores)


var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(var_explained, xlab="Principal Component", ylab="Proportion of Variance Explained",
     type='b', pch=19, col="blue", main="Variance Explained by Each Principal Component")
