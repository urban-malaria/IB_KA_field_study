user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Documents")



## read ibadan ward shape files
df_ib = st_read(file.path(NuDir,"kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

p <- ggplot(agugu_shp) +
  geom_sf(fill = NA) +
  geom_text_repel(
    data = agugu_shp,
    aes(label =  FID, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan ")+
  coord_sf()

ib_hh_issues <- read.csv(file.path(NuDir, "values_not_in_ts.csv"))

ib_hh_issues_i <- ib_hh_issues %>% dplyr::filter(Status == "Declined" | Status == "Relocated")

#Make plots of HHs with issues
ib_hh_issues_df <- sf::st_as_sf(ib_hh_issues_i, coords=c('HOUSEHOLD.COORDINATE..Longitude', 'HOUSEHOLD.COORDINATE..Latitude'), crs=4326)

sf::st_crs(ib_hh_issues_df) <- 4326

sf::st_crs(df_ib) <- 4326

# df_ib_b <- df_ib %>%
#   dplyr::filter(WardName == 'Bashorun')

ggplot(df_ib) +
  geom_sf(fill= NA)+
  geom_point(data = ib_hh_issues_df,  aes(geometry = geometry, size = 0.2, alpha = 0.7, col = Status), stat= "sf_coordinates")+
  #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_text_repel(
     data = subset(df_ib, WardName %in% c("Olopomewa", "Bashorun", "Challenge", "Agugu")),
     aes(label =  WardName, geometry = geometry),color ='black',
     stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+ 
  ylab("")+
  xlab("")+
  labs(title= "Wards in Ibadan showing HH visited with issues(57)")+
  coord_sf()


#Bashorun issues
ba_hh_issues <- read.csv(file.path(NuDir, "Bash_trouble.csv"))

ggplot(ba_hh_issues, aes(x = Status, fill = Status)) +
  geom_bar() +
  geom_text(aes(label = stat(count), y = stat(count)), 
            stat = "count", position = position_stack(vjust = 0.5), size = 3.5) +
  labs(x = "Ward", y = "Number") +
  theme_manuscript()+
  labs(title= "Stauts of HHs to be sampled in Basorun")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##summary for Laurette
names(ib_css_hh_df)

#remove first 9 rows
row_to_delete <- 1:9
ib_css_hh_df <- ib_css_hh_df[-row_to_delete, ]


ib_unique_hhss <- ib_css_hh_dfn  %>%
  group_by(SN, Ward) %>% 
  summarise(n())

ggplot(ib_unique_hhss, aes(x = Ward, fill = Ward)) +
  geom_bar() +
  geom_text(aes(label = stat(count), y = stat(count)), 
            stat = "count", position = position_stack(vjust = 0.5), size = 3.5) +
  labs(x = "Ward", y = "Number") +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() 
#+
 # coord_flip()




##Flood data from NEMA
flood_data <- read.csv(file.path(NuDir, "excel files", "nema-flood-data_2022.csv"))

flood_data_sum <- flood_data %>% 
  group_by(STATE) %>% 
  summarise(No_affected = sum(`PERSONS.AFFECTED`))

colnames(flood_data_sum)[1] <- "StateName"

nga_sf <- st_read(file.path(NuDir, "Boundary_VaccStates_Export", "Boundary_VaccStates_Export.shp" ))

ng_plot <- ggplot(nga_sf) +
  geom_sf(fill=alpha('lightgreen', 0.5))+
  geom_text_repel(
    data = nga_sf,
    aes(label =  StateName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  #labs(title= "LGAs in Ibadan")+
  xlab("")+
  ylab("")

flood_data_df <- merge(nga_sf, flood_data_sum, by.x = "StateName", by.y = "StateName", all.x = TRUE)

flood_data_df <- flood_data_df %>% 
  mutate(NoperTh = No_affected/1000)

ib_wmall = con_gplot(flood_data_df,quo(NoperTh), quo(StateName))+
  scale_fill_continuous(name='No of people \n affected per 1000', low="lightskyblue1", high="steelblue", guide="colorbar",na.value="transparent")



raster <- raster(file.path(NuDir, "NGA_population_v2_0_gridded.tif"))
fd_popndens <- raster::extract(raster,nga_sf, buffer = buffer, fun = mean, df =TRUE)

val_popndens = val_popndens$gpw_v4_population_density_rev11_2020_1_deg

df_ward_popden = cbind(ib_wall, val_popndens)


NGA_population_v2_0_gridded

ib_w_popden = con_gplot(df_ward_popden,quo(val_popndens), quo(WardName))+
  scale_fill_continuous(name='Population \n density', low="cornsilk", high="cornsilk4", guide="colorbar",na.value="transparent")

##Yearly Trend

Yr_flood_data <- read.csv(file.path(NuDir, "excel files", "yr_data_fld.csv"))

# Yr_flood_data_df <- Yr_flood_data  %>%
#   group_by(Start.Year) %>% 
#   summarise(n())

colnames(Yr_flood_data)[1] <- "StateName"


flood_data_df <- merge(nga_sf, Yr_flood_data, by.x = "StateName", by.y = "StateName", all.x = TRUE)

# flood_data_df <- flood_data_df %>% 
#   mutate(NoperTh = No_affected/1000)

ib_wmall = con_gplot(flood_data_df,quo(Count.of.N), quo(StateName))+
  scale_fill_continuous(name='Frequency of \n devastating floods', low="lightskyblue1", high="steelblue", guide="colorbar",na.value="transparent")


#Correlation analysis(Flood)

cor_flood_data <- Yr_flood_data %>% 
  dplyr::select(Count.of.N, mal_prev)

correlation_matrix <- cor(cor_flood_data, method = "pearson", use = "complete.obs")

ggplot(cor_flood_data, aes(x = Count.of.N, y = mal_prev)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "Variable 1", y = "Variable 2")


##Extra figure
Treatment <- c("Visited the hospital", "Sought advice from HP", "Visited a Chemist/PPMV", 
               "Visited a Spiritual Healer", "Took no action",
               "Visited the hospital", "Sought advice from HP", "Visited a Chemist/PPMV", 
               "Visited a Spiritual Healer", "Took no action",
               "Visited the hospital", "Sought advice from HP", "Visited a Chemist/PPMV", 
               "Visited a Spiritual Healer", "Took no action",
               "Visited the hospital", "Sought advice from HP", "Visited a Chemist/PPMV", 
               "Visited a Spiritual Healer", "Took no action")
Values <- c(18.2, 63.6, 4.6, 2.3, 11.4,
            59.5, 24.2, 10.0, 1.6, 4.8,
            90.2, 8.0, 0.9, 0.0, 0.9,
            58.9, 28.5, 6.2, 1.3, 5.1)
State <- c("Oyo", "Oyo","Oyo","Oyo","Oyo", 
          "Abia", "Abia","Abia","Abia","Abia",
          "Kano", "Kano","Kano","Kano","Kano",
          "Total", "Total","Total","Total","Total")

trt_df <- data.frame(Treatment, Values, State)


ggplot(trt_df, aes(x = as.factor(Treatment), y = Values))+
  geom_bar(position = "stack", stat = "identity", fill = "skyblue", width = 0.5)+
  geom_text(aes(x = Treatment, y= Values, label = Values), size = 2.5, vjust = -0.5)+
  facet_wrap(~ State)+
  labs(x = "Treatment seeking practice",
       y = "Proportion") +
    theme_manuscript()+
  theme(strip.background = element_rect(fill = "plum", color = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))


library(tidyr)

# Assuming your dataframe is named 'df'
# Assuming the column names are as follows:
# "SerialNumber", "VisitType", "Name", "BaselineDate", "FollowUpDate", "Longitude", "Latitude", "VisitNumber"

# Convert "VisitType" column into factor with appropriate levels to ensure correct ordering
df$VisitType <- factor(df$VisitType, levels = c("Baseline", paste("Follow up", 1:12)))

# Pivot the dataframe to wide format
wide_df <- pivot_wider(df, names_from = VisitType, values_from = c(Name, BaselineDate, FollowUpDate, Longitude, Latitude))

# Rename the columns for better clarity
colnames(wide_df) <- c("SerialNumber", "Name_Baseline", "Name_FollowUp1", "Name_FollowUp2", "Name_FollowUp3", "Name_FollowUp4",
                       "Name_FollowUp5", "Name_FollowUp6", "Name_FollowUp7", "Name_FollowUp8", "Name_FollowUp9", "Name_FollowUp10", "Name_FollowUp11", "Name_FollowUp12",
                       "BaselineDate", "FollowUp1Date", "FollowUp2Date", "FollowUp3Date", "FollowUp4Date", "FollowUp5Date",
                       "FollowUp6Date", "FollowUp7Date", "FollowUp8Date", "FollowUp9Date", "FollowUp10Date", "FollowUp11Date", "FollowUp12Date",
                       "Longitude", "Longitude_FollowUp1", "Longitude_FollowUp2", "Longitude_FollowUp3", "Longitude_FollowUp4", "Longitude_FollowUp5",
                       "Longitude_FollowUp6", "Longitude_FollowUp7", "Longitude_FollowUp8", "Longitude_FollowUp9", "Longitude_FollowUp10", "Longitude_FollowUp11", "Longitude_FollowUp12",
                       "Latitude", "Latitude_FollowUp1", "Latitude_FollowUp2", "Latitude_FollowUp3", "Latitude_FollowUp4", "Latitude_FollowUp5",
                       "Latitude_FollowUp6", "Latitude_FollowUp7", "Latitude_FollowUp8", "Latitude_FollowUp9", "Latitude_FollowUp10", "Latitude_FollowUp11", "Latitude_FollowUp12")

# Print the wide format dataframe
print(wide_df)

ibb_women_df <- read.csv(file.path(NuDir , "excel files", "UrbanMalariaWomenSur_DATA_LABELS_2024-03-19_0419.csv"))
knn_women_df <- read.csv(file.path(NuDir , "excel files", "UrbanMalariaWomenSur_DATA_LABELS_2024-03-19_0439.csv"))




error_ib <- read_xlsx(file.path(LuPDir, "coordinate errors wet_season.xlsx"))


# Define custom break points and labels
custom_breaks <- c(8, 100, 500, 1000, 1500, 2000, 10000)  # Custom break points
custom_labels <- c("Less than 100", "100-499", "500-999", "1000-1499", "1500-2000", "Above 2000")  # Custom labels

# Recode 'value' into custom categories
error_ib$dist_cat <- cut(error_ib$distance_rounded, 
                   breaks = custom_breaks,     # Use custom break points
                   labels = custom_labels,     # Assign custom labels
                   include.lowest = TRUE)      # Include the lowest value in the first bin

error_ib_clean <- error_ib %>% 
  filter(!is.na(dist_cat))

ggplot(error_ib_clean, aes(x = dist_cat, fill = dist_cat)) +  # Map category to the x-axis and color bars
  geom_bar() +                                                # Create a bar chart
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +                   # Create a bar chart
  facet_wrap(~ bi2)+
  ylim(0,30)+
  labs(x = "Distance to Ward Boundary(meters)",
       y = "Number of Households") +
  theme_manuscript()+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

Yes <- c(79.7, 77.3,73.9, 70.4, 64.1, 56.3, 71.5)
No <- c(20.3, 22.7, 26.1, 29.6, 35.9, 43.8, 28.5)
Region <- c("Kaduna", "Gombe", "Lagos", "Abuja", "Rivers", "Imo", "Total")

int_df <- data.frame(Yes, No, Region)

#Convert dataframe back to long to enable ploting stacked bar
int_df_ws <- int_df %>%
  pivot_longer(cols = c(`Yes`, `No`), 
               names_to = "Intention",
               values_to = "count")

int_df_ws$Region <- factor(int_df_ws$Region, levels = c("Kaduna", "Gombe", "Lagos", 
                                                        "Abuja", "Rivers", "Imo", "Total"))  

ggplot(int_df_ws, aes(fill=Intention, y=count, x=as.factor(Region))) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`No` = "#FED2C7", `Yes` = "#DD80AD")) +
  #geom_text(aes(x = as.factor(Region), y= count, label = count), vjust = 1.4, size = 4) +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 4)+
  xlab("State")+
  ylab("Intention to vaccinate(%)")+
  #guides(fill = "none")+
  theme_manuscript() +
  theme(
    legend.position = "right",    # Position legend to the right of the plot
    legend.justification = "center", # Align the legend vertically centered
    legend.box = "vertical",      # Ensure the legend is placed vertically
    legend.box.margin = margin(0, 10, 0, 0)  # Add space between the plot and the legend
  )

None <- c(78.6, 75.9,74.0, 59.2, 41.6, 39.7, 50.9)
Incomplete <- c(6.5, 8.6, 18.0, 20.6, 19.4, 21.6, 19.3)
Full <- c(14.8, 15.2, 8.0, 20.2, 38.9, 38.7, 29.7)

Region <- c("Imo", "Gombe", "Kaduna", "Abuja", "Rivers", "Lagos", "Total")

uptk_df <- data.frame(Full, Incomplete, None, Region)

#Convert dataframe back to long to enable ploting stacked bar
uptk_df_ws <- uptk_df %>%
  pivot_longer(cols = c(`Full`, `Incomplete`, `None`), 
               names_to = "Vaccination Status",
               values_to = "count")

uptk_df_ws$Region <- factor(uptk_df_ws$Region, levels = c("Imo", "Gombe", "Kaduna", "Abuja", "Rivers", "Lagos", "Total"))  

ggplot(uptk_df_ws, aes(fill=`Vaccination Status`, y=count, x=as.factor(Region))) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`Full` = "#FF7777", `Incomplete` = "#FED2C7", `None` = '#DD80AD')) +
  #geom_text(aes(x = as.factor(Region), y= count, label = count), vjust = 1.4, size = 4) +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 4)+
  xlab("State")+
  ylab("Vaccination Status(%)")+
  #guides(fill = "none")+
  theme_manuscript() +
  theme(
    legend.position = "right",    # Position legend to the right of the plot
    legend.justification = "center", # Align the legend vertically centered
    legend.box = "vertical",      # Ensure the legend is placed vertically
    legend.box.margin = margin(0, 10, 0, 0)  # Add space between the plot and the legend
  )


# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("tidyverse")
library(ggplot2)
library(ggrepel)
library(tidyverse)

# Get the positions
df <- data.frame(value = c(15, 25, 32, 28),
                 group = paste0("G", 1:4))

df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))

breeding_site_sum_dry2 <- breeding_site_sum_dry %>% 
  mutate(csum = rev(cumsum(rev(SitesVisited))), 
         pos = SitesVisited/2 + lead(csum, 1),
         pos = if_else(is.na(pos), SitesVisited/2, pos))

ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(value, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Group")) +
  theme_void() 


ggplot(data = breeding_site_sum_dry, aes(x = "", y = SitesVisited, fill = `Breeding site`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  geom_text(aes(label = SitesVisited), position = position_stack(vjust = 0.5), color = "black", size = 3.5) +  # Center text inside each slice
  theme(legend.position = "right") +  # Optional: Adjust legend
  theme_manuscript()+
  theme_void() +  # Clean theme for pie charts
  ggtitle("Number and type of breeding sites visited in Ibadan, Jan-March, 2023")

    theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Adjust x-axis label angle and size
    legend.position = "none",  # Remove the legend
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,100)+
      
      

##Combine breeding site summary data

breeding_site_sum_dry$Season <- "Dry"

breeding_site_sum_wet$Season <- "Wet"
      
breeding_site_sum_all <- rbind(breeding_site_sum_dry, breeding_site_sum_wet) 

library(dplyr)

# Step 1: Sum the number of SitesVisited by Breeding_Site_Recode
breeding_site_summary <- breeding_site_sum_all %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(
    TotalSitesVisited = sum(SitesVisited))

# Step 2: Calculate the overall total for all breeding sites
overall_total_sites <- sum(breeding_site_summary$TotalSitesVisited)

# Step 3: Compute the percentage for each Breeding_Site_Recode
breeding_site_summary <- breeding_site_summary %>%
  mutate(
    OverallPercentage = (TotalSitesVisited / overall_total_sites) * 100,
    Label = paste(TotalSitesVisited, "(", round(OverallPercentage, 1), "%)", sep = "")
  )

# View the final summary
breeding_site_summary

# Compute the cumulative percentages (top of each rectangle)
breeding_site_summary$ymax = cumsum(breeding_site_summary$OverallPercentage)

# Compute the bottom of each rectangle
breeding_site_summary$ymin = c(0, head(breeding_site_summary$ymax, n=-1))

# Compute label position
breeding_site_summary$labelPosition <- (breeding_site_summary$ymax + data$ymin) / 1.1

# Make the plot
bs_overal <- ggplot(breeding_site_summary, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= Breeding_Site_Recode)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=Label), size=3.5) +
  scale_fill_brewer(palette="Pastel1") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() 
  theme(legend.position = "none")

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites visited in Ibadan.pdf'), bs_overal, width = 8, height = 6)

##Overall Analysis      
##Combine data(anopheles wet and dry)
colnames(b_site_ano_sum_wet_wide) [1] <- "Breeding site"      

b_site_ano_sum_all_wide <- rbind(b_site_ano_sum_dry_wide, b_site_ano_sum_wet_wide)
      
b_site_ano_sum_all_wide_y <- b_site_ano_sum_all_wide %>% 
  dplyr::filter(Yes > 0, )

ggplot(b_site_ano_sum_all_wide_y, aes(x = Breeding_Site_Recode, y = Proportion_Larvae_Caught, size = Total_Sites)) +
  geom_point(aes(size = Total_Sites), color = "darkorchid", alpha = 0.7) +  
  facet_wrap(~ Season)+
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Proportion of Anopheles Caught Across Breeding Sites",
           y = "Proportion of Anopheles Caught (%)",
           size = "Number of Breeding Sites Visited") +
  guides(size = FALSE)+
  theme_manuscript() 

      

##Settlement Type Analysis
##Combine breeding site summary

breeding_site_sum_all_sett <- rbind(breeding_site_sum_dry_sett, breeding_site_sum_wet_sett)

breeding_site_summary_all_sett <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`) %>%
  summarise(
    TotalSitesVisited = sum(SitesVisited))

breeding_site_summary_all_sett0 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode) %>%
  summarise(
    TotalSitesVisitedn0 = sum(SitesVisited))

##Some data wrangling
breeding_site_summary_all_settx<- breeding_site_sum_all_sett %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(
    TotalSitesVisitedn0 = sum(SitesVisited))

# Step 1: Calculate the total number of sites per settlement type
total_sites_per_settlement <- breeding_site_summary_all_settx %>%
  group_by(Breeding_Site_Recode) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0))

# Step 2: Merge the total with the original data and compute the proportion
breeding_sites_with_proportion <- breeding_site_summary_all_sett0 %>%
  group_by(Breeding_Site_Recode, `Settlement Type`) %>%
  summarise(TotalSites = sum(TotalSitesVisitedn0), .groups = 'drop') %>%  # summing TotalSitesVisitedn0 per group
  left_join(total_sites_per_settlement, by = "Breeding_Site_Recode") %>%        # Joining with total sites per settlement
  mutate(Proportion = (TotalSites.x / TotalSites.y) * 100)                   # Computing the proportion



breeding_site_summary_all_sett1 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Season) %>%
  summarise(
    TotalSitesVisitedn = sum(SitesVisited))

breeding_site_summary_all_sett2 <- breeding_site_sum_all_sett %>%
  group_by(`Settlement Type`, Breeding_Site_Recode, Season) %>%
  summarise(
    TotalSitesVisitedn2 = sum(SitesVisited))

# Step 2: Calculate the overall total for all breeding sites
overall_total_sitesn <- sum(breeding_site_summary_all_sett$TotalSitesVisited)

# Step 3: Compute the percentage for each Breeding_Site_Recode
breeding_site_summary_all_sett <- breeding_site_summary_all_sett %>%
  mutate(
    OverallPercentage = (TotalSitesVisited / overall_total_sitesn) * 100,
    Label = paste(TotalSitesVisited, "(", round(OverallPercentage, 1), "%)", sep = "")
  )


Fig4 <- ggplot(breeding_sites_with_proportion, aes(fill=Breeding_Site_Recode, y=Proportion, x=as.factor(Breeding_Site_Recode))) + 
  geom_bar(position="stack", stat="identity")+
  #scale_fill_manual(values = c(`Dry` = "#FF7777", `Wet` = "#FED2C7")) +
  facet_wrap(~`Settlement Type`)+
  geom_text(aes(label = round(Proportion, 1)), 
            position = position_stack(vjust = 0.5),   # Center labels within each stack
            size = 2.5)+
  xlab("Breeding Site")+
  ylab("Number of Sites")+
  #guides(fill = "none")+
  theme_manuscript() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none")

  #   ,    # Position legend to the right of the plot
  #   legend.justification = "center", # Align the legend vertically centered
  #   legend.box = "vertical",      # Ensure the legend is placed vertically
  #   legend.box.margin = margin(0, 10, 0, 0),
  #   theme(legend.position = "none")# Add space between the plot and the legend
  # )

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites visited in Ibadan by settlement.pdf'), Fig4, width = 8, height = 6)

##Combine data(anopheles wet and dry)
b_site_ano_sum_all_wide_sett <- rbind(b_site_ano_sum_dry_wide_sett, b_site_ano_sum_wet_wide_sett)

b_site_ano_sum_all_wide_sett_y <- b_site_ano_sum_all_wide_sett %>% 
  dplyr::filter(Yes > 0, )


Fig7 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Yes)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `Season`)+
  scale_color_manual(values = c(Formal = "#376C8B", Slum = "#FF5757"))+
geom_text(aes(label = Yes), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Number and type of Breeding Sites by settlement type ",
       y = "Number of Breeding sites",
       size = "Number of Breeding Sites Visited") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Breeding sites with anopheles by settlement.pdf'), Fig7, width = 8, height = 6)

Fig8 <- ggplot(b_site_ano_sum_all_wide_sett_y, aes(x = Breeding_Site_Recode, y = Proportion_Larvae_Caught, size = Total_Sites)) +
  geom_point(aes(color = Season, size = Total_Sites), , alpha = 0.7) +  
  facet_wrap(~ `Settlement Type`)+
  scale_color_manual(values = c(Dry = "#FFD4B5", Wet = "#008080"))+
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Proportion of Anopheles Caught Across Breeding Sites",
       y = "Proportion of Anopheles Caught (%)",
       size = "Number of Breeding Sites Visited") +
  guides(size = FALSE)+
  theme_manuscript() 

  ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Proportion of Breeding sites with anopheles by settlement.pdf'), Fig8, width = 8, height = 6) 
  

  
  
  
  
# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
breeding_site_summary$ymax = cumsum(breeding_site_summary$OverallPercentage)

# Compute the bottom of each rectangle
breeding_site_summary$ymin = c(0, head(breeding_site_summary$ymax, n=-1))

# Compute label position
breeding_site_summary$labelPosition <- (breeding_site_summary$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(breeding_site_summary, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= Breeding_Site_Recode)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=Label), size=3.5) +
  scale_fill_brewer(palette=7) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")



#Extra Analysis
lav_df_wet_y <- lav_df_wet %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

lav_df_dry_y <- lav_df_dry %>% 
  dplyr::filter(Anopheles_Caught == "Yes")

table(lav_df_wet_y$`Origin of Water`)

table(lav_df_wet_y$`IIs the breeding site exposed to sunlight?`)

table(lav_df_wet_y$`Presence of Vegetation`)

table(lav_df_dry_y$`Origin of water`)

table(lav_df_dry_y$`Sunlight exposure`)

table(lav_df_dry_y$Vegetation)


dt <- data.frame(
  #Location = c("Gutter/Drainages", "Gutter/Drainages", "Gutter/Drainages", "Tyre tracks", "Tyre tracks", "Tyre tracks"),
  Season = c("Dry", "Wet"),
  An_gambiae_ss = c(13, 0),
  An_coluzzii = c(2, 25)
)

# Convert data to long format
data_long <- pivot_longer(dt, cols = c(An_gambiae_ss, An_coluzzii), names_to = "Species", values_to = "Value")

# Create a stacked bar plot
Fig10 <- ggplot(data_long, aes(x = "", y = Value, fill = Species)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c(An_gambiae_ss = "#ffdbac", An_coluzzii = "#fcbf49"))+
  facet_wrap(~Season)+
  labs(title = "Distribution of adult larvae(mosquito) by Species Type", x = "Season", y = "Number of Adult Mosquito from Larva") +
  theme_manuscript() +
  theme(legend.position = "right")


ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Distribution of adult larvae(mosquito) by Species Type.pdf'), Fig10, width = 8, height = 6) 

##Larval Density Analysis
##Compute Av. Larval density
#Dry
lav_den_sum_dry <- lav_den_sum %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )
lav_den_sum_dry$season <- "Dry"

#Wet
lav_den_sum_wett <- lav_den_sum_wet %>% 
  group_by(`Settlement Type`, `Breeding_Site_Recode`) %>%  # Group by breeding site type
  summarize(
    AvgLD = mean(`Larva_Density`, na.rm = TRUE)  # Average number of Anopheles caught per site
  )

lav_den_sum_wett$season <- "Wet"

##Combine Larval Density Data
lav_den_sum_all <- rbind(lav_den_sum_dry, lav_den_sum_wett)


Fig9 <- ggplot(lav_den_sum_all, aes(x = Breeding_Site_Recode, y = AvgLD)) +
  geom_point(aes(color = `Settlement Type`, size = 4.5), , alpha = 0.7) +  
  facet_wrap(~ `season`)+
  scale_color_manual(values = c(Formal = "#376C8B", Slum = "#FF5757"))+
  geom_text(aes(label = AvgLD), vjust = -1.2, hjust = 0.5, size = 3) + 
  geom_text(aes(label = Breeding_Site_Recode), vjust = -1.2, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Average Larval Density per Breeding Sites by settlement type ",
       y = "Average Larval Density",
       size = "Average Larval Density") +
  guides(size = FALSE)+
  theme_manuscript()

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Larval Density of Breeding sites by settlement.pdf'), Fig9, width = 8, height = 6) 


##Morphological classification Analysis
##Wet Season
lav_mol_df <- read_excel(file.path(LuPDir , "Molecular ID for larval mosquitoes.xlsx"))

lav_mol_df_rec <- lav_mol_df %>%
  group_by(`Location`, `Breeding site`) %>%
  summarise(count = n())

lav_mol_df_rec$Species <- "An_coluzii"

lav_mol_df_rec$season <- "Wet"

lav_mol_df_wet <- lav_mol_df_rec %>%
  mutate(settlement = case_when(
    Location == "Agugu" ~ "Slum",
    Location == "Challenge" ~ "Formal"
    ))

lav_mol_df_wet <- lav_mol_df_wet %>%
  mutate(`Breeding site` = case_when(
    `Breeding site` == "Puddle" ~ "Open Drain/Puddles",
    `Breeding site` == "Tyre" ~ "Tyre",
    `Breeding site` == "Plastic" ~ "Artificial containers",
    `Breeding site` == "Gutter" ~ "Drainage/Gutter/Dithces",
  ))

colnames(lav_mol_df_wet)[1] <- "Ward"


# Create dataframe for Dry Season
dt <- data.frame(
  Ward = c("Agugu", "Olopomeji", "Challenge", "Agugu", "Olopomeji", "Challenge"),
  Location = c("Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Drainage/Gutter/Dithces", "Tyre tracks", "Tyre tracks", "Tyre tracks"),
  An_gambiae_ss = c(8, 3, 0, 2, 0, 0),
  An_coluzzii = c(2, 0, 0, 0, 0, 0)
)

# Convert data to long format
data_long <- pivot_longer(dt, cols = c(An_gambiae_ss, An_coluzzii), names_to = "Species", values_to = "Value")

lav_mol_df_dry <- data_long[data_long$Value > 0, ]

lav_mol_df_dry$season <- "Dry"


lav_mol_df_dry <- lav_mol_df_dry %>%
  mutate(settlement = case_when(
    Ward == "Agugu" ~ "Slum",
    Ward == "Olopomeji" ~ "Formal"
  ))

colnames(lav_mol_df_dry)[2] <- "Breeding site"

colnames(lav_mol_df_dry)[4] <- "count"

#Combine Dry and Wet season
lav_mol_df_all <- rbind(lav_mol_df_dry, lav_mol_df_wet)

Fig11 <- ggplot(lav_mol_df_all, aes(x = "", y = count, size = count)) +
  geom_point(aes(color =  season, size = count), , alpha = 0.7) +  
  facet_wrap(~ `settlement`)+
  scale_color_manual(values = c(Dry = "#FFD4B5", Wet = "#008080"))+
  geom_text(aes(label = `Breeding site`), vjust = -1.2, hjust = 0.5, size = 3) + 
  scale_size_continuous(range = c(2, 10)) + 
  labs(title = "Distribution of anopheles reared to adulthood by specie and settlement",
       y = "Number of Adult Anopheles",
       size = "Number of Adult larva") +
  guides(size = FALSE)+
  theme_manuscript() 

ggsave(paste0(LuDir, '/plots/', Sys.Date(), 'Distribution of anopheles reared to adulthood by specie and settlement.pdf'), Fig11, width = 8, height = 6) 


