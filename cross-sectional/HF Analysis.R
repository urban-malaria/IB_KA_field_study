rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "urban_malaria")
NuCDir <- file.path(Drive, "my_stuff")
ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')

#=======
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  NuDPDir <- file.path(Drive, "Desktop")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EADat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "EA_data")
  EpiDir <- file.path(NuDir, "data", "nigeria", "kano_ibadan_epi", "Shiny data")
  IlDIr <- file.path(NuDir, "data", "nigeria", "ilorin_shapes_files")
  HFDir <- file.path(EpiDir, "Health_Facility")
  HHDir <- file.path(EpiDir, "Household")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}

##Health Facility

hf_data <- read.csv(file.path(NuDPDir, "HFS_2509.csv"))

# Select data for CSS Analysis

library(dplyr)

hf_data_sub1 <- hf_data %>%
  dplyr::select(q101..How.old.were.you.on.your.last.birthday.AGE.AT.LAST.BIRTHDAY..IN.YEARS., q503..RESULT, State,
                q309..Did.you.sleep.inside.a.mosquito.net.last.night.)

hf_data_sub1$Status <- "Pregnant"

table(hf_data_sub1$q503..RESULT)


hf_sum1 <- hf_data_sub1 %>% 
  group_by(`State`, `q503..RESULT`, `q309..Did.you.sleep.inside.a.mosquito.net.last.night.`) %>% 
  dplyr::summarise(count = n())


hf_sum1 <- hf_sum1 %>%
  mutate(q503..RESULT = case_when(
    q503..RESULT == "POSITIVE" ~ 1,
    q503..RESULT == "NEGATIVE" ~ 0,
    TRUE ~ 2  # For all other cases, including empty or blank values
  ))

# Create a factor with custom labels
hf_sum1$q503..RESULT <- factor(hf_sum1$q503..RESULT, 
                               levels = c(0, 1, 2), labels = c("NEG", "POS", "ND"))

library(dplyr)
library(tidyr)


# Remove rows where ColumnName is equal to "LabelToRemove"
hf_sum1 <- hf_sum1 %>%
  dplyr::filter(`q503..RESULT`!="ND")

hf_sum1 <- hf_sum1 %>%
  dplyr::filter(`q309..Did.you.sleep.inside.a.mosquito.net.last…` !="")

##Convert data to wide to enable computing proportion

wide_hf_sum <- hf_sum %>%
  tidyr::pivot_wider(names_from = q503..RESULT,
              values_from = count)

##Compute Proportions

wide_hf_sum$TOT <- wide_hf_sum$NEG + wide_hf_sum$POS

wide_hf_sum$PROP_POS <- round(wide_hf_sum$POS/wide_hf_sum$TOT * 100, 1)

wide_hf_sum$PROP_NEG <- round(wide_hf_sum$NEG/wide_hf_sum$TOT * 100, 1)

#Convert dataframe back to long to enable ploting stacked bar
wide_hf_sum_df <- wide_hf_sum %>%
  pivot_longer(cols = c(`PROP_POS`, `PROP_NEG`), 
               names_to = "RESULT",
               values_to = "count")

ggplot(hf_sum1, aes(fill=q503..RESULT, y=count, x=State)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c(`PROP_NEG` = "palegreen", `PROP_POS` = "coral3")) +
  geom_text(aes(x = State, y= count, label = count), vjust = 0.9, size = 3) +
  #facet_grid(~ Ward)+
  labs(title = "Results of RDTs among pregnant women by State")+
  theme_manuscript()+
  theme(legend.position = c(0.90, 0.85))+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))


##Odds Ratio Plots
Age_cat <- c("less than 20years", "20 - 24 years", "25-34 years", "35 years and above")
Age_cat_or <- c(2.1, 1.5,1.2,1)
Outside_Chores <- c("Yes", "No")
Out_cho_or <- c(1.2,1)

od_df <- data.frame(Age_cat, Age_cat_or, Outside_Chores, Out_cho_or)

hf_or <- read.csv(file.path(NuDPDir, "hf_or.csv"))

age <- head(hf_or, n=4)

age <- as.data.frame(age)

chor <- tail(hf_or, n=2)

library(ggplot2)
age <- age %>%
  as_tibble() %>%
  ggplot(aes(y=as.factor(Category), x=OR, label=as.factor(Category))) +
  geom_point(size=4, shape=19, color = "red") +
  geom_errorbarh(aes(xmin=LCI, xmax=UCI), height=.3) +
  coord_fixed(ratio=.15) +
  geom_vline(xintercept=1, linetype='longdash') +
  facet_wrap(~Variable, ncol=1)+
  theme_manuscript1()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Age and RDT_OR.pdf'), age, width = 8, height = 6)

chor <- chor %>%
  as_tibble() %>%
  ggplot(aes(y=Category, x=OR, label=Category)) +
  geom_point(size=4, shape=19, color = "red") +
  geom_errorbarh(aes(xmin=LCI, xmax=UCI), height=.3) +
  coord_fixed(ratio=.05) +
  geom_vline(xintercept=1, linetype='longdash') +
  facet_wrap(~Variable, ncol=1)+
  theme_manuscript1()+
  theme(strip.background = element_rect(fill = "khaki", color = "black"))

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'RDT RESULTS and Chores.pdf'), chor, width = 8, height = 6)

library(gridExtra)

od_plot <- grid.arrange(age, chor)


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Age chor and RDT_OR.pdf'), od_plot, width = 8, height = 6)


library(ggplot2)

# Create a custom theme
custom_theme <- theme(
  # Customize the background color
  plot.background = element_rect(fill = "lightgray"), 
  panel.background = element_rect(fill = "white"),
  
  # Customize strip (facet label) fill color
  strip.background = element_rect(fill = "lightblue"),
  
  # Customize point color
  panel.grid.major = element_line(color = "gray", size = 0.2),
  panel.grid.minor = element_line(color = "gray", size = 0.2),
  
  # Additional customizations as needed
)

# Apply the custom theme and customize colors
age %>%
  as_tibble() %>%
  ggplot(aes(y = Category, x = OR, label = Category)) +
  geom_point(size = 4, shape = 19, color = "red") +  # Change point color to red
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.3) +
  coord_fixed(ratio = 0.3) +
  geom_vline(xintercept = 1, linetype = 'longdash') +
  facet_wrap(~Variable, ncol = 1) +
  
  # Apply the custom theme
  custom_theme

dat <- data.frame(
  Index = c(1, 2, 3, 4, 5, 6), ## This provides an order to the data
  label = c("Age (less than 25 years versus >=25 years)", "Marital Status(Not in Union versus In Union)", "Slept under ITN last night(Yes versus No)",
            "Travel outside home(Yes versus No)", "Work(Yes versus No)", "Household chores(Outside versus Not outside"),
  OR = c(1.5, 2.2, 0.7, 1.1, 0.8, 1.2),
  LL = c(1.2, 1.4, 0.6, 0.9, 0.7, 1.0),
  UL = c(1.9, 3.3, 1.3, 1.4, 1.8, 1.6),
  CI = c("1.2, 1.9", "1.4, 3.3", "0.6, 1.3","0.9, 1.4","0.7, 1.8", "1.0, 1.6")
)


plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 1.8) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:6, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1

## Create the table-base pallete
table_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(OR, digits = 1))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")

library(gridExtra)

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
odp <- grid.arrange(plot1, tab1, tab2, layout_matrix = lay)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'ORplots .pdf'), odp, width = 8, height = 6)








hf_data_ib <- hf_data %>% dplyr::filter(State == "Ibadan (Oyo)")

table(hf_data$State)


names(hf_data)

hf_data_s <- hf_data_ib %>%
  group_by(`NAME.OF.HEALTH.FACILITY`) %>%
  summarise(total_pts = n()) %>%
  ungroup()

hf_data_sd <- hf_data_ib %>%
  group_by(`NAME.OF.HEALTH.FACILITY`, `Date`) %>%
  summarise(total_pts = n()) %>%
  ungroup()

hf_data_a <- hf_data_ib %>%
  group_by(`Area`) %>%
  summarise(total_pts = n()) %>%
  ungroup()


##Extracting dates and weeks
dates_as_date <- as.Date(hf_data_sd$Date, format = "%m/%d/%Y")

weeks <- format(dates_as_date, "%U")  # or "%W" for week starting on Monday

# Health_Facility_Name <- data.frame(Health_Facility_Name)
# 
# HF_Name <- c("Adeoyo Maternity Hospital", "Adeoyo Maternity Hospital", "Adeoyo Maternity Hospital",
#              "Jericho Specialist Hospital", "Naomi Medical Center", "Naomi Medical Center",
#              "Oranyan CHC", "Oranyan CHC", "Oranyan CHC", "Oranyan CHC")

# HF_Name <- data.frame(HF_Name)
# 
# Week <- c("Week 1", "Week 2", "Week 2", "Week 2", "Week 2", "Week 2", 
#           "Week 1","Week 1", "Week 2", "Week 2")

hf_data_sd <- cbind(hf_data_sd, weeks)

hf_data_sw <- hf_data_sd %>%
  group_by(`NAME.OF.HEALTH.FACILITY`, `weeks`) %>%
  summarise(total_pts = sum(total_pts)) %>%
  ungroup()

# targets <- c(9,9,9,23,23,23,23,23,23,23,13,13,13,13,13,6,6,6,6,6,6,6,3,3,8,8,8,8,
#              4,4,15,15,15,15,15,15,15,11,10,10)
# 
# 9, 23, 13, 6, 3, 8, 4, 15, 11, 10 

hf_data_sw <- cbind(hf_data_sw, targets)

hf_data_sw_ag <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "AGB")
hf_data_sw_a <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "AMH")
hf_data_sw_j <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "JSH")
hf_data_sw_n <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "NMC")
hf_data_sw_oad <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "OADU")
hf_data_sw_or <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "OP")
hf_data_sw_ol <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "OCH")
hf_data_sw_on <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "ONCHC")
hf_data_sw_u <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "UCH")
hf_data_sw_r <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "RRSH")


ag_p <- ggplot(data = hf_data_sw_ag, aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 9, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # facet_wrap(~NAME.OF.HEALTH.FACILITY)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Agbongbon PHC")

ad_p <- ggplot(data = hf_data_sw_a , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "green")+
  geom_hline(yintercept = 23, linetype = "dashed", color = "red")+
  #geom_text(data = hf_data_sw_a, aes(y = targets, label = targets), 
  #          hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Adeoyo Maternity")

je_p <- ggplot(data = hf_data_sw_j , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "blue")+
  geom_hline(yintercept = 13, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_j, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Jericho Specialist")

na_p <- ggplot(data = hf_data_sw_n , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "orange")+
  geom_hline(yintercept = 6, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_n, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Naomi Medical Center")

oad_p <- ggplot(data = hf_data_sw_oad , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "maroon3")+
  geom_hline(yintercept = 3, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_ol, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Oke Adu PHC")

or_p <- ggplot(data = hf_data_sw_or , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "tan4")+
  geom_hline(yintercept = 15, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_or, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Oranmiyan CHC")

ol_p <- ggplot(data = hf_data_sw_ol , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "orangered")+
  geom_hline(yintercept = 8, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_ol, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Oluyoro Catholic Hospital")

on_p <- ggplot(data = hf_data_sw_on , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "gold2")+
  geom_hline(yintercept = 4, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_ol, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Oniyanrin Comprehensive HC")

u_p <- ggplot(data = hf_data_sw_u , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "yellow1")+
  geom_hline(yintercept = 10, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_u, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in University College Hospital")

r_p <- ggplot(data = hf_data_sw_r , aes(x = weeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "cyan")+
  geom_hline(yintercept = 11, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_ol, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Ring Road Specialist Hospital")

library(gridExtra)

hf_plots <- grid.arrange(ad_p, or_p, je_p, na_p, on_p, r_p, ag_p,
                         oad_p, u_p)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen as at August 29 .pdf'), hf_plots, width = 8, height = 6)


# hf_data_coords <- hf_data %>% dplyr::select(Longitude, Latitude)
#Location of health facilities 
hf_data_coord <- rbind(amh_coord, jsh_coord, nmc_coord,op_coord)

colnames(hf_data_coord)[1] <- "Latitude"
colnames(hf_data_coord)[2] <- "Longitude"

hf_data_df <- cbind(hf_data_s, hf_data_coord, Health_Facility_Name)

hf_data_dff <- sf::st_as_sf(hf_data_df, coords=c('Latitude', 'Longitude'), crs=4326)

p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = hf_data_dff, aes(geometry = geometry, size = total_pts, col = Health_Facility_Name), stat = "sf_coordinates") +
  # scale_size_manual(values = c(`(0,5]` = 1, `(6,9]` = 2, `(10,14]` = 3, `(15,19]` = 4, `(20,24]` = 5)) +
  # scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, 100))+
  geom_text(data = hf_data_dff,
            aes(label= total_pts, geometry = geometry), color = 'black',
            stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE)+
  geom_text_repel(
    data = df_ib,
    aes(label = WardName, geometry = geometry), color = 'black',
    stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE) +
  map_theme()+
  ylab("") +
  xlab("") +
  labs(title = "Location of Health Facilities visited in Ibadan and number of patients seen as at July 5, 2023") +
  coord_sf()


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen July 5.pdf'), p, width = 8, height = 6)


p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = hf_data_dff, aes(geometry = geometry, size = 8.5, col = Health_Facility_Name), stat = "sf_coordinates") +
  # scale_size_manual(values = c(`(0,5]` = 1, `(6,9]` = 2, `(10,14]` = 3, `(15,19]` = 4, `(20,24]` = 5)) +
  # scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, 100))+
  geom_text(data = hf_data_dff,
            aes(label= total_pts, geometry = geometry), color = 'black',
            stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE)+
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label = WardName, geometry = geometry), color = 'black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf) +
  # guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Location of Health Facilities visited in Ibadan and number of patients seen as at July 7, 2023") +
  coord_sf()


#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen July 5.pdf'), p, width = 8, height = 6)


# ggplot(data = hf_data_sd , aes(x = Date, y = as.numeric(total_pts))) +
#   geom_point(size = 2.0, alpha = 0.7, col = "blue")+
#   geom_line()+
#   facet_wrap(~NAME.OF.HEALTH.FACILITY)+
#   theme_manuscript1()+
#   ylab("Number of Patients")+
#   xlab("Date of Visit")+
#   labs(title= "Number of patients seen per day")


##Kano HF Analysis

hf_data_kn <- hf_data %>% dplyr::filter(Location == "Kano")

view(hf_data)
names(hf_data)

khf_data_s <- hf_data_kn %>%
  group_by(`NAME.OF.HEALTH.FACILITY`) %>%
  summarise(total_pts = n()) %>%
  ungroup()

khf_data_sd <- hf_data_kn %>%
  group_by(`NAME.OF.HEALTH.FACILITY`, `Date`) %>%
  summarise(total_pts = n()) %>%
  ungroup()

khf_data_a <- hf_data_kn %>%
  group_by(`Area`) %>%
  summarise(total_pts = n()) %>%
  ungroup()


kdates_as_date <- as.Date(khf_data_sd$Date, format = "%m/%d/%Y")

kweeks <- format(kdates_as_date, "%U")  # or "%W" for week starting on Monday

khf_data_sd <- cbind(khf_data_sd, kweeks)

khf_data_sw <- khf_data_sd %>%
  group_by(`NAME.OF.HEALTH.FACILITY`, `kweeks`) %>%
  summarise(total_pts = sum(total_pts)) %>%
  ungroup()


khf_data_sw_d <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "DALA")
khf_data_sw_g <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "GWG")
khf_data_sw_k <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "KBG")
khf_data_sw_rl <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "RLEMO")
khf_data_sw_s <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "SHR")
khf_data_sw_un <- khf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "UNG")

# hf_data_sw_ol <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "OCH")
# hf_data_sw_on <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "ONCHC")
# hf_data_sw_u <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "UCH")
# hf_data_sw_r <- hf_data_sw%>% dplyr::filter(NAME.OF.HEALTH.FACILITY == "RRSH")


dal_p <- ggplot(data = khf_data_sw_d, aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "tomato1") +
  geom_hline(yintercept = 34, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # facet_wrap(~NAME.OF.HEALTH.FACILITY)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in DALA MHC")

gw_p <- ggplot(data = khf_data_sw_g , aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "green")+
  geom_hline(yintercept = 42, linetype = "dashed", color = "red")+
  #geom_text(data = hf_data_sw_a, aes(y = targets, label = targets), 
  #          hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Gwagwarwa PHC")

kb_p <- ggplot(data = khf_data_sw_k , aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "blue")+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_j, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Kabuga PHC")

rl_p <- ggplot(data = khf_data_sw_rl , aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "orange")+
  geom_hline(yintercept = 29, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_n, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Rijar Lemo Center")

sh_p <- ggplot(data = khf_data_sw_s , aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "maroon3")+
  geom_hline(yintercept = 25, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_ol, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  # #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Sharada PHC")

un_p <- ggplot(data = khf_data_sw_un , aes(x = kweeks, y = as.numeric(total_pts))) +
  geom_point(size = 4.0, col = "tan4")+
  geom_hline(yintercept = 24, linetype = "dashed", color = "red")+
  # geom_text(data = hf_data_sw_or, aes(y = targets, label = targets), 
  #           hjust = -0.1, vjust = 1, color = "black")+
  #facet_wrap(~ HF_Name)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Week of Visit")+
  labs(title= "Number of patients seen per Week in Unguwa Uku PHC")

library(gridExtra)

khf_plots <- grid.arrange(dal_p, gw_p, kb_p, rl_p, sh_p, un_p)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen as at August 21 .pdf'), hf_plots, width = 8, height = 6)


# # hf_data_coords <- hf_data %>% dplyr::select(Longitude, Latitude)
# # 
# hf_data_coord <- rbind(amh_coord, jsh_coord, nmc_coord,op_coord)
# 
# colnames(hf_data_coord)[1] <- "Latitude"
# colnames(hf_data_coord)[2] <- "Longitude"
# 
# hf_data_df <- cbind(hf_data_s, hf_data_coord, Health_Facility_Name)
# 
# hf_data_dff <- sf::st_as_sf(hf_data_df, coords=c('Latitude', 'Longitude'), crs=4326)
# 
# p <- ggplot(df_ib) +
#   geom_sf(fill = NA) +
#   geom_point(data = hf_data_dff, aes(geometry = geometry, size = total_pts, col = Health_Facility_Name), stat = "sf_coordinates") +
#   # scale_size_manual(values = c(`(0,5]` = 1, `(6,9]` = 2, `(10,14]` = 3, `(15,19]` = 4, `(20,24]` = 5)) +
#   # scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, 100))+
#   geom_text(data = hf_data_dff,
#             aes(label= total_pts, geometry = geometry), color = 'black',
#             stat = "sf_coordinates", min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf) +
#   guides(alpha = FALSE, size = FALSE)+
#   geom_text_repel(
#     data = df_ib,
#     aes(label = WardName, geometry = geometry), color = 'black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf) +
#   guides(alpha = FALSE, size = FALSE) +
#   map_theme()+
#   ylab("") +
#   xlab("") +
#   labs(title = "Location of Health Facilities visited in Ibadan and number of patients seen as at July 5, 2023") +
#   coord_sf()


#ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen July 5.pdf'), p, width = 8, height = 6)






## Other General Analysis





p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  geom_point(data = hf_data_dff, aes(geometry = geometry, size = 8.5, col = Health_Facility_Name), stat = "sf_coordinates") +
  # scale_size_manual(values = c(`(0,5]` = 1, `(6,9]` = 2, `(10,14]` = 3, `(15,19]` = 4, `(20,24]` = 5)) +
  # scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, 100))+
  geom_text(data = hf_data_dff,
            aes(label= total_pts, geometry = geometry), color = 'black',
            stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf) +
  guides(alpha = FALSE, size = FALSE)+
  # geom_text_repel(
  #   data = df_ib,
  #   aes(label = WardName, geometry = geometry), color = 'black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf) +
  # guides(alpha = FALSE, size = FALSE) +
  map_theme() +
  ylab("") +
  xlab("") +
  labs(title = "Location of Health Facilities visited in Ibadan and number of patients seen as at July 7, 2023") +
  coord_sf()


ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'Health Facility and patient seen July 5.pdf'), p, width = 8, height = 6)


ggplot(data = hf_data_sd , aes(x = Date, y = as.numeric(total_pts))) +
  geom_point(size = 2.0, alpha = 0.7, col = "blue")+
  geom_line()+
  facet_wrap(~NAME.OF.HEALTH.FACILITY)+
  theme_manuscript1()+
  ylab("Number of Patients")+
  xlab("Date of Visit")+
  labs(title= "Number of patients seen per day")


##Positivity

post_rate <- table(hf_data$NAME.OF.HEALTH.FACILITY, hf_data$q503..RESULT)

hf_data$q503..RESULT <- ifelse(hf_data$q503..RESULT == "", 0, hf_data$q503..RESULT)

post1_rate <- table(hf_data$q503..RESULT)

coded_post <- ifelse(hf_data$q503..RESULT == "NEGATIVE", 0, 1)

hf_data_p <- cbind(hf_data, coded_post)

proportion_positive <- mean(coded_post == 1)

chi_square_result <- chisq.test(state_p)

# Build the logistic regression model
model <- glm(coded_post ~ State + q103..What.is.the.highest.level.of.formal.school.you.completed., 
             data = hf_data_p, 
             family = binomial(link = "logit"))

library(stargazer)

summary_table <- stargazer(model, type = "text")

cat(summary_table)

summary(model)

#ppost_rate <- data.frame(post1_rate)

HF <- c("AMH", "JSH", "NMC", "OCH", "OP", "Total")
NEG <- c(198)
POS <- c(35)

postivity_rate <- data.frame(NEG,POS)

#postivity_rate$Total <- post1_rate$NEGATIVE + post1_rate$POSITIVE

Total_Test <- rowSums(postivity_rate[, c("NEG", "POS")])

post_df <- cbind(postivity_rate, Total_Test)

post_df$prev <- post_df$POS/post_df$Total_Test * 100

post_df$tpR <- post_df$POS/post_df$Total_Test

print(post_df)

##Positve Pregnant Women Location
install.packages(ggmap)
library(ggmap)

library(ggmap)

register_google(key = "AIzaSyAgHZuBbhQkHdDQe7l0liTJQlhI7GEIUsc")

hf_pp <- read.csv(file.path(NuDPDir, "pp_hf_test.csv"))

coordinates <- geocode(hf_pp$WARD)

view(coordinates)

##Location of pregnant women..................................

# ppg_list <- st_read(file.path(NuDPDir, "Health Facility Survey.kml"))
# 
# ggplot(df_ib) +
#   geom_sf(fill= NA)+
#   geom_point(data = ppg_list,  aes(geometry = geometry, size = 0.2, alpha = 0.7, col = "red"), stat= "sf_coordinates")+
#   #scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
#   geom_text_repel(
#     data = df_ib,
#     aes(label =  WardName, geometry = geometry),color ='black',
#     stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1, max.overlaps = Inf)+
#   guides(alpha = FALSE, size = FALSE) +
#   map_theme()+ 
#   ylab("")+
#   xlab("")+
#   labs(title= "Wards in Ibadan showing where positive pregnant women are located")+
#   coord_sf()



##Location of pregnant women interviewed
ib_hf_dt <- read.csv(file.path(NuDPDir, "ibadan_address_classification.csv"))

ib_hf_dt <- ib_hf_dt %>% dplyr::filter(Appropriate == "Yes")

ib_hf_df <- ib_hf_dt %>%
  group_by(`WARD`)%>%
  dplyr::summarise(count = n()) 

df_hf <- df_ib%>% 
  left_join(ib_hf_df, by = c("WardName" = "WARD")) 


# Define breakpoints and labels for the groups
breaks <- c(0, 9, 19, 29, 39, 49, 59, 69, Inf)  # Define the breakpoints for the groups
labels <- c("0-9", "10-19", "20-29", "30-39", "40-49","50-59", "60-69", "70+")

df_hf <- df_hf %>% 
  mutate(class = cut(count, breaks = breaks, labels = labels)) 
  
st_crs(df_hf) <- 4326

discrete_palettes <- list(rev(RColorBrewer::brewer.pal(8, "RdYlBu")))

ibhf_plot <- ggplot() +
  geom_sf(data = df_hf, aes(fill = class)) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = bas, mapping = aes(x = Latitude, y = Longitude), colour = "blue", size = 2.0) +
  geom_text_repel(
    data = df_hf,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_discrete(drop=FALSE, name="class", type = discrete_palettes)+
  labs(title= "Wards in Ibadan Metro Area showing where pregnant women come from ")+
  map_theme()+
  coord_sf()




## read kano ward shape files
df_ko = st_read(file.path(shapepath, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

hf_kn_add <- read.csv(file.path(NuDPDir, "kn_hf_add.csv"))

hf_kn_add1 <- hf_kn_add %>%
  select(Health_Facility_Name,Longitude, Latitude, q503..RESULT)

kn_add_df <- sf::st_as_sf(hf_kn_add1, coords=c('Longitude', 'Latitude'), crs=4326)

hf_kn_add_in <- st_intersection(kn_add_df, df_ko)


st_crs(kn_add_df) <- 4326
st_crs(df_ko) <- 4326

p <- ggplot(df_ko) +
  geom_sf(fill = "khaki") +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  #geom_point(data = kn_add_df,  aes(geometry = geometry, size = 0.1, alpha = 0.2, col = as.factor(q503..RESULT), stat= "sf_coordinates"))+
  geom_point(data = kn_add_df, mapping = aes(x = Longitude, y = Latitude),colour = "red", size = 0.5) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano showing where pregnant women live")+
  coord_sf()

library(ggplot2)
library(sf)

# Assuming you have loaded your data into a dataframe called 'df' and 'sf' object called 'shapefile'

# Create a ggplot object
ggplot() +
  # Plot the shapefile using geom_sf
  geom_sf(data = df_ko, fill = "khaki") +
  # Plot the points from the dataframe using geom_point
  geom_point(data = kn_add_df, aes(x = geometry), color = "red", size = 2) +
  labs(title = "Points on Shapefile Map") +
  coord_sf() +
  theme_minimal()


library(ggplot2)
library(ggrepel)

library(ggplot2)
library(sf)

# Load the shapefile (replace 'shapefile.shp' with the path to your shapefile)
shapefile <- st_read("path/to/your/shapefile.shp")

# Create a ggplot object with your points dataframe
ggplot() +
  geom_sf(data = df_ko, fill = "khaki") +  # Plot the shapefile
  geom_sf(data = hf_kn_add_in, aes(color = q503..RESULT), size = 2) +  # Plot the points
  scale_color_manual(values = c("NEGATIVE" = "green", "POSITIVE" = "red")) +  # Define point colors
  theme_manuscript1()+
  labs(title = "Location of pregnant women")  # Set the title

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_kano2.png'), p, width = 8, height = 6)






##Health Facility Pretest##-------------------------------------------
Day <- c("20/06/2023", "20/06/2023", "20/06/2023", "21/06/2023", "22/06/2023", "22/06/2023")
HF <- c("Sango", "Akorede", "Bashorun", "Agbongbon", "Agbowo", "Idi Ogungun" )
freq <- c(3,2,1,8,4,2)

hf_pretest <- data.frame(Day, HF, freq)

ggplot(hf_pretest, aes(fill= HF, 
                       y=freq, 
                       x=HF)) + 
  geom_bar(position='stack', stat='identity')+ 
  facet_wrap(~ Day)+
  labs(x = "Name of Health Facility", fill = "Health Facility",
       y = "No. of women interviwed")+
  theme(legend.position = "right", legend.background = element_blank(),
        panel.grid.major = element_blank())+ 
  theme_manuscript22()
