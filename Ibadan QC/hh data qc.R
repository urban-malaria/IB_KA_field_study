library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lubridate)
install.packages("anytime")
library(anytime)
install.packages("formattable")
library(formattable)
install.packages("stringdist")
library(stringdist)


df<-read_csv('ib_hh_data_2911.csv')



##Cleaning up names of data collectors
# Create an empty mapping data structure
manual_mapping <- list(
  "/LOLA" = "LOLA",
  "ADEMOLA"="ADEMOLA",
  "ADEWALE" = "ADEWALE",
  "ADEWALE ADEBLOLA" = "ADEWALE",
  "ADEWALE ADEBOLA" = "ADEWALE",
  "ADEWALEADEBOLA" = "ADEWALE",
  "ADEYEMO COMFORT ADERONKE" = "ADEYEMO COMFORT ADERONKE",
  "AKINDELE" = "AKINDELE",
  "AKINDELE AYOMIDELE" = "AKINDELE",
  "AKINTUNDE" = "AKINTUNDE",
  "ALADE SOLOMON" = "ALADE SOLOMON",
  "AMINAT" = "AMINAT",
  "ANIBIJUWON OLUWATIMILEHIN" = "ANIBIJUWON OLUWATIMILEHIN",
  "AYINDE FOLASHADE" = "FOLASHADE",
  "AYINDE FOLASHADE ADEOLA" = "FOLASHADE",
  "AYODELE" = "AYODELE",
  "DOLAPO" = "DOLAPO",
  "EMEKA" = "Emeka",
  "EMMANUEL" = "EMMANUEL",
  "FAWOLE DAVID" = "FAWOLE DAVID",
  "FOLASHADE" = "FOLASHADE",
  "FRANCIS" = "FRANCIS",
  "FUNMITO ABIOLA" = "FUNMITO ABIOLA",
  "GBEMISOLA" = "GBEMISOLA",
  "GLORY" = "GLORY",
  "KEHINDE" = "KEHINDE",
  "KEJI" = "KEJI",
  "LOLA" = "LOLA",
  "MICHAEL" = "MICHAEL",
  "MISOLA" = "MISOLA",
  "MORENIKEJI" = "MORENIKEJI",
  "MRS ADEBAYO" = "MRS ADEBAYO",
  "OLAOLU" = "OLAOLU",
  "OLUWABUNMI" = "OLUWABUNMI",
  "OLUWABUNMI AREMO" = "OLUWABUNMI",
  "OLUWAFEMI" = "OLUWAFEMI",
  "OLUWAFEMI AKINSETE" = "OLUWAFEMI",
  "OLUWAPELUMI" = "OLUWAPELUMI",
  "OLUWATOSIN" = "OLUWATOSIN",
  "OPEMIPO" = "OPEMIPO",
  "OYEKANMI" = "OYEKANMI",
  "OYEKANMI VICTORIA" = "OYEKANMI",
  "Samuel" = "SAMUEL",
  "SAMUEL" = "SAMUEL",
  "TEMIDAYO" = "TEMIDAYO",
  "TOBILOBA" = "TOBILOBA",
  "TOMISIN" = "TOSIN",
  "TOSIN" = "TOSIN",
  "UDEME" = "UDEME",
  "ZAINAB" = "ZAINAB"
  
)

# Replace names in the original dataframe using the manual mapping
for (i in seq_along(df$`INTERVIEWER'S NAME`)) {
  name <- df$`INTERVIEWER'S NAME`[i]
  if (!is.na(name) && name %in% names(manual_mapping)) {
    df$`INTERVIEWER'S NAME`[i] <- manual_mapping[[name]]
  }
}






# see the 'INTERVIEWER'S NAME' 
unique_names <- unique(df$`INTERVIEWER'S NAME`)
unique_names <- sort(unique_names)  # Sort names alphabetically
# Displaying unique names alphabetically
print(unique_names)



#prefill empty cells
# Replace empty strings with NA
df[df == ""] <- NA

# Columns to prefill
columns_to_prefill <- c(
  "LOCAL GOVT. AREA", "Ward", "Settlement Type", 
  "Community Name", "Enumeration Area/Cluster Number", 
  "Household Number", "HOUSEHOLD COORDINATE- Longitude", 
  "HOUSEHOLD COORDINATE- Latitude", "Name of Household Head", 
  "Date", "INTERVIEWER'S NAME", "INTERVIEWERS PHONE NO", "NTERVIEWER VISIT 1 result",	
  "Visit 1 Date",	"NTERVIEWER VISIT 2 result",	"Visit 2 Date",
  "NTERVIEWER VISIT 3 result",	"Visit 3 Date",	"SUPERVISORS NAME",	"FIELD EDITOR"

)

# Group by Serial Number and fill missing values within specified columns for each group
df_filled <- df %>%
  group_by(`Serial Number`) %>%
  fill(!!!syms(columns_to_prefill)) %>%
  ungroup()


# Replacing Ologuneru with Olopomewa
df_filled <- df_filled %>%
  mutate(Ward = ifelse(Ward == "Ologuneru", "Olopomewa", Ward))











#Find mltiple LGAs entry for unique wards per LGA
# Find unique combinations of Name, Ward, and Local Govt. Area
name_ward_lga <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`, Ward, `LOCAL GOVT. AREA`) %>%
  summarise() %>%
  distinct() %>%
  ungroup()

# Count unique local government areas per name
name_lga_count <- name_ward_lga %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(num_lga = n_distinct(`LOCAL GOVT. AREA`)) %>%
  filter(num_lga > 1)


# Save the dataframe as a CSV file
write.csv(name_lga_count, "name_lga_count2.csv", row.names = FALSE)




#Number of daily interviews
daily_interviews <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`, Date) %>%
  summarise(num_interviews = n()) %>%
  ungroup() %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(avg_interviews_per_day = mean(num_interviews))

# Display the average number of interviews conducted daily by each interviewer
view(daily_interviews)

# Save the dataframe as a CSV file
write.csv(daily_interviews, "daily_interviews1.csv", row.names = FALSE)








#total interviews conducted by each RA
assistant_interviews <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(total_interviews = n_distinct(`Serial Number`))

# Display the total number of interviews done by each Research Assistant
view(assistant_interviews)

# Save the dataframe as a CSV file
write.csv(assistant_interviews, "assistant_interviews1.csv", row.names = FALSE)








#Monthly interviews conducted
# Assuming 'Date' is the column you want to convert in the 'df_filled' dataframe
df_filled$Date <- as.Date(df_filled$Date, format = "%m/%d/%Y")


# Group by INTERVIEWER'S NAME and month, count interviews
monthly_interviews <- df_filled %>%
  group_by(`INTERVIEWER'S NAME`, Month = format(Date, "%B")) %>%
  summarise(total_interviews = n_distinct(`Serial Number`))

view(monthly_interviews)

# Save the dataframe as a CSV file
write.csv(monthly_interviews, "monthly_interviews1.csv", row.names = FALSE)














#Number of children(10 years and under) by Ward and Settlement Type

#Summary Children 0 to 10 years----
summary_Children <-  df_filled   %>%
  filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...29` <= 10 ) %>%
  group_by(`Settlement Type`, Ward) %>%
  summarise(
    Total_Count = n(),
  ) |>
  select( Ward,`Settlement Type`, Total_Count )


summary_Children1 <-  df_filled   %>%
  filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...29` <= 10 )
view(summary_Children1)



# Filter out 'Formal' settlements from 'Agugu' ward
filtered_summary <- summary_Children %>%
  filter(!(Ward == "Agugu" & `Settlement Type` == "Formal"))

# Save the dataframe as a CSV file
write.csv(summary_Children1, "summary_children1.csv", row.names = FALSE)

# Plotting
plot <- ggplot(filtered_summary, aes(x = Ward, y = Total_Count, fill = `Settlement Type`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total_Count), vjust = -0.5, position = position_dodge(width = 0.9)) +  # Adding count labels
  labs(title = "No. of Children by Ward and Settlement Type", x = "Ward", y = "No. of Children") +
  theme_minimal()

# Show the plot
print(plot)













# Filtering based on enumeration starting with numbers and extracting corresponding names
names_starting_with_numbers <- df_filled[grepl("^\\d", df_filled$'Enumeration Area/Cluster Number'), "INTERVIEWER'S NAME"]

# Getting unique names
unique_names <- unique(names_starting_with_numbers)

# Counting occurrences of each unique name
name_counts <- table(names_starting_with_numbers)

# Creating a dataframe with unique names and their counts
unique_names_counts <- data.frame(INTERVIEWER'S NAME = names(unique_names), Count = as.vector(name_counts))

View(unique_names_counts)















                                  





























