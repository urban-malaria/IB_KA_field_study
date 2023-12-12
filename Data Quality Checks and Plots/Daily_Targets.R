library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
df1<-read_csv('/Users/user/Downloads/kn_hh_data_3011.csv')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)



df_filled3 <- df1 %>%
  group_by(`Serial Number`) %>%
  fill(2:7, 20:26)

View(df_filled3)


mydata <-table(df_filled3$`If others, specify` )

View(mydata)


# Check for missing values in each column ------
column_missingkn <- colSums(is.na(checks_na))
print("Missing values in each column:")
View(column_missingkn)

file_path <- "/Users/user/Downloads/missing_columns_kn_1.csv"

# Export the data frame to a CSV file
write.csv(column_missingkn, file = file_path, row.names = TRUE)


# Check for missing values by individual questions ------
missing_lga<- df_filled3 %>%
  filter(is.na(`LOCAL GOVT. AREA`)) |>
  select(1,24,25,27)
View(missing_lga)


missing_ward<- df_filled3 %>%
  filter(is.na(`Ward`)) |>
  select(1,24,25,27)
View(missing_ward)

missing_settlement<- df_filled3 %>%
  filter(is.na(`Settlement Type`)) |>
  select(1,24,25,27)
View(missing_settlement)


missing_comm<- df_filled3 %>%
  filter(is.na(`Community Name`)) |>
  select(1,24,25,27)
View(missing_comm)

missing_ea<- df_filled3 %>%
  filter(is.na(`Enumeration Area/Cluster Number`)) |>
  select(1,24,25,27)
View(missing_ea)




#Summary Children 0 to 10 years----

summary_Children <-  df_filled3   %>%
 filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...12` <= 10 ) %>%
  group_by(`Settlement Type`, Ward) %>%
  summarise(
    Total_Count = n(),
  ) |>
  select( Ward,`Settlement Type`, Total_Count )

View(summary_Children)


#Data of children 0 - 10
summary_Children_0_to_10 <-  df_filled3   %>%
  filter (`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) %>%
  filter(`Age: How old was   (NAME) as at last birthday?...12` <= 10 )
View(summary_Children_0_to_10)


dateplot<-ggplot(summary_Children, aes(reorder(`Ward`, -Total_Count ),pmax(Total_Count,30), fill = `Settlement Type`)) +
 # geom_line()
 geom_bar(stat = "identity")+
geom_text(aes(label = paste( `Settlement Type`, ": ", signif(Total_Count)))  , position = position_stack(vjust = 0.3),
          vjust = -0.3 ) +

 # geom_text(aes(label = Ward ), position = position_stack(vjust = 0.6),
    #        vjust = -0.6 ) +
 # coord_cartesian(ylim = c(0, max(summary_Children$Total_Count) + 1)) +
  labs(title = "Household Survey - Cross Sectional",
    subtitle = "Plot of Children who are 0 to 10 by Ward and Settlement type",
    caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Count by Ward", x = "") #+ theme(axis.text.x = element_text(angle = 90))
dateplot





#Summary by DATE by Enumerator----

summary_Date_RA <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, `INTERVIEWER'S NAME` , Total_Completed_by_DATE )

View(summary_Date_RA)


dateplot<-ggplot(summary_Date_RA,
                 aes(reorder(`Visit 1 Date`, -Total_Completed_by_DATE ),Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Health Facility Survey",
    # subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "") + theme(axis.text.x = element_text(angle = 90))
dateplot


#Summary table for Completion by Date by Interviewer ----
table_result <- summary_Date_RA  %>%
  spread(`Visit 1 Date`, Total_Completed_by_DATE)
View(table_result)




#Summary by DATE by Enumerator by Target----

summary_Date_RA_target <-  summary_Date_RA  %>%
 # filter (Total_Completed_by_DATE < 3  ) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    Days_worked = n(),
    Days_target_not_met = sum(Total_Completed_by_DATE < 3 )
  ) #|>
 # select(`INTERVIEWER'S NAME` , Days_target_not_met_less_than_3 )
summary_Date_RA_target$Number_of_Work_Days <- 75
View(summary_Date_RA_target)



breaks <- c(3, 5)
colors <- c("green","red3")

dateplot<-ggplot(summary_Date_RA_target ,
                 aes(x=reorder(`INTERVIEWER'S NAME`, -Days_worked  ), y = Days_worked, Fill = Days_target_not_met) ) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Days_target_not_met)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Health Facility Survey",
    # subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "") + theme(axis.text.x = element_text(angle = 90)) + scale_fill_manual(values = colors)
dateplot





#Summary by DATE----

summary_Date <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date` , Total_Completed_by_DATE )
summary_Date$MonthNum <- gsub("/","" ,substr(summary_Date$`Visit 1 Date`, 1, 2))
View(summary_Date)

#Summary by DATE and Enumerators----
dateplot<-ggplot(summary_Date, aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , group = MonthNum, color = Month)+
  geom_bar(stat = "identity", fill="#661133") +
  facet_wrap(~MonthNum, scales = "free_x") +
  #geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Cross Sectional Survey",
    # subtitle = "Plot of Achievement by Date",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Date", x = "") + theme(axis.text.x = element_text(angle = 90))
dateplot



#Summary by Dates All Months----
summary_Date_ <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ) ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`)),
    InterviewCount = sum(nrow(`INTERVIEWER'S NAME`))
  ) |>
  select( `Visit 1 Date`, `INTERVIEWER'S NAME`, Total_Completed_by_DATE )
View(summary_Date_)

summary_Date_$MonthNum <- gsub("/","" ,substr(summary_Date_$`Visit 1 Date`, 1, 2))

summary_Month_ <-  summary_Date_  %>%
group_by(MonthNum, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)

  )

View(summary_Month_)



#Summary table for Completion by Month by Interviewer ----
table_result_month <- summary_Month_  %>%
  spread(MonthNum, Total_Completed_in_Month)
View(table_result_month)



#Plot by DATE All Month----
dateplot<-ggplot(summary_Month_,
                 aes(reorder(MonthNum,- Total_Completed_in_Month),Total_Completed_in_Month), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_in_Month)), nudge_y = 0, vjust = -0.5) +
 # geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Cross Sectional Survey",
   subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "")
dateplot

#Summary by September With Dates----

summary_Date_Sep <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ), str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Date_Sep)


#Plot by DATE All Month----
dateplot<-ggplot(summary_Date_Sep,
                 aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
#  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(title = "Plots of Achievement by Date",
     subtitle = "September 2023",
    caption = "Data source : Cross Sectional Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript()  +labs(y= "Total Completed by Date", x = "Date of Interviews") #+ theme(axis.text.x = element_text(angle = 90))
dateplot




#Summary of HH Total by Those Tested with Interviewer name----

summary_hh <-  df_filled3  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ) ) %>%
  group_by(`Serial Number`, `Name of Household Head`,`INTERVIEWER'S NAME`) %>%
  summarise(
   Num_HH = sum(!is.na(`Relationship to Household   Head (HH): What is the relationship of (NAME) to the HH`)),
   Num_DBS = sum(!is.na(`q303: DRIED BLOOD SAMPLE COLLECTED`)),
  # Interviewer = `INTERVIEWER'S NAME`
  )



View(summary_hh)

file_path <- "/Users/user/Downloads/summary_by_hh.csv"

# Export the data frame to a CSV file
write.csv(summary_hh, file = file_path, row.names = TRUE)


#Summary by Dates Sept----

summary_Sep_Date_ <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )

View(summary_Sep_Date_)

#Summary by September With Dates----

summary_Sep_Date <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, `INTERVIEWER'S NAME` , Total_Completed_by_DATE )

View(summary_Sep_Date)


#Summary by September with Interviewer name----

summary_Sep <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    Completed_in_September = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`INTERVIEWER'S NAME` , Completed_in_September )

View(summary_Sep)



#Summary by Enumeration Area/Cluster Number with Interviewer name----

summary_Sep <-  df_filled3  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ),str_detect(df1$`Visit 1 Date`, "^9") ) %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  summarise(
    Completed_in_September = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`INTERVIEWER'S NAME` , Completed_in_September )

View(summary_Sep)



rows_with_keyword <- which(grepl("/", df_filled3$`Enumeration Area/Cluster`, ignore.case = TRUE))

# Print the result
if (length(rows_with_keyword) > 0) {
  cat("Keyword found in rows:", rows_with_keyword, "\n")
} else {
  cat("Keyword not found in any row.\n")
}


#Summary by DATE and Enumerators----
dateplot<-ggplot(filtered_df,
                 aes(reorder(`Visit 1 Date`, -Total_Completed_by_DATE ),Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`Visit 1 Date` ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Cross Sectional Survey",
    # subtitle = "Plot of Achievement by Date",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Date", x = "")
dateplot



library(openxlsx)

# Create a new Excel workbook
wb <- createWorkbook()

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Summary Date RAs")
writeData(wb, sheet = "Summary Date RAs", x = table_result , row.names = TRUE)

addWorksheet(wb, sheetName = "Summary by Target 3+ Perday")
writeData(wb, sheet = "Summary by Target 3+ Perday", x = summary_Date_RA_target, row.names = TRUE)

addWorksheet(wb, sheetName = "Summary by Date by RAs")
writeData(wb, sheet = "Summary by Date by RAs", x = summary_Date_, row.names = TRUE)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Summary by Month by RAs")
writeData(wb, sheet = "Summary by Month by RAs", x = table_result_month, row.names = TRUE)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Whole Data")
writeData(wb, sheet = "Whole Data", x = df_filled3, row.names = TRUE)


# Add dataframes to different sheets
addWorksheet(wb, sheetName = "summary_Children_0_to_10")
writeData(wb, sheet = "summary_Children_0_to_10", x = summary_Children_0_to_10, row.names = TRUE)


# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Missing Columns")
writeData(wb, sheet = "Missing Columns", x = column_missingkn , row.names = TRUE)

addWorksheet(wb, sheetName = "Missing Columns")
writeData(wb, sheet = "Missing Columns", x = column_missing, row.names = TRUE)

addWorksheet(wb, sheetName = "Missing LGA Records")
writeData(wb, sheet = "Missing LGA Records", x = missing_lga)

# Add dataframes to different sheets
addWorksheet(wb, sheetName = "Missing Wards")
writeData(wb, sheet = "Missing Wards", x = missing_ward)

addWorksheet(wb, sheetName = "Missing Settlement Type")
writeData(wb, sheet = "Missing Settlement Type", x = missing_settlement)

addWorksheet(wb, sheetName = "Missing Community Name")
writeData(wb, sheet = "Missing Community Name", x = missing_comm)

addWorksheet(wb, sheetName = "Missing EA ")
writeData(wb, sheet = "Missing EA , x = missing_ea")


# Save the workbook to a file
saveWorkbook(wb, file = "/Users/user/Downloads/summary_rows.xlsx")



