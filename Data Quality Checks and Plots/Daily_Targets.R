library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
df1<-read_csv('/Users/user/Downloads/kn_hh_data_2811.csv')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)



#Summary by DATE----

summary_Date <-  df1  %>%
  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`Visit 1 Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, `INTERVIEWER'S NAME` , Total_Completed_by_DATE )

View(summary_Date)


dateplot<-ggplot(summary_Date,
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
dateplot<-ggplot(summary_Date,
                 aes(`Visit 1 Date`,Total_Completed_by_DATE), size =4 , ) +
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
  group_by(`Visit 1 Date`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Visit 1 Date`))
  ) |>
  select(`Visit 1 Date`, Total_Completed_by_DATE )
View(summary_Date_)

summary_Date_$MonthNum <- gsub("/","" ,substr(summary_Date_$`Visit 1 Date`, 1, 2))

summary_Month_ <-  summary_Date_  %>%
group_by(MonthNum) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)

  )

View(summary_Month_)



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
