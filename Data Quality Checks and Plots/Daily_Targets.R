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
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "")
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

View(summary_Date)

#Summary by DATE and Enumerators----
dateplot<-ggplot(summary_Date,
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
