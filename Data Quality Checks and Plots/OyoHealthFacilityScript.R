library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)



df1<-read_xlsx('/Users/user/Downloads/UrbanMalariaHFS_DATA_ALL_19102023.xlsx')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)






#Summary by DATE----
wardds<- c("Bashorun","Olopomewa","Agugu", "Challenge")

summary_Date <-  df1  %>%
filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
#df_filtered <- summary_Date[!(summary_Date$`Ward` %in% wardds), ] %>%
  group_by(`Month_of_Completion`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Month_of_Completion`))
  ) |>
  select(`Month_of_Completion`,  Total_Completed_by_DATE )
Meettarget=775-df1$Total_Completed_by_DATE
View(summary_Date,Meettarget)

x <- c(summary_Date$`Month_of_Completion`)
y <- c(summary_Date$Total_Completed_by_DATE)


dateplot<-ggplot(summary_Date,
                 aes(reorder(Month_of_Completion, -Total_Completed_by_DATE ),Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity", fill="#661133")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =Month_of_Completion ), nudge_y = 25, vjust = -0.7, ) +
  labs(#title = "Health Facility Survey",
    # subtitle = "Plot of Achievement by Month",
    # caption = "Data source : Health Facility Survey, Kano"
  )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "")
print(dateplot)



###Summary by Classified correctly LGA----

wardds<- c("Bashorun","Olopomewa","Agugu", "Challenge")

summary_lga <-  df1  %>%
  #filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
  #df_filtered <- summary_Date[!(summary_Date$`Ward` %in% wardds), ] %>%
  group_by(`LGA of Address`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA )
View(summary_lga)
print(sum(summary_lga$Total_Completed_by_LGA))

x <- c(summary_lga$`LGA of Address`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot<-ggplot(summary_lga,
  aes(x= reorder(`LGA of Address`,-Total_Completed_by_LGA),y=Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Health Facility Survey",
    #   subtitle = "Plot of Completed interviews by PHC",
      # caption = "Data source : Health Facility Survey, Kano"
    )

lgplot<-lgplot + theme_manuscript() +labs(y= "Total Completed in Local Gov't", x = " ")
lgplot




###Summary by Classified incorrectly LGA----

dfnew<-read_xlsx('/Users/user/Downloads/UrbanMalariaHFS_DATA_ALL_19102023.xlsx')

dfnew <- dfnew %>%
  mutate_if(is.character, as.factor)


wardds<- c("Bashorun","Olopomewa","Agugu", "Challenge")

summary_lga <-  dfnew  %>%
  filter ( `State` =='Ibadan (Oyo)')%>%
 # filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
  #df_filtered <- summary_Date[!(summary_Date$`Ward` %in% wardds), ] %>%
  group_by(`LGA of Address`,`Sampled`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`),
    Sampled=sum(nrow(`Sampled`)=='Yes'))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA , `Sampled`)
View(summary_lga)
print(sum(summary_lga$Total_Completed_by_LGA))

colors <- c("Yes" = "green", "No" = "red", "Sampled" = "red")

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=10, colour = '#334425'),
          legend.text =element_text(size = 10, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot<-ggplot(summary_lga, aes(x= reorder(`LGA of Address`,-Total_Completed_by_LGA), y=Total_Completed_by_LGA, fill=Sampled)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Health Facility Survey",
       #   subtitle = "Plot of Completed interviews by PHC",
       # caption = "Data source : Health Facility Survey, Kano"
  )+ scale_fill_manual(values = colors) + theme_manuscript() +labs(y= "Total Completed in Local Gov't", x = " ")

lgplot




###Summary by Previous Pregnancies----

summary_lga_sel <-  df1  %>%
  filter ( `State` =='Ibadan (Oyo)')%>%
  group_by(`Number of Pregnancy New`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`Number of Pregnancy New`))
  ) |>
  select(`Number of Pregnancy New`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`q124b: How many pregnancies have you had`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          #axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

#colors <- c("1" = "green", "2" = "green", "3" = "green","4" = "green","5" = "red", "6" = "red", "7" = "red", "8" = "red")

lgplot1<-ggplot(summary_lga_sel,
                aes(x=reorder(`Number of Pregnancy New`,-Total_Completed_by_LGA),Total_Completed_by_LGA), fill=`Number of Pregnancy New`) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(sprintf("%.0f", `Number of Pregnancy New`)," Pregnancies")), nudge_y = 20, vjust = -1.9) +
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5 ) +
  labs(title = "Health Facility Survey : Number of Previous Pregnancies",
       #subtitle = "From the graph we can see that some respondents visits the health facility from different LGAs even from outside of the selected LGAs, this might be due to  among other \n reasons proximity of the residence to the Health Facilities, We could not classify some other ",
       caption = "Data source : Health Facility Survey, Kano"
  )+ scale_fill_manual(values = colors)

lgplot1<-lgplot1 + theme_manuscript() +labs(y= "Total Completed by No of Previous Pregnancies", x = " ")
lgplot1




# Check for missing values across all columns---
missing_values <- colSums(is.na(df1))
View(missing_values)


proportion_missing_values <- missing_values / nrow(df1)
View(proportion_missing_values)




###Summary by LGA of Address----

summary_lga_sel <-  df1  %>%
#  filter ( `Complete_part1` =='Complete'  ) %>%
  group_by(`LGA of Address`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`LGA of Address`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot1<-ggplot(summary_lga_sel,
               aes(reorder(`LGA of Address`, -Total_Completed_by_LGA),Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Health Facility Survey : Completed by LGA of Residence",
        #subtitle = "From the graph we can see that some respondents visits the health facility from different LGAs even from outside of the selected LGAs, this might be due to  among other \n reasons proximity of the residence to the Health Facilities, We could not classify some other ",
        #caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1<-lgplot1 + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1


###Summary by LGA of Address - Selected----

summary_lga_sel <-  df1  %>%
  filter ( `Complete_part1` =='Complete' , (`LGA of Address`=="Kano Municipal" | `LGA of Address` == "Tarauni" |`LGA of Address`=="Dala" |`LGA of Address`=="Nassarawa"|`LGA of Address`=="Gwale" |`LGA of Address` =="Fagge")) %>%
  group_by(`LGA of Address`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`LGA of Address`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot1s<-ggplot(summary_lga_sel,
                aes(reorder(`LGA of Address`, -Total_Completed_by_LGA),Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey : Completed by LGA's (Selected) of Residence",
        #  caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1s<-lgplot1s + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1s




###Summary by LGA of Address out of scope----
lgas<- c("Kano Municipal","Tarauni","Dala", "Gwale","Nassarawa","Fagge")
df_filtered <- df1[!(df1$`LGA of Address` %in% lgas), ]
print(df_filtered)

summary_lga_sel <-  df_filtered  %>%
  #filter ( `Complete_part1` =='Complete',`Ward` == 'Zango', `Ward` == 'Dorayi', `Ward` == 'Tudun Wazurci', `Ward` == 'Fagge D2', `Ward` == 'Gobirawa' ) %>%
  group_by(`LGA of Address`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`LGA of Address`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot1ns<-ggplot(summary_lga_sel,
                 aes(reorder(`LGA of Address`, -Total_Completed_by_LGA),Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey : Completed by LGA's (Selected) of Residence",
       #caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1ns<-lgplot1ns + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1ns



lgplot1/(lgplot1s + lgplot1ns)


lgplot1s/(lgplot1nsw)/  lgplot1sfw


###Summary by LGA of Address filtering out 5 wards that are out of scope----

summary_lga_sel <-  df1  %>%
  filter ( `Complete_part1` =='Complete',(`Ward` == 'Zango' | `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') ) %>%
  group_by(`LGA of Address`, `Ward`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`LGA of Address`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green", "POSITIVE" = "red")

# Create a basic bar plot with proportions and colored bars
lgplot1nsw <- ggplot(summary_lga_sel, aes(x = `LGA of Address`, y = Count, fill = `Ward`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5
  ) +
  labs(title = "Wrong wards where interviews were done by LGA", x = " ", y = "Proportion of Result") +
#  scale_fill_manual(values = colors)
 theme_manuscript()  # Apply the color scale
View(summary_lga_sel)
#lgplot1nsw<-lgplot1nsw + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1nsw






###Summary by LGA of Address - Selected filtering out wrong wards----
wardds<- c("Zango","Tudun Wazurci","Dorayi", "Fagge D2","Gobirawa")
summary_lga_sel <-  df1  %>%
  filter ( `Complete_part1` =='Complete' , (`LGA of Address`=="Kano Municipal" | `LGA of Address` == "Tarauni" |`LGA of Address`=="Dala" |`LGA of Address`=="Nassarawa"|`LGA of Address`=="Gwale" |`LGA of Address` =="Fagge"), `Number of Pregnancy New`< 5)
  df_filtered <- summary_lga_sel[!(summary_lga_sel$`Ward` %in% wardds), ] %>%
  group_by(`LGA of Address`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LGA of Address`))
  ) |>
  select(`LGA of Address`,  Total_Completed_by_LGA )
View(df_filtered)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`LGA of Address`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot1sfw<-ggplot(df_filtered,
                 aes(reorder(`LGA of Address`, -Total_Completed_by_LGA),Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LGA of Address`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Completed by LGA of Residence after removing Wrong LGAs and Wards",
    #  caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1sfw<-lgplot1sfw + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1sfw







#Summary of completion---
tablecompletion <- table(df1$Complete_part1)
plot(tablecompletion) + theme_manuscript()

###Summary by PHC----

summary_phc <-  df1  %>%
  filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
  #filter ( `Complete_part1` =='Complete' ) %>%
  group_by(Health_Facility_Name) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(Health_Facility_Name))
  ) |>
  select(Health_Facility_Name,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$Health_Facility_Name)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(x=reorder(Health_Facility_Name,-Total_Completed_by_PHC),y=Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
 # geom_text(aes(label =Health_Facility_Name ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
       #subtitle = "Plot of Completed interviews by PHC",
       caption = "Data source : Health Facility Survey, Kano"
    )

phcplot<-phcplot + theme_manuscript() +labs(y= "Total Completed in PHC", x = "")+ theme(axis.text.x = element_text(angle = 90))
phcplot
###LGA Plot with PHC ----

lgplot/phcplot



###Summary by History of Suspected Fever----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q201: Have you been ill with suspected malaria in the last 2 weeks?`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`q201: Have you been ill with suspected malaria in the last 2 weeks?`))
  ) |>
  select(`q201: Have you been ill with suspected malaria in the last 2 weeks?`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`q201: Have you been ill with suspected malaria in the last 2 weeks?`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(`q201: Have you been ill with suspected malaria in the last 2 weeks?`,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`q201: Have you been ill with suspected malaria in the last 2 weeks?` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
    #subtitle = "Plot of Completed interviews by PHC",
    caption = "Data source : Health Facility Survey, Kano"
  )

phcplot<-phcplot + theme_manuscript() +labs(y= "Total Ill Suspected of Fever", x = "")
phcplot


###Summary by Sick with Fever in past 2 weeks----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q200: Have you been ill with a fever in the last 2 weeks?`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`q200: Have you been ill with a fever in the last 2 weeks?`))
  ) |>
  select(`q200: Have you been ill with a fever in the last 2 weeks?`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`q200: Have you been ill with a fever in the last 2 weeks?`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(`q200: Have you been ill with a fever in the last 2 weeks?`,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`q200: Have you been ill with a fever in the last 2 weeks?` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey Sick with Fever in past 2 weeks",
    #subtitle = "Plot of Completed interviews by PHC",
    caption = "Data source : Health Facility Survey, Kano"
  )

phcplot<-phcplot + theme_manuscript() +labs(y= "Total ill in the last 2 weeks Fever", x = "")
phcplot




###Summary by Treatment Practices----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete', `q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`=="Checked" ) %>%
  group_by(`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`))
  ) |>
  select(`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)`,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`q203: What did you do when you had fever or suspected malaria?  (choice=Nothing)` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey Sick with Fever in past 2 weeks",
    #subtitle = "Plot of Completed interviews by PHC",
    #caption = "Data source : Health Facility Survey, Kano"
  )

plotdn<-phcplot + theme_manuscript() +labs(y= "Did Nothing when ill", x = "")
plotdn

###Summary by Treatment Practices 2----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete', `q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`=="Checked" ) %>%
  group_by(`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`))
  ) |>
  select(`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)`,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`q203: What did you do when you had fever or suspected malaria?  (choice=Self-medication with drugs at home)` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey Sick with Fever in past 2 weeks",
    #subtitle = "Plot of Completed interviews by PHC",
    #caption = "Data source : Health Facility Survey, Kano"
  )

plothm<-phcplot + theme_manuscript() +labs(y= "Self Medication with Drugs at Home", x = "")
plothm




###Summary by Treatment Practices 3----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete', `q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`=="Checked" ) %>%
  group_by(`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`))
  ) |>
  select(`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)`,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`q203: What did you do when you had fever or suspected malaria?  (choice=Sought advice/treatment)` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey Sick with Fever in past 2 weeks",
    #subtitle = "Plot of Completed interviews by PHC",
    caption = "Data source : Health Facility Survey, Kano"
  )

plott<-phcplot + theme_manuscript() +labs(y= "(choice=Sought advice/treatment)", x = "")
plott









plotdn + plothm + plott
#Summary by AGE----
summary_Age <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`Age`) %>%
  summarise(
    Total_Completed_by_AGE = n(),
    Positive_Count = sum(nrow(`Age`))
  ) |>
  select(`Age`,  Total_Completed_by_AGE )

View(summary_Age)

x <- c(summary_Age$`Age`)
y <- c(summary_Age$Total_Completed_by_AGE)


ageplot<-ggplot(summary_Age,
                 aes(Age,Total_Completed_by_AGE), size =4 , ) +
  geom_bar(stat = "identity", fill="#223377")+
  geom_text(aes(label = signif(Total_Completed_by_AGE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =Age ), nudge_y = 25, vjust = -0.5, ) +
  labs(#title = "Health Facility Survey",
       # subtitle = "Plot of Achievement by Month",
        #caption = "Data source : Health Facility Survey, Kano"
  )
ageplot <- ageplot + theme_manuscript() +labs(y= "Completed Interviews by Age", x = "")
ageplot


#Summary by Education----

summary_Education <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q103: What is the highest level of formal school you completed?`) %>%
  summarise(
    Total_Completed_by_Education = n(),
    Positive_Count = sum(nrow(`q103: What is the highest level of formal school you completed?`))
  ) |>
  select(`q103: What is the highest level of formal school you completed?`,  Total_Completed_by_Education )

View(summary_Education)

x <- c(summary_Education$`q103: What is the highest level of formal school you completed?`)
y <- c(summary_Education$Total_Completed_by_Education)


educationplot<-ggplot(summary_Education,
                aes(`q103: What is the highest level of formal school you completed?`,Total_Completed_by_Education), size =4 , ) +
  geom_bar(stat = "identity", fill="#775500")+
  geom_text(aes(label = signif(Total_Completed_by_Education)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`q103: What is the highest level of formal school you completed?` ), nudge_y = 25, vjust = -0.5, ) +
  labs(title = "Health Facility Survey",
       # subtitle = "Plot of Achievement by Month",
       #caption = "Data source : Health Facility Survey, Kano"
  )
educationplot <- educationplot + theme_manuscript() +labs(y= "Completed Interviews by Education", x = "")
educationplot


#Summary by Post_Secondary_Education----

summary_Post_Secondary_Education <-  df1  %>%
  filter ( `Complete_part1` =='Complete', !(is.na(`q104: If post-secondary school completed: Specify the highest level completed in this category:`)) ) %>%
  group_by(`q104: If post-secondary school completed: Specify the highest level completed in this category:`) %>%
  summarise(
    Total_Completed_by_Post_Secondary_Education = n(),
    Positive_Count = sum(nrow(`q104: If post-secondary school completed: Specify the highest level completed in this category:`))
  ) |>
  select(`q104: If post-secondary school completed: Specify the highest level completed in this category:`,  Total_Completed_by_Post_Secondary_Education )

View(summary_Post_Secondary_Education)

oeducationplot<-ggplot(summary_Post_Secondary_Education,
  aes(`q104: If post-secondary school completed: Specify the highest level completed in this category:`,Total_Completed_by_Post_Secondary_Education), size =4 , ) +
  geom_bar(stat = "identity", fill="#909090")+
  geom_text(aes(label = signif(Total_Completed_by_Post_Secondary_Education)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =`q104: If post-secondary school completed: Specify the highest level completed in this category:` ), nudge_y = 25, vjust = -0.5, ) +
  labs(#title = "Health Facility Survey",
       # subtitle = "Plot of Achievement by Month",
       caption = "Data source : Health Facility Survey, Kano"
  )
oeducationplot <- oeducationplot + theme_manuscript() +labs(y= "Completed Cases by Education - Post Secondary", x = "")
oeducationplot


# Combining plots for Month of Completion, Age, Education and Post Secondary Level

 educationplot / (dateplot + ageplot+oeducationplot)

#Summary by Post_Secondary_Education_Others----

summary_Post_Secondary_Education_Others <-  df1  %>%
  filter ( `Complete_part1` =='Complete', !(is.na(`q104i: If Others, specify`))  ) %>%
  group_by(`q104i: If Others, specify`) %>%
  summarise(
    Total_Completed_by_Post_Secondary_Education_Others = n(),
    Positive_Count = sum(nrow(`q104i: If Others, specify`))
  ) |>
  select(`q104i: If Others, specify`,  Total_Completed_by_Post_Secondary_Education_Others )

View(summary_Post_Secondary_Education_Others)

x <- c(summary_Post_Secondary_Education_Others$`q104i: If Others, specify`)
y <- c(summary_Post_Secondary_Education_Others$Total_Completed_by_Post_Secondary_Education_Others)



  barplot(y,names.arg=x, density=10, main="Total Completed Interviews for Post Secondary Education", xlab="Post_Secondary_Education_Otherss", ylab="Number of Completed Interviews") +
  geom_text(aes(label = y), vjust = -0.5, size = 4)


#Summary by HH_Income----

summary_HH_Income <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q109: Do you currently do any work to earn an income?`) %>%
  summarise(
    Total_Completed_by_HH_Income = n(),
    Positive_Count = sum(nrow(`q109: Do you currently do any work to earn an income?`))
  ) |>
  select(`q109: Do you currently do any work to earn an income?`,  Total_Completed_by_HH_Income )

View(summary_HH_Income)

x <- c(summary_HH_Income$`q109: Do you currently do any work to earn an income?`)
y <- c(summary_HH_Income$Total_Completed_by_HH_Income)


barplot(y,names.arg=x, density=10, main="Total Completed Interviews by HH_Income", xlab="HH_Incomes", ylab="Number of Completed Interviews") +
  geom_text(aes(label = y), vjust = -0.5, size = 4)



#q110 Summary by Type_of Work----
typeofwork <- table(df1$`q110: What kind of work do you do?`)
show(typeofwork)
barplot(typeofwork)



#q111 Summary by Agricultural Work----
typeofagricwork <- table(df1$`q111: If you are an agricultural worker, what type of agricultural produce do you work with?`)
typeofagricwork
barplot(typeofagricwork)



#Summary by AGE Tested Positive----

summary_Age_positive <-  df1  %>%
  filter ( `Complete_part1` =='Complete' , `q503: RESULT` =='POSITIVE' ) %>%
  group_by(`Age`) %>%
  summarise(
    Total_Completed_by_AGE = n(),
    Positive_Count = sum(nrow(`Age`))
  ) |>
  select(`Age`,  Total_Completed_by_AGE )

View(summary_Age_positive)

x <- c(summary_Age_positive$`Age`)
y <- c(summary_Age_positive$Total_Completed_by_AGE)

barplot(y,names.arg=x, density=10, main="Total Positive by ages", xlab="AGEs", ylab="Number of Completed Interviews") +
  geom_text(aes(label = y), vjust = -0.5, size = 4)


#Summary by MARITAL_STATUS----

  summary_Marital_Status <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q107: What is your current marital status?`) %>%
  summarise(
    Total_Completed_by_MARITAL_STATUS = n(),
    Positive_Count = sum(nrow(`q107: What is your current marital status?`))
  ) |>
  select(`q107: What is your current marital status?`,  Total_Completed_by_MARITAL_STATUS )

  View(summary_Marital_Status)

  x <- c(summary_Marital_Status$`q107: What is your current marital status?`)
  y <- c(summary_Marital_Status$Total_Completed_by_MARITAL_STATUS)


  barplot(y,names.arg=x, density=10, main="Total Completed Interviews by areas", xlab="MARITAL_STATUSs", ylab="Number of Completed Interviews") +
    geom_text(aes(label = y), vjust = -0.5, size = 4)



#Summary by PLACE_OF_RESIDENCE----

  summary_Place_of_Residence <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`q100: How long have you been living continuously in (CURRENT PLACE OF RESIDENCE)  YEARS?`) %>%
  summarise(
    Total_Completed_by_PLACE_OF_RESIDENCE = n(),
    Positive_Count = sum(nrow(`q100: How long have you been living continuously in (CURRENT PLACE OF RESIDENCE)  YEARS?`))
  ) |>
  select(`q100: How long have you been living continuously in (CURRENT PLACE OF RESIDENCE)  YEARS?`,  Total_Completed_by_PLACE_OF_RESIDENCE )

  View(summary_Place_of_Residence)

  x <- c(summary_Place_of_Residence$`q100: How long have you been living continuously in (CURRENT PLACE OF RESIDENCE)  YEARS?`)
  y <- c(summary_Place_of_Residence$Total_Completed_by_PLACE_OF_RESIDENCE)


  barplot(y,names.arg=x, density=10, main="Total Completed Interviews by areas", xlab="Length of Stay in PLACE_OF_RESIDENCEs", ylab="Number of Completed Interviews") +
  geom_text(aes(label = y), vjust = -0.5, size = 4)


#Check the time they spend outdoors at work if that could be responsible for malaria ----

  dftimeoutdoors <- df1 %>%
  filter ( q117a_e == 'Checked' | q117a_f == 'Checked' | q117a_g == 'Checked' | q117a_h == 'Checked') |>
  print()

  #Tested Positive cases among those that works at night
  dftimeoutdoors_positive <- dftimeoutdoors %>%
    filter ( q503 =='POSITIVE' | q503 =='NEGATIVE')

  summary_dfpositive <-  dftimeoutdoors_positive  %>%
    group_by(`LGA of Address`, q503) %>%
  summarize(Count = n()) %>%
    ungroup() %>%
    group_by(q114i_b) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_outdoors <- ggplot(summary_dfpositive, aes(x = `LGA of Address`, y = Proportion, fill = `q503`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by  Mosquito net used yesterday", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_outdoors)



  #Tested Positive cases by area----
  dfarea_positive <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' )

    summary_area_positive <-  dfarea_positive  %>%
    group_by(`LOCAL GOVT. AREA`) %>%
    summarise(
      Total_positive_by_outdoors_work_area = n(),
      Positive_Count = sum()
    ) |>
      select(`LOCAL GOVT. AREA`, Total_positive_by_outdoors_work_area)

  View(summary_area_positive)

  x <- c(summary_area_positive$`LOCAL GOVT. AREA`)
  y <- c(summary_area_positive$Total_positive_by_outdoors_work_area)

  barplot(y,names.arg=x, density=10, main="Tested Positive for Malaria by area", xlab="LGAs", ylab="Number of Cases") +
    geom_text(aes(label = y), vjust = -0.5, size = 4)


#Test Status by area----
Status_by_LGA =  table(df1$`q503: RESULT`,
        df1$`LOCAL GOVT. AREA`)

Status_by_LGA

  barplot(table(df1$`q503: RESULT`,
                df1$`LOCAL GOVT. AREA`),
          legend.text = TRUE,
          main = "Test Status by LGA",
          xlab = "LGA",
          ylab = "Cases",
          col = c("green",
                  "red3"),
          border = NA,
          las = 1)


  tested <-  df1  %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  )
  p <- ggplot(tested, aes(x = `LOCAL GOVT. AREA`, fill = `q503: RESULT` )) +
    geom_bar(position = "fill") +  # Adjust 'position' as needed

  geom_text(
    stat = "count",
    aes(label = ..count.., group = `q503: RESULT`),
    position = position_fill(vjust = 0.5)
  ) +

  labs(title = "Stacked Bar Plot", x = "Local Govt. Area", y = "Proportion") + theme_manuscript()
  # Print the plot
  print(p)


  #Overall Postive Count ----

  tested1 <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  ) %>%
    group_by(`q503: RESULT`) %>%
    summarize(Count = n()) %>%
    #ungroup() %>%
    mutate(Proportion = Count / sum(Count))

  # Create a plot with proportions
  colors <- c("NEGATIVE" = "green", "POSITIVE" = "red")
  p <- ggplot(tested, aes(x = "", y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(label = scales::percent(Proportion)),
      position = position_fill(vjust = 0.5),
      vjust = -0.5
    ) +
    labs(title = "Stacked Bar Plot with Proportions", x = "Local Govt. Area", y = "Proportion")
  labs(title = "Test Results by LGA", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p)






  tested <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  ) %>%
    group_by(`LOCAL GOVT. AREA`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    #ungroup() %>%
    mutate(Proportion = Count / sum(Count))

  # Create a plot with proportions
  colors <- c("NEGATIVE" = "green", "POSITIVE" = "red")
  p <- ggplot(tested, aes(x = `LOCAL GOVT. AREA`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(label = scales::percent(Proportion)),
      position = position_fill(vjust = 0.5),
      vjust = -0.5
    ) +
    labs(title = "Stacked Bar Plot with Proportions", x = "Local Govt. Area", y = "Proportion")
  labs(title = "Test Results by LGA", x = " ", y = "Proportion of Result") +
  scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p)


  # Calculate the percentage within each group
    tested <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  ) %>%
    group_by(`LOCAL GOVT. AREA`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LOCAL GOVT. AREA`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

    # Create a basic bar plot with proportions
    p <- ggplot(tested, aes(x = `LOCAL GOVT. AREA`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Stacked Bar Plot with Proportions", x = "Local Govt. Area", y = "Percentage") + theme_manuscript()

  # Print the plot
  print(p)


  #Test Status by LGA----



  tested <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  )%>%
  group_by(`LGA of Address`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green", "POSITIVE" = "red")

  # Create a basic bar plot with proportions and colored bars
  p <- ggplot(tested, aes(x = `LGA of Address`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by LGA", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p)






  #Test Status by Age----



  testedAge <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  )%>%
    group_by(Age, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(Age) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green", "POSITIVE" = "red")

  # Create a basic bar plot with proportions and colored bars
  p_Age <- ggplot(testedAge, aes(x = reorder(Age,-Count), y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Age", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale


  # Print the plot
  print(p_Age)


  #Test Status by Workplace----



  testedWp <- df1 %>%
    filter ( `q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'  )%>%
    group_by(`q115: Which best describes your workplace`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q115: Which best describes your workplace`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_Wp <- ggplot(testedWp, aes(x = `q115: Which best describes your workplace`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Type of Work Place", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_Wp)



  #Test Status by Workplace----



  testedWp <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`q115: Which best describes your workplace` == "Outside the city" | `q115: Which best describes your workplace` == "Rural Area" | `q115: Which best describes your workplace` == "Within the city")  )%>%
    group_by(`q115: Which best describes your workplace`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q115: Which best describes your workplace`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_Wp <- ggplot(testedWp, aes(x = `q115: Which best describes your workplace`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Type of Work Place", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_Wp)

table(df1$`q115: Which best describes your workplace`, df1$`q503: RESULT`)



#Test Status by Workplace 116----



testedWp <- df1 %>%
  filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`q116: Do you work indoors, outdoors or both?` == "Both" | `q116: Do you work indoors, outdoors or both?` == "Indoors" | `q116: Do you work indoors, outdoors or both?` == "Not Applicable" | `q116: Do you work indoors, outdoors or both?` == "Outdoors"))  %>%
  group_by(`q116: Do you work indoors, outdoors or both?`, `q503: RESULT`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`q116: Do you work indoors, outdoors or both?`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

# Create a basic bar plot with proportions and colored bars
p_Wp <- ggplot(testedWp, aes(x = `q116: Do you work indoors, outdoors or both?`, y = Proportion, fill = `q503: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5
  ) +
  labs(title = "Test Results by Type of Work Place", x = " ", y = "Proportion of Result") +
  scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

# Print the plot
print(p_Wp)

table(df1$`q116: Do you work indoors, outdoors or both?`, df1$`q503: RESULT`)




#Test Status by  301----



testedWp <- df1 %>%
  filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`q116: Do you work indoors, outdoors or both?` == "Both" | `q116: Do you work indoors, outdoors or both?` == "Indoors" | `q116: Do you work indoors, outdoors or both?` == "Not Applicable" | `q116: Do you work indoors, outdoors or both?` == "Outdoors"))  %>%
  group_by(`q116: Do you work indoors, outdoors or both?`, `q503: RESULT`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`q116: Do you work indoors, outdoors or both?`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

# Create a basic bar plot with proportions and colored bars
p_Wp <- ggplot(testedWp, aes(x = `q116: Do you work indoors, outdoors or both?`, y = Proportion, fill = `q503: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5
  ) +
  labs(title = "Test Results by Type of Work Place", x = " ", y = "Proportion of Result") +
  scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

# Print the plot
print(p_Wp)

table(df1$`q301: Does your household own a mosquito net`, df1$`q503: RESULT`)






  #Test Status by Mosquito net----



  testednet <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q301: Does your household own a mosquito net` =='Yes' | `q301: Does your household own a mosquito net`=='No') )%>%
    group_by(`q301: Does your household own a mosquito net`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q301: Does your household own a mosquito net`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_net <- ggplot(testednet, aes(x = `q301: Does your household own a mosquito net`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Ownership of Mosquito net", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_net)


  #Test Status by Mosquito net Hung ----



  testednethang <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q303: Do you have your nets permanently hung?` =='Yes' | `q303: Do you have your nets permanently hung?`=='No') )%>%
    group_by(`q303: Do you have your nets permanently hung?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q303: Do you have your nets permanently hung?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_nethang <- ggplot(testednethang, aes(x = `q303: Do you have your nets permanently hung?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Permanently hung Mosquito net", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_nethang)





  #Test Status by Mosquito net Used Yesterday ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q309: Did you sleep inside a mosquito net last night?` =='Yes' | `q309: Did you sleep inside a mosquito net last night?`=='No') )%>%
    group_by(`q309: Did you sleep inside a mosquito net last night?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q309: Did you sleep inside a mosquito net last night?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q309: Did you sleep inside a mosquito net last night?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by  Mosquito net used yesterday", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)

  table(df1$`q309: Did you sleep inside a mosquito net last night?`, df1$`q503: RESULT`)

p_net+p_nethang+p_netused_yesterday

  table(df1$`q301: Does your household own a mosquito net`)
  p/(p_Age + p_Wp)




  #Test Status by Mosquito net ownership versus used----



  testednet <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q301: Does your household own a mosquito net` =='Yes' ), (`q309: Did you sleep inside a mosquito net last night?` =='Yes' | `q309: Did you sleep inside a mosquito net last night?`=='No') )%>%
    group_by(`q301: Does your household own a mosquito net`, `q309: Did you sleep inside a mosquito net last night?`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q301: Does your household own a mosquito net`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("Yes" = "green3", "No" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_net <- ggplot(testednet, aes(x = `q301: Does your household own a mosquito net`, y = Proportion, fill = `q309: Did you sleep inside a mosquito net last night?`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Ownership of Mosquito net versus used", x = " ", y = "Proportion of those that owned mosquito net that used yesterday") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_net)




  #Test Status by Mosquito net ownership versus hunged----


  testednet <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q303: Do you have your nets permanently hung?` =='Yes' | `q303: Do you have your nets permanently hung?`=='No'),(`q301: Does your household own a mosquito net` =='Yes' ) )%>%
    group_by(`q301: Does your household own a mosquito net`, `q303: Do you have your nets permanently hung?`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q301: Does your household own a mosquito net`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("Yes" = "green3", "No" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_net <- ggplot(testednet, aes(x = `q301: Does your household own a mosquito net`, y = Proportion, fill = `q303: Do you have your nets permanently hung?`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Ownership of Mosquito net versus Hunged", x = " ", y = "Proportion of those that owned mosquito net that used yesterday") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_net)



  #Test Status by Mosquito net ownership versus hunged versus used ----


  testednet <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q303: Do you have your nets permanently hung?` =='Yes'),(`q301: Does your household own a mosquito net` =='Yes' ), (`q309: Did you sleep inside a mosquito net last night?` =="Yes" | `q309: Did you sleep inside a mosquito net last night?` =="No") )%>%
    group_by(`q303: Do you have your nets permanently hung?`, `q309: Did you sleep inside a mosquito net last night?`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q303: Do you have your nets permanently hung?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("Yes" = "green3", "No" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_net <- ggplot(testednet, aes(x = `q303: Do you have your nets permanently hung?`, y = Proportion, fill = `q309: Did you sleep inside a mosquito net last night?`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Hunged Mosquito nets versus used", x = " ", y = "Proportion of those that owned mosquito net, hanged it  that used yesterday") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_net)


  #Test Status by Door screen ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q311: Do you have window/door screens?` =='Yes' | `q311: Do you have window/door screens?`=='No') )%>%
    group_by(`q311: Do you have window/door screens?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q311: Do you have window/door screens?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q311: Do you have window/door screens?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by  Have Door or Window Screen", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)



  #Test Status by ever used insecticide spray ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q312: Has your household ever used an insecticide spray?` =='Yes' | `q312: Has your household ever used an insecticide spray?` =='No' ) )%>%
    group_by(`q312: Has your household ever used an insecticide spray?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q312: Has your household ever used an insecticide spray?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q312: Has your household ever used an insecticide spray?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results Have you ever used Insecticide Spray", x = " q312: Has your household ever used an insecticide spray ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)



  #Test Status by Frequency of use for insecticide spray ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q312: Has your household ever used an insecticide spray?` =='Yes' ) )%>%
    group_by(`q313: If yes, how often do you use insecticide spray?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q313: If yes, how often do you use insecticide spray?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q313: If yes, how often do you use insecticide spray?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by  Usage of Insecticide Spray", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)




  #Test Status by ever used mosquito coil in the last 2 weeks ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q314: In the last two weeks, have you used a mosquito coil?` =='Yes' | `q314: In the last two weeks, have you used a mosquito coil?` =='No' ) )%>%
    group_by(`q314: In the last two weeks, have you used a mosquito coil?` , `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q314: In the last two weeks, have you used a mosquito coil?` ) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q314: In the last two weeks, have you used a mosquito coil?` , y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results Have you used Mosquito coil in the last 2 weeks", x = " q312: Has your household ever used an insecticide spray ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)





  #Test Status by Frequency of use for mosquito coil ----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q314: In the last two weeks, have you used a mosquito coil?` =='Yes') , `q315: Have you ever used mosquito coils in your household?`=="Yes")%>%
    group_by(`q316: If yes, how often do you use the mosquito coil?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q316: If yes, how often do you use the mosquito coil?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q316: If yes, how often do you use the mosquito coil?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Frequency of Usage of Mosquito Coil", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)





  #Test Status by Frequency of use of SP/Fansidar----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q321: During this pregnancy, have you ever taken SP/Fansidar to prevent malaria?` =='Yes') | `q321: During this pregnancy, have you ever taken SP/Fansidar to prevent malaria?`=="No")%>%
    group_by(`q321: During this pregnancy, have you ever taken SP/Fansidar to prevent malaria?`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q321: During this pregnancy, have you ever taken SP/Fansidar to prevent malaria?`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q321: During this pregnancy, have you ever taken SP/Fansidar to prevent malaria?`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by taken SP/Fansidar to Prevent Malaria in Pregnancy", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)


  #Test Status by Travel Status----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q402: Have you travelled out of your current residence in the last 4 weeks` =='Yes') | `q402: Have you travelled out of your current residence in the last 4 weeks`=="No")%>%
    group_by(`q402: Have you travelled out of your current residence in the last 4 weeks`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q402: Have you travelled out of your current residence in the last 4 weeks`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q402: Have you travelled out of your current residence in the last 4 weeks`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Travel Status in the Last 4 Weeks", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)








  #Test Status by Travel Status----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ),(`q402: Have you travelled out of your current residence in the last 4 weeks` =='Yes') )%>%
    group_by(`q407: How best would you describe your travel location? RESPONSES TO BE GUIDED BY INTERVIEWER`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q407: How best would you describe your travel location? RESPONSES TO BE GUIDED BY INTERVIEWER`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q407: How best would you describe your travel location? RESPONSES TO BE GUIDED BY INTERVIEWER`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Travel Status in the Last 4 Weeks", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)






  #Test Status by Previous Pregnancies----

  testednetused_yesterday <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ), `Complete_part1`=="Complete")%>%
    group_by(`q124b: How many pregnancies have you had`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`q124b: How many pregnancies have you had`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_netused_yesterday <- ggplot(testednetused_yesterday, aes(x = `q124b: How many pregnancies have you had`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Test Results by Number of Previous Pregnancies", x = " q313: If yes, how often do you use insecticide spray? ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_netused_yesterday)



table(df1$Ward,df1$`q503: RESULT`)


#Test Status by Ward  ----

wardds<- c("Bashorun","Olopomewa","Agugu", "Challenge")

df1 <- df1 %>%
  filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
  filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ))%>%
  group_by(`Ward`, `q503: RESULT`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`Ward`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

# Create a basic bar plot with proportions and colored bars
plot_wardresult <- ggplot(df1, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5
  ) +
  labs(title = "Test Results by  Mosquito net used yesterday", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
  scale_fill_manual(values = colors) + theme_manuscript() + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

# Print the plot
print(plot_wardresult)



  # Create a factor variable for 'shstate' to store the labels

  summary_by_wards <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE'))%>%
    group_by(Ward) %>%
    summarise(
      Total_Count = n(),
      Positive_Count = sum()
    )
  View(summary_by_wards)


  #Positive by Month ----

  testedbymonth <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023' | `Month_of_Completion` == 'Oct, 2023' ) )%>%
    group_by(`Month_of_Completion`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Month_of_Completion`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_testedbymonth <- ggplot(testedbymonth, aes(x = reorder(`Month_of_Completion`,-Count), y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Month_of_Completion", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbymonth)





  #Health Facility by Month ----

  testedbymonth <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023' | `Month_of_Completion` == 'Oct, 2023' ) )%>%
    group_by(`Month_of_Completion`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Month_of_Completion`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_testedbymonth <- ggplot(testedbymonth, aes(x = reorder(`Month_of_Completion`,-Count), y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Month_of_Completion", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbymonth)


  #Positive by Age ----

  testedbymonth <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE') )%>%
    group_by(`Age`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Age`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Create a basic bar plot with proportions and colored bars
  p_testedbymonth <- ggplot(testedbymonth, aes(x = reorder(`Age`,-Count), y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +


    labs(title = "Interviews by Age ", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale
   #geom_text(aes(label = signif(Total)), nudge_y = 2, vjust = -2.5 )
  # Print the plot
  print(p_testedbymonth)




   #Status by LGA ----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'))%>%
    group_by(`LGA of Address`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion LGAs based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of KMC
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by LGAs based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count







  #Status by Selected LGA ----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'),(`LGA of Address`=="Kano Municipal" | `LGA of Address` == "Tarauni" |`LGA of Address`=="Dala" |`LGA of Address`=="Nassarawa"|`LGA of Address`=="Gwale" |`LGA of Address` =="Fagge"))%>%
    group_by(`LGA of Address`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion LGAs based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of KMC
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by LGAs based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count




  #PHC by LGA of Residence ----

  testedbyWard_sel <- df1 %>%
    filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
        group_by( `Health_Facility_Name`, `LOCAL GOVT. AREA`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LOCAL GOVT. AREA`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  #View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `LOCAL GOVT. AREA` , y = Proportion, fill =  `Health_Facility_Name`)) +
    geom_bar(stat = "identity")  +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)), position = position_stack(vjust = 0.5),  vjust = -0.2 ) +
    #geom_text(aes(label = signif(Count)),
     #         position = position_stack(vjust = 0.5),
      #        vjust = -1.6
   # ) +
   # geom_text(aes(label = `Health_Facility_Name`), position = position_stack(vjust = 0.5), vjust = -4.0) +

    theme_manuscript()    # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)




  #PHC by LGA ----

  testedbyWard_sel <- df1 %>%
    filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
    group_by( `Health_Facility_Name`, `LGA of Address`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Health_Facility_Name`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  #View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Health_Facility_Name`, y = Proportion, fill =  `LGA of Address`)) +
    geom_bar(stat = "identity")  +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)), position = position_stack(vjust = 0.5),  vjust = -0.2 ) +
    #geom_text(aes(label = signif(Count)),
    #         position = position_stack(vjust = 0.5),
    #        vjust = -1.6
    # ) +
    # geom_text(aes(label = `Health_Facility_Name`), position = position_stack(vjust = 0.5), vjust = -4.0) +

    theme_manuscript()   + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  #LGA of Health Facility by LGA of Residence ----

  testedbyWard_sel <- df1 %>%
    filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds)) %>%
    group_by( `LOCAL GOVT. AREA`, `LGA of Address`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard_sel)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x =`LGA of Address` , y = Proportion, fill =   `LOCAL GOVT. AREA`)) +
    geom_bar(stat = "identity")  +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)), position = position_stack(vjust = 0.5),  vjust = -0.2 ) +
    #geom_text(aes(label = signif(Count)),
    #         position = position_stack(vjust = 0.5),
    #        vjust = -1.6
    # ) +
    # geom_text(aes(label = `Health_Facility_Name`), position = position_stack(vjust = 0.5), vjust = -4.0) +

    theme_manuscript()  # + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)





  #LGA of Health Facility by LGA of Residence ----

  testedbyWard_sel <- df1 %>%
    filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds), `LGA_Match` == "Yes" ) %>%
    group_by( `LOCAL GOVT. AREA`, `LGA of Address`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard_sel)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x =`LGA of Address` , y = Total, fill =   `LOCAL GOVT. AREA`)) +
    geom_bar(stat = "identity")  +
    #geom_text(aes(label = sprintf("%.1f%%", Proportion)), position = position_stack(vjust = 0.5),  vjust = -0.2 ) +
    geom_text(aes(label = signif(Count)),
             position = position_stack(vjust = 0.5),
            vjust = -1.6
     ) +
    # geom_text(aes(label = `Health_Facility_Name`), position = position_stack(vjust = 0.5), vjust = -4.0) +

    theme_manuscript()  # + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  #Health Facility by LGA of Residence ----

  testedbyWard_sel <- df1 %>%
    filter ( `State` =='Ibadan (Oyo)', (`LGA of Address`=="Ibadan North East" | `LGA of Address` == "Ibadan North" |`LGA of Address`=="Ibadan North West" |`LGA of Address`=="Ibadan South West"|`LGA of Address`=="Ibadan South East"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds), `LGA_Match` == "Yes" ) %>%
    group_by( `Health_Facility_Name`, `LGA of Address`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Health_Facility_Name`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard_sel)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = reorder(`Health_Facility_Name`,-Total) , y = Total, fill =   `LGA of Address`)) +
    geom_bar(stat = "identity")  +
    #geom_text(aes(label = sprintf("%.1f%%", Proportion)), position = position_stack(vjust = 0.5),  vjust = -0.2 ) +
    geom_text(aes(label = signif(Count)),
              position = position_stack(vjust = 0.5),
              vjust = -1.6
    ) +
    # geom_text(aes(label = `Health_Facility_Name`), position = position_stack(vjust = 0.5), vjust = -4.0) +

    theme_manuscript()   + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)




  #PHC by LGA Side by side----

  testedbyWard_sel <- df1 %>%
    filter ( Selected_LGA=='No', (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'))%>%
    group_by(`LGA of Address`, `Health_Facility_Name`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Proportion, fill = `Health_Facility_Name`)) +
    geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
    ylim(0,800) +

  geom_text(aes(label = sprintf("%.1f%%", Proportion)), vjust = 1.5,
            position = position_dodge(.9), size = 4) +

    labs(title = "Proportion LGAs based on Test Results", x = " ", y = "Proportion of Result") +
    theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)






  # Count plot of KMC
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `LOCAL GOVT. AREA`, y = Count, fill = `Health_Facility_Name`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by LGAs based on Test Results", x = " ", y = "Proportion of Result")  +
    theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count
  table(testedbyWard_sel$`Health_Facility_Name`,testedbyWard_sel$`LOCAL GOVT. AREA`)

  #Positive by Ward Selected KMC----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='KMC' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in KMC based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of KMC
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in KMC based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count






  #Positive by Ward Selected Gwale----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='Gwale' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in Gwale based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of GWL ----
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in Gwale based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count



  #Positive by Ward Selected Nassarawa----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='Nassarawa' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in Nassarawa based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of NSR ----
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in Nassarawa based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count


  #Positive by Ward Selected Fagge----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='Fagge' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in Fagge based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of NSR ----
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in Fagge based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count



  #Positive by Ward Selected Tarauni----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='Tarauni' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in Tarauni based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of NSR ----
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in Tarauni based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count




  #Positive by Ward Selected Dala----

  testedbyWard_sel <- df1 %>%
    filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='Dala' )%>%
    group_by(`Ward`, `q503: RESULT`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`Ward`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)

  table(testedbyWard_sel$Ward, testedbyWard_sel$`q503: RESULT`)

  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Proportion by Wards in Dala based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)



  # Count plot of NSR ----
  p_testedbyWard_sel_count <- ggplot(testedbyWard_sel, aes(x = `Ward`, y = Count, fill = `q503: RESULT`)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5),
              vjust = -0.5
    ) +
    labs(title = "Frequency by Wards in Dala based on Test Results", x = " ", y = "Proportion of Result") +
    scale_fill_manual(values = colors) + theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel_count)

  #Combined Maps  Selected Proportion and Counts

  p_testedbyWard_sel / p_testedbyWard_sel_count


    #Loading the map

  library(sf)
  library(ggplot2)
  library(ggmap)
  library(plotly)
  shape <- read_sf(dsn = "/Users/user/Downloads/Kano_metro_ward_sixLGAs/", layer = "Kano_metro_ward_sixLGAs")

  #Left joining the positive count with map polygon
  mymap2=left_join(shape,summary_by_wards,by=c("WardName"="Ward"))


  #Plotting the map
   mymapplot<- ggplot(mymap2) +
    geom_sf(data=mymap2, mapping = aes(fill=WardName, geometry=geometry, colors="white"), show.legend = TRUE) +
   # geom_sf_text(data=mymap2,aes(label = WardName, geometry=geometry), vjust = -0.5)+
     geom_sf_text(data=mymap2,aes(label = Total_Count, geometry=geometry), vjust = -0.5)+
     theme_manuscript()

    #+geom_point(data = df1, aes(x="latitude", y="longitude"))
mymapplot


p <-  mymapplot + geom_point(aes(df1$latitude, df1$longitude))

ggplotly(p)
