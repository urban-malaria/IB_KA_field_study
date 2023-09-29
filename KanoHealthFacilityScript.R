library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)



df1<-read_xlsx('/Users/user/Desktop/HFS_2509_Kano_034852_V1 With Analysis.xlsx',sheet = 'HFS_2509_Kano_034852_V1')



df1 <- df1 %>%
  mutate_if(is.character, as.factor)


###Summary by LGA----

summary_lga <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`LOCAL GOVT. AREA`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LOCAL GOVT. AREA`))
  ) |>
  select(`LOCAL GOVT. AREA`,  Total_Completed_by_LGA )
View(summary_lga)
print(sum(summary_lga$Total_Completed_by_LGA))

x <- c(summary_lga$`LOCAL GOVT. AREA`)
y <- c(summary_lga$Total_Completed_by_LGA)

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text( size = 10, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = 'black'),
          legend.text =element_text(size = 8, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

lgplot<-ggplot(summary_lga,
  aes(`LOCAL GOVT. AREA`,Total_Completed_by_LGA), ) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LOCAL GOVT. AREA`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Health Facility Survey",
    #   subtitle = "Plot of Completed interviews by PHC",
      # caption = "Data source : Health Facility Survey, Kano"
    )

lgplot<-lgplot + theme_manuscript() +labs(y= "Total Completed in Local Gov't", x = " ")
lgplot

###Summary by PHC----

summary_phc <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
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
                aes(Health_Facility_Name,Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =Health_Facility_Name ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
       #subtitle = "Plot of Completed interviews by PHC",
       caption = "Data source : Health Facility Survey, Kano"
    )

phcplot<-phcplot + theme_manuscript() +labs(y= "Total Completed in PHC", x = "")
phcplot
###LGA Plot with PHC ----

lgplot/phcplot


#Summary by DATE----

summary_Date <-  df1  %>%
  filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`Month_of_Completion`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    Positive_Count = sum(nrow(`Month_of_Completion`))
  ) |>
  select(`Month_of_Completion`,  Total_Completed_by_DATE )

View(summary_Date)

x <- c(summary_Date$`Month_of_Completion`)
y <- c(summary_Date$Total_Completed_by_DATE)


dateplot<-ggplot(summary_Date,
       aes(Month_of_Completion,Total_Completed_by_DATE), size =4 , ) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = signif(Total_Completed_by_DATE)), nudge_y = 0, vjust = -0.5) +
  geom_text(aes(label =Month_of_Completion ), nudge_y = 25, vjust = -0.5, ) +
   labs(#title = "Health Facility Survey",
        # subtitle = "Plot of Achievement by Month",
        # caption = "Data source : Health Facility Survey, Kano"
   )
dateplot <- dateplot + theme_manuscript() +labs(y= "Total Completed by Month", x = "")
dateplot


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
  geom_bar(stat = "identity")+
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
  geom_bar(stat = "identity")+
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
  geom_bar(stat = "identity")+
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

  dftimeoutdoors <- df %>%
  filter ( q117a_e == 'Checked' | q117a_f == 'Checked' | q117a_g == 'Checked' | q117a_h == 'Checked') |>
  print()

  #Tested Positive cases among those that works at night
  dftimeoutdoors_positive <- dftimeoutdoors %>%
    filter ( q503 =='POSITIVE' )

  summary_dfpositive <-  dftimeoutdoors_positive  %>%
    group_by(q114i_b) %>%
    summarise(
      Total_positive_by_outdoors_work_area = n(),
      Positive_Count = sum()
    )

  View(summary_dfpositive)




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
