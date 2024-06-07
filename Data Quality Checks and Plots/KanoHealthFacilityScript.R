library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)



df1<-read_csv('/Users/user/Downloads/UrbanMalariaHFS_DATA_KN_IB_Merged.csv')

df1 <- df1 %>%
  mutate_if(is.character, as.factor)

View(df1)
#Summary by DATE----
wardds<- c("Zango","Giginyu","Dorayi", "Fagge D2","Gobirawa","Olopomewa","Basorun", "Agugu", "Challenge")
#summary_Date <-  df1  %>%
#  filter ( `Complete1` =='Complete' ) %>%
#summary_Date <-  df1  %>%
# filter ( `Complete1` =='Complete' , (`LGA of Address`=="Kano Municipal" | `LGA of Address` == "Tarauni" |`LGA of Address`=="Dala" |`LGA of Address`=="Nassarawa"|`LGA of Address`=="Gwale" |`LGA of Address` =="Fagge"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
df_filtered <- df1[!(df1$`Ward` %in% wardds), ] %>%
  group_by(`State`) %>%
  summarise(
    Total = n(),
    #myCount = sum(nrow(`Month_of_Completion`))
  ) |>
  select(`State`,  Total)
#Meettarget=775-df1$Total_Completed_by_DATE
View(df_filtered)

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
dateplot





#Summary by DATE OF BIRTH----

summary_Date <-  df1  %>%
  #  filter ( `Complete1` =='Complete' ) %>%
  #  wardds<- c("Zango","Tudun Wazurci","Dorayi", "Fagge D2","Gobirawa")
  #summary_Date <-  df1  %>%
  # filter ( `Complete1` =='Complete' , (`LGA of Address`=="Kano Municipal" | `LGA of Address` == "Tarauni" |`LGA of Address`=="Dala" |`LGA of Address`=="Nassarawa"|`LGA of Address`=="Gwale" |`LGA of Address` =="Fagge"), `Number of Pregnancy New`< 5, !(`Ward` %in% wardds) ) %>%
  #df_filtered <- summary_Date[!(summary_Date$`Ward` %in% wardds), ] %>%
 # group_by(`Month_of_Completion`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Month_of_Completion`))
  ) |>
  select(`Month_of_Completion`,  Total_Completed_by_DATE )
Meettarget=775-df1$Total_Completed_by_DATE
View(summary_Date,Meettarget)

x <- c(summary_Date$`Month_of_Completion`)
y <- c(summary_Date$Total_Completed_by_DATE)

df1$agediff <- date() - df1$`q102: Can you tell us your date of birth?`

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
dateplot




###Summary by LGA----

summary_lga <-  df1  %>%
  #filter ( `Complete1` =='Complete' ) %>%
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
          axis.text.x = element_text(size = 10, color = "#334425"),
          axis.text.y = element_text( size = 10, color = "#334425"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size =10),
          legend.title=element_text(size=8, colour = '#334425'),
          legend.text =element_text(size = 8, colour = '#334425'),
          legend.key.height = unit(1, "cm"))
}

lgplot<-ggplot(summary_lga,
  aes(x=`LOCAL GOVT. AREA`,y=Total_Completed_by_LGA), color=`LOCAL GOVT. AREA`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LOCAL GOVT. AREA`), nudge_y = 20, vjust = -0.5 ) +
  labs(title = "Health Facility Survey",
    #   subtitle = "Plot of Completed interviews by PHC",
      # caption = "Data source : Health Facility Survey, Kano"
    )

lgplot<-lgplot + theme_manuscript() +labs(y= "Total Completed in Local Gov't", x = " ")
lgplot

###Interviewer Name ----




df1$`INTERVIEWER'S NAME` <- gsub("A'ISHA MUKHTAR", "AISHA MUKHTAR", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("HAFSAT RABI'U YARIMA", "HAFSA RABI'U YARIMA", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("07064413583", "HAUWAU BARDE ABDULLAHI", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("FATIMA YAHYA MUHAMMAD", "FATIMA YAHAYA MUHAMMAD", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("AMINASARKIABUBAKAR", "AMINA SARKI ABUBAKAR", df1$`INTERVIEWER'S NAME`)

df1$`INTERVIEWER'S NAME` <- gsub("ABBAS  MAHMOUD AABBAS", "ABBAS MAHMUD ABBAS", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("ABBAS MAHAMMADU ABBAS", "ABBAS MAHMUD ABBAS", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("ABBAS MAHMOUD AABBAS", "ABBAS MAHMUD ABBAS", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("ABBAS MAHMOUD ABBAS", "ABBAS MAHMUD ABBAS", df1$`INTERVIEWER'S NAME`)

df1$`INTERVIEWER'S NAME` <- gsub("MAEYAM ADAMU GARBA", "MARYAM ADAMU GARBA", df1$`INTERVIEWER'S NAME`)


df1$`INTERVIEWER'S NAME` <- gsub("AISHA BELL0 MUHAMMAD", "AISHA BELLO MUHAMMAD", df1$`INTERVIEWER'S NAME`)
df1$`INTERVIEWER'S NAME` <- gsub("AISHA BELLO.MUHAMMAD", "AISHA BELLO MUHAMMAD", df1$`INTERVIEWER'S NAME`)

df1$`INTERVIEWER'S NAME` <- gsub("DANLAMI ZAHARAU", "ZAHARAU DANLAMI", df1$`INTERVIEWER'S NAME`)





summary_int <-  df1  %>%
  #filter ( `Complete_part1` =='Complete'  ) %>%
  group_by(`NAME OF HEALTH FACILITY`,`INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed = n()
  ) |>
  select(`NAME OF HEALTH FACILITY`,`INTERVIEWER'S NAME`,Total_Completed )
View(summary_int)


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

lgplot1<-ggplot(summary_int, aes(x=reorder(`INTERVIEWER'S NAME`,-Total_Completed),Total_Completed, fill=`NAME OF HEALTH FACILITY`) )+
  geom_bar(stat = "identity")+
  # geom_text(aes(label = paste0(`q124b: How many pregnancies have you had`," Pregnancies")), nudge_y = 20, vjust = -1.9) +
  geom_text(aes(label = paste0(signif(Total_Completed))), nudge_y = 2, vjust = -0.5 ) +
  labs(title = "Health Facility Survey : Number of Completed",
       #subtitle = "From the graph we can see that some respondents visits the health facility from different LGAs even from outside of the selected LGAs, this might be due to  among other \n reasons proximity of the residence to the Health Facilities, We could not classify some other ",
       caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1<-lgplot1 + theme_manuscript() +labs(y= "Total Completed by Interviewer", x = " ")+ theme(axis.text.x = element_text(angle = 90)) 
lgplot1




###Summary by Previous Pregnancies----
df1$`q124b: How many pregnancies have you had` <- gsub("^01", "1", df1$`q124b: How many pregnancies have you had`)
df1$`q124b: How many pregnancies have you had` <- gsub("^02", "2", df1$`q124b: How many pregnancies have you had`)
df1$`q124b: How many pregnancies have you had` <- gsub("^03", "3", df1$`q124b: How many pregnancies have you had`)
df1$`q124b: How many pregnancies have you had` <- gsub("^04", "4", df1$`q124b: How many pregnancies have you had`)

summary_lga_sel <-  df1  %>%
  #filter ( `Complete_part1` =='Complete'  ) %>%
  group_by(`NAME OF HEALTH FACILITY`,`INTERVIEWER'S NAME`,`q124b: How many pregnancies have you had`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`q124b: How many pregnancies have you had`))
  ) |>
  select(`NAME OF HEALTH FACILITY`,`q124b: How many pregnancies have you had`,  Total_Completed_by_LGA )
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

lgplot1<-ggplot(summary_lga_sel,
                aes(x=reorder(`q124b: How many pregnancies have you had`,-Total_Completed_by_LGA),Total_Completed_by_LGA, fill = `NAME OF HEALTH FACILITY`)) +
  geom_bar(stat = "identity")+
 # geom_text(aes(label = paste0(`q124b: How many pregnancies have you had`," Pregnancies")), nudge_y = 20, vjust = -1.9) +
  geom_text(aes(label = paste0( `q124b: How many pregnancies have you had` ," Pregnancies :", signif(Total_Completed_by_LGA))), position = position_stack(vjust = 0.5) ) +
  labs(title = "Health Facility Survey : Number of Previous Pregnancies",
       #subtitle = "From the graph we can see that some respondents visits the health facility from different LGAs even from outside of the selected LGAs, this might be due to  among other \n reasons proximity of the residence to the Health Facilities, We could not classify some other ",
       caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1<-lgplot1 + theme_manuscript() +labs(y= "Total Completed by No of Pregnancies", x = " ")
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
df_filtered <- df1[!(df1$`LOCAL GOVT. AREA` %in% lgas), ]
print(df_filtered)

summary_lga_sel <-  df1  %>%
  filter ( `Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa' ) %>%
  group_by(`LOCAL GOVT. AREA`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`LOCAL GOVT. AREA`))
  ) |>
  select(`LOCAL GOVT. AREA`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

x <- c(summary_lga$`LOCAL GOVT. AREA`)
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
                 aes(reorder(`LOCAL GOVT. AREA`, -Total_Completed_by_LGA),Total_Completed_by_LGA), color=`LGA of Address`) +
  geom_bar(stat = "identity", fill="#334425")+
  geom_text(aes(label = signif(Total_Completed_by_LGA)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label = `LOCAL GOVT. AREA`), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey : Completed by LGA's (Selected) of Residence",
       #caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1ns<-lgplot1ns + theme_manuscript() +labs(y= "Total Completed by Local Gov't of Residence", x = " ")
lgplot1ns



###Summary by Wards of Address out of scope----

  
  Allowedwards <-c(
    'Adakawa',
    'Bakin Ruwa',
    'Dala',
    'Dogon Nama',
    'Gwammaja',
    'Kabuwaya',
    'Kantudu',
    'Kofar Mazugal',
    'Kofar Ruwa',
    'Madigawa',
    'Yalwa',
    'Fagge A',
    'Fagge B',
    'Fagge C',
    'Fagge D1',
    'Kwachiri',
    'Rijiyar Lemo',
    'Sabon Gari East',
    'Sabon Gari West',
    'Yammata',
    'Dandago',
    'Diso',
    'Galadanchi',
    'Goron Dutse',
    'Gwale',
    'Gyaranya',
    'Kabuga',
    'Mandawari',
    'Sani Mai Nagge',
    'Chedi',
    'Dan Agundi',
    'Gandu Albasa',
    'Jakara',
    'Kan Karofi',
    'Shahuchi',
    'Sharada',
    'She She',
    'Tudun Nufawa',
    'Yakasai',
    'Zaitawa',
    'Dakata',
    'Gama',
    'Gawuna',
    'Giginyu',
    'Gwagwarwa',
    'Hotoro North',
    'Hotoro South',
    'Kaura Goje',
    'Kawaji',
    'Tudun Wada',
    'Tudun Murtala',
    'Babban Giji',
    'Darmanawa',
    'Daurawa',
    'Gyadi Gyadi Arewa',
    'Gyadi Gyadi Kudu',
    'Hotoro',
    'Tarauni',
    'Unguwa Uku Cikin Gari',
    'Unguwa Uku Kauyen Alu',
    'Unguwar Gano'
    
  )
summary_lga_sel <-  df1  %>%
  filter ( (`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Giginyu'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') | !(`Ward` %in% Allowedwards) ) %>%
  group_by(`NAME OF HEALTH FACILITY`, `Ward`, `Serial Number`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`Ward`))
  ) |>
  select(`NAME OF HEALTH FACILITY`,`Ward`,  Total_Completed_by_LGA )
View(summary_lga_sel)
print(sum(summary_lga_sel$Total_Completed_by_LGA))

#Out of Scope Addresses ----
summary_lga_sel <-  df1  %>%
  filter ( (`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Giginyu'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') | !(`Ward` %in% Allowedwards) ) %>%
  group_by(`NAME OF HEALTH FACILITY`, `Ward`, `Serial Number`, `RESPONDENTS ADDRESS`) %>%
  summarise(
    Total_Completed_by_LGA = n(),
    Positive_Count = sum(nrow(`Ward`))
  ) |>
  select(`NAME OF HEALTH FACILITY`,`Ward`,  `Serial Number`, `RESPONDENTS ADDRESS` )
View(summary_lga_sel)


print(sum(summary_lga_sel$Total_Completed_by_LGA))


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
                  aes(reorder(`NAME OF HEALTH FACILITY`, -Total_Completed_by_LGA),Total_Completed_by_LGA, fill=`Ward` )) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste(`Ward`, signif(Total_Completed_by_LGA))), position = position_stack(vjust = 0.5) ) +
 # geom_text(aes(label = `Ward`), nudge_y = 5, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey : Completed by LGA's (Selected) of Residence",
    #caption = "Data source : Health Facility Survey, Kano"
  )

lgplot1ns<-lgplot1ns + theme_manuscript() +labs(y= "Total Completed in Cross Sectional Wards", x = " ")
lgplot1ns




lgplot1/(lgplot1s + lgplot1ns)


lgplot1s/(lgplot1nsw)/  lgplot1sfw


###Summary by LGA of Address filtering out 5 wards that are out of scope----

summary_lga_sel <-  df1  %>%
  filter ( `Ward` == 'Zango' | `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa')  %>%
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
 # filter ( !(`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') , (`Ward` %in% Allowedwards) ) %>%
  group_by(`NAME OF HEALTH FACILITY`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`NAME OF HEALTH FACILITY`))
  ) |>
  select(`NAME OF HEALTH FACILITY`,  Total_Completed_by_PHC )

View(summary_phc)

phcplot<-ggplot(summary_phc,
                aes(x=reorder(`NAME OF HEALTH FACILITY`,-Total_Completed_by_PHC),y=Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`NAME OF HEALTH FACILITY` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
       #subtitle = "Plot of Completed interviews by PHC",
       caption = "Data source : Health Facility Survey, Kano"
    ) +
  theme_manuscript() +labs(y= "Total Completed in PHC", x = "")
phcplot

###Summary by PHC after filter----

summary_phc <-  df1  %>%
  filter ( !(`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Giginyu'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') , (`Ward` %in% Allowedwards) ) %>%
  group_by(`NAME OF HEALTH FACILITY`) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Positive_Count = sum(nrow(`NAME OF HEALTH FACILITY`))
  ) |>
  select(`NAME OF HEALTH FACILITY`,  Total_Completed_by_PHC )

View(summary_phc)

x <- c(summary_phc$`NAME OF HEALTH FACILITY`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot1<-ggplot(summary_phc,
                aes(x=reorder(`NAME OF HEALTH FACILITY`,-Total_Completed_by_PHC),y=Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`NAME OF HEALTH FACILITY` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
    #subtitle = "Plot of Completed interviews by PHC",
    caption = "Data source : Health Facility Survey, Kano"
  )

phcplot1<-phcplot1 + theme_manuscript() +labs(y= "Total Completed in PHC After Removing wrong wards", x = "")
phcplot1

phcplot/phcplot1

#Summary by Dates All Months----

df1$Datef <- as.Date(df1$`Date`, format = "%m/%d/%Y")
View(df1)
summary_Date_ <-  df1  %>%
  #  filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"   ) ) %>%
  group_by(`Date`, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(`Date`)),
    InterviewCount = sum(nrow(`INTERVIEWER'S NAME`))
  ) |>
  select( `Date`, `INTERVIEWER'S NAME`, Total_Completed_by_DATE )
View(summary_Date_)

summary_Date_$MonthNum <- gsub("/","" ,substr(summary_Date_$`Date`, 6, 7))

summary_Month_ <-  summary_Date_  %>%
  group_by(MonthNum, `INTERVIEWER'S NAME`) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)
    
  )

View(summary_Month_)



summary_Month <-  summary_Date_  %>%
  group_by(MonthNum) %>%
  summarise(
    Total_Completed_in_Month = sum(Total_Completed_by_DATE)
    
  )

View(summary_Month)



df1$MonthNum <- gsub("/","" ,substr(df1$`Date`, 4, 5))

startd <- df1 %>%
  group_by(`INTERVIEWER'S NAME`) %>%
  #mutate(Start_Date = min(Date, na.rm = TRUE)) %>%
 # mutate(End_Date = max(Date, na.rm = TRUE)) %>%
 # mutate(Days_worked = End_Date - Start_Date) %>%
  ungroup()
View(startd)

summary_Date_RA <-  startd  %>%
 # filter ((`Settlement Type` =="Formal" |`Settlement Type` =="Informal" | `Settlement Type` =="Slum"  ) ) %>%
  group_by(`MonthNum`,`INTERVIEWER'S NAME`, Date) %>%
  summarise(
    Total_Completed_by_DATE = n(),
    myCount = sum(nrow(Date)),
  ) |>
  select( `MonthNum`, Date, `INTERVIEWER'S NAME` , Total_Completed_by_DATE)
View(summary_Date_RA)


#Summary table for Completion by Month by Interviewer ----
table_result_month <- summary_Month_  %>%
  spread(MonthNum, Total_Completed_in_Month)
View(table_result_month)


#Summary table for Completion by Date by Interviewer ----
table_result_day <- summary_Date_RA  %>%
  spread(Date, Total_Completed_by_DATE)
View(table_result_day)




summary_Date_RA$Date1 <- as.Date(summary_Date_RA$Date)

# Create a line plot
line_plot <- ggplot(summary_Date_RA, aes(x = Date1, y = Total_Completed_by_DATE, color = `INTERVIEWER'S NAME`)) +
  geom_line() +
  labs(title = "Completion Rates Over Time",
       x = "Date1",
       y = "Total Completed",
       color = "Date") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))

# Print the line plot
print(line_plot)


bar_plot <- ggplot(summary_Date_RA, aes(x = `INTERVIEWER'S NAME` , y = Total_Completed_by_DATE, fill = Date)) +
  geom_bar(stat = "identity") +
  labs(title = "Completion Rates Over Time",
       x = "Date",
       y = "Total Completed",
       fill = "Date") + theme(axis.text.x = element_text(angle = 90))

# Print the bar plot
print(bar_plot)



time_series_plot <- ggplot(summary_Date_RA, aes(x = Date, y = Total_Completed_by_DATE, color = `INTERVIEWER'S NAME`, group = `INTERVIEWER'S NAME`)) +
  geom_line() +
  geom_point() +
  labs(title = "Completion Rates Over Time",
       x = "Date",
       y = "Total Completed",
       color = "Interviewer") +
  theme_minimal()

# Print the time series plot
print(time_series_plot)



plot(summary_Month_$`INTERVIEWER'S NAME`,summary_Month_$Total_Completed_in_Month) 

# Plot by DATE All Month----
mdateplot <- ggplot(table_result_month,
                    aes(reorder(MonthNum, -Total_Completed_in_Month), Total_Completed_in_Month)) +
  geom_bar(stat = "identity", fill = "#661133") +
  geom_text(aes(label = paste("Month", MonthNum, " : ", signif(Total_Completed_in_Month))), 
            nudge_y = 0, vjust = -0.7) +
  labs(subtitle = "Plot of Achievement by Month")

dateplot1 <- mdateplot + theme_manuscript() + labs(y = "Total Completed by Month", x = "Month of Completion")
dateplot1

#Summary table for Completion by Date by Interviewer ----
table_result <- summary_Date_RA  %>%
  spread(`Date`, Total_Completed_by_DATE)
View(table_result)

phcplot1/phcplot



###Summary by PHC Incomplete Address----

summary_phc <-  df1  %>%
  mutate(Length_Address = length(`RESPONDENTS ADDRESS`)) %>%
  filter ( Length_Address <20 ) %>%
  group_by(`NAME OF HEALTH FACILITY`, `INTERVIEWER'S NAME`,Length_Address) %>%
  summarise(
    Total_Completed_by_PHC = n(),
    Add_Count = sum(nrow(`Length_Address` < 20))
  ) |>
select(`NAME OF HEALTH FACILITY`, `INTERVIEWER'S NAME`, Total_Completed_by_PHC, Add_Count )

View(summary_phc)

x <- c(summary_phc$`NAME OF HEALTH FACILITY`)
y <- c(summary_phc$Total_Completed_by_PHC)

phcplot<-ggplot(summary_phc,
                aes(x=reorder(`NAME OF HEALTH FACILITY`,-Total_Completed_by_PHC),y=Total_Completed_by_PHC), ) +
  geom_bar(stat = "identity" , fill="#991199")+
  geom_text(aes(label = signif(Total_Completed_by_PHC)), nudge_y = 2, vjust = -0.5) +
  geom_text(aes(label =`NAME OF HEALTH FACILITY` ), nudge_y = 20, vjust = -0.5 ) +
  labs(#title = "Health Facility Survey",
    #subtitle = "Plot of Completed interviews by PHC",
    caption = "Data source : Health Facility Survey, Kano"
  )

phcplot<-phcplot + theme_manuscript() +labs(y= "Total Completed in PHC", x = "")
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

# Corrected code to recode values for rows where State is "Kano"
df1 <- df1 %>%
  mutate(`q124b_new: How many pregnancies have you had` = ifelse(
    State == "Kano",
    recode(`q124b: How many pregnancies have you had`, 
           "1" = "0", 
           "2" = "1", 
           "3" = "2", 
           "4" = "3",
           "5" = "4"),
    `q124b: How many pregnancies have you had`
  ))




summary_Age <-  df1  %>%
  #filter ( `Complete_part1` =='Complete' ) %>%
  group_by(`State`,`q101: How old were you on your last birthday? AGE AT LAST BIRTHDAY (IN YEARS)`, `q124b: How many pregnancies have you had`) %>%
  summarise(
    Total_Completed_by_AGE = n(),
    Positive_Count = sum(nrow(`q101: How old were you on your last birthday? AGE AT LAST BIRTHDAY (IN YEARS)`))
  ) |>
  select(`State`,`q101: How old were you on your last birthday? AGE AT LAST BIRTHDAY (IN YEARS)`, `q124b: How many pregnancies have you had`, Total_Completed_by_AGE )

View(summary_Age)


ageplot <- ggplot(summary_Age,
                  aes(x = `q101: How old were you on your last birthday? AGE AT LAST BIRTHDAY (IN YEARS)`)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
 # geom_text(aes(label = signif(Total_Completed_by_AGE)), nudge_y = 0.5, vjust = -0.5, size = 3, color = "black") +
  labs(
    y = "Completed Interviews by Age",
    x = "Age of Respondents",
    title = "Health Facility Survey",
    subtitle = "Plot of Respondents' Age Distribution",
    caption = "Data source: Health Facility Survey, Kano"
  ) +
  theme_manuscript()
ageplot

df1$`q124b: How many pregnancies have you had` <- as.numeric(df1$`q124b: How many pregnancies have you had`)

# Summary by Numbers of Prgnancies ----
summary_Age <- df1 %>%
  group_by( `State`,`q124b_new: How many pregnancies have you had` ) %>%
  summarise(
    Total_Completed_by_AGE = n()
  ) 

View(summary_Age)

ageplot <- ggplot(summary_Age,
                  aes(`q124b_new: How many pregnancies have you had`,Total_Completed_by_AGE)) +
  geom_bar(stat = "identity", fill = "#223377") +
  facet_wrap(State ~ .) +
  geom_text(aes(label = Total_Completed_by_AGE), vjust = -0.5, size = 3, hjust = 1) +
  labs(
    y = "Completed Interviews by Age",
    x = "Plots by Age of Respondents",
    title = "Health Facility Survey"
  ) +
  theme_manuscript()

ageplot





#Summary by Education----

summary_Education <-  df1  %>%
  #filter ( `Complete_part1` =='Complete' ) %>%
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
#  filter ( `Complete_part1` =='Complete' ) %>%
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
#Test Status by Health Facility  ----
wardresult <- df1 %>%
 # filter ( !(`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') , (`Ward` %in% Allowedwards) ) %>%
  filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ))%>%
  group_by(`NAME OF HEALTH FACILITY...6`, `q503: RESULT`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`NAME OF HEALTH FACILITY...6`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

# Create a basic bar plot with proportions and colored bars
plot_wardresult <- ggplot(wardresult, aes(x = `NAME OF HEALTH FACILITY...6`, y = Proportion, fill = `q503: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5),
            vjust = -0.5
  ) +
  #labs(title = "Test Results by  Mosquito net used yesterday", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
  scale_fill_manual(values = colors) + theme_manuscript() + theme(axis.text.x = element_text(angle = 90)) # Apply the color scale

# Print the plot
print(plot_wardresult)



#Test Status by Ward  ----
wardresult <- df1 %>%
  filter ( !(`Ward` == 'Zango'| `Ward` == 'Dorayi'| `Ward` == 'Tudun Wazurci'| `Ward` == 'Fagge D2'| `Ward` == 'Gobirawa') , (`Ward` %in% Allowedwards) ) %>%
  filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE' ))%>%
  group_by(`Ward`, `q503: RESULT`) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  group_by(`Ward`) %>%
  mutate(Total = sum(Count), Proportion = Count / Total * 100)

colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

# Create a basic bar plot with proportions and colored bars
plot_wardresult <- ggplot(wardresult, aes(x = `Ward`, y = Proportion, fill = `q503: RESULT`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)),
            position = position_stack(vjust = 0.5)
  ) +
  #labs(title = "Test Results by  Mosquito net used yesterday", x = " Did you sleep inside a mosquito net last night? ", y = "Proportion of Result") +
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




  #PHC by LGA ----

  testedbyWard_sel <- df1 %>%
  #  filter ( Selected_LGA=='No', (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'))%>%
    group_by(`LGA of Address`, `Health_Facility_Name`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`LGA of Address`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `LGA of Address`, y = Proportion, fill = `Health_Facility_Name`)) +
    geom_col(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", Proportion)),
              position = position_stack(vjust = 0.5),
              vjust = -0.2
    ) +
    geom_text(aes(label = signif(Count)),
              position = position_stack(vjust = 0.5),
              vjust = -1.6
    ) +
    geom_text(aes(label = `Health_Facility_Name`),
              position = position_stack(vjust = 0.5),
              vjust = -4.0
    ) +

    theme_manuscript()  # Apply the color scale

  # Print the plot
  print(p_testedbyWard_sel)




  #PHC by LGA Side by side----

  testedbyWard_sel <- df1 %>%
 #   filter ( Selected_LGA=='No', (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'))%>%
    group_by(`LOCAL GOVT. AREA`, `NAME OF HEALTH FACILITY`) %>%
    summarize(Count = n()) %>%
    ungroup() %>%
    group_by(`NAME OF HEALTH FACILITY`) %>%
    mutate(Total = sum(Count), Proportion = Count / Total * 100)
  View(testedbyWard)



  colors <- c("NEGATIVE" = "green3", "POSITIVE" = "red3")

  # Proportion plot
  p_testedbyWard_sel <- ggplot(testedbyWard_sel, aes(x = `NAME OF HEALTH FACILITY`, y = Proportion, fill = `LOCAL GOVT. AREA`)) +
    geom_bar(stat = "identity", alpha = 0.75) +

  geom_text(aes(label = sprintf("%.1f%%", Proportion)), vjust = 1.5,
            position = position_stack(), size = 4) +

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
   # filter ( (`q503: RESULT` =='POSITIVE' |  `q503: RESULT` =='NEGATIVE'), (`Month_of_Completion` == 'Aug, 2023'|`Month_of_Completion` == 'Sep, 2023'), `LGA of Address`=='KMC' )%>%
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


    #Loading the map-----
  #View(mymap2)
  
  # breaks <- c(1, 10, 20, 30, 40)
  #  colors <- c("yellow","orange","red","red3" , "red4")
  #Plotting the map
  mymapplot<- ggplot(data = shape) +
    # geom_sf(data=gps_sf, aes( color=WardName), show.legend = FALSE) +
    #geom_sf_text(data=mymap2,aes(label = WardName, geometry=geometry))+
    geom_point(data=gps_sf, aes(x="HOUSEHOLD COORDINATE- Longitude", y="HOUSEHOLD COORDINATE- Latitude"))+
    #scale_fill_manual(values = colors) +
    #  geom_sf_text(data=mymap2,aes(label = Total_Count, geometry=geometry))+
    theme_manuscript()
  mymapplot
  
  
  
  #Plotting the map
  ggplot() +
    geom_sf(data=mymap2,mapping = aes(fill= cut(Total_Count, breaks = breaks) , geometry=geometry, colors="white"), show.legend = TRUE) +
    # geom_sf_text(data=mymap2,aes(label = Total_Count, geometry=geometry)) +
    geom_sf_text(data=mymap2,aes(label = NAME_1 , geometry=geometry) ) +
    scale_fill_manual(values = colors)
  #+geom_point(data = df1, aes(x="latitude", y="longitude"))
  mymapplot
  
  p <-  mymapplot + geom_point(aes(df1$latitude, df1$longitude))
  
  ggplotly(p) 
  
  
  library(haven)
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(sf)
  library(ggplot2)
  library(ggmap)
  library(plotly)

    dframe<-read_csv('/Users/user/Downloads/UrbanMalariaHousehol_DATA_LABELS_2024-05-28_1844.csv')%>%
    select(`Serial Number`, `INTERVIEWER'S NAME...15` , `Settlement Type` ,Ward,`HOUSEHOLD COORDINATE- Latitude`,`HOUSEHOLD COORDINATE- Longitude` )
  View(dframe)
  
  
  dframe1<-read_csv('/Users/user/Downloads/UrbanMalariaHousehol-DataUpdate2_DATA_LABELS_2024-01-14_1648_Wet_season.csv')%>%
    select(`Serial Number`, `INTERVIEWER'S NAME` , `Settlement Type` ,Ward,`HOUSEHOLD COORDINATE- Latitude`,`HOUSEHOLD COORDINATE- Longitude` )
  View(dframe1)
  
  
  shape <- st_read(dsn = "/Users/user/Downloads/Kano_metro_ward_sixLGAs/", layer = "Kano_metro_ward_sixLGAs")
  View(shape)
  
  #Dry season rename of Lat and Long
  names(dframe)[names(dframe) == "HOUSEHOLD COORDINATE- Latitude"] <- "Latitude"
  names(dframe)[names(dframe) == "HOUSEHOLD COORDINATE- Longitude"] <- "Longitude"
  
  
  #Wet season rename of Lat and Long
  names(dframe1)[names(dframe1) == "HOUSEHOLD COORDINATE- Latitude"] <- "Latitude"
  names(dframe1)[names(dframe1) == "HOUSEHOLD COORDINATE- Longitude"] <- "Longitude"
  
  
  dframe <- dframe%>% dplyr::filter((`Settlement Type`=="Formal" | `Settlement Type`=="Informal" | `Settlement Type`=="Slum"),(!is.na(Latitude) | !is.na(Longitude)))
  dframe1<- na.omit(dframe[c("Longitude", "Latitude")])
  
  
  #Left joining the positive count with map polygon

 #shape=left_join(dframe,shape,by=c("Ward"="WardName"))

#define the map theme function
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

final_centroid_data<-read_csv('/Users/user/Downloads/kano_final_centroids_data.csv')
  

dff <- final_centroid_data%>%
  filter(!is.na(longitude) & !is.na(latitude))
#remove other wards from the shape file
shp <- shape
#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("longitude", "latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)




dff <- dframe%>%
  filter(!is.na(Longitude) & !is.na(Latitude))
#remove other wards from the shape file
shp <- shape
#define the coordinates
gps <- sf::st_as_sf(dff, coords=c("Longitude", "Latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shp) <- 4326
intersects_a1 <- st_intersection(gps, shp)


invalid_gps <- dframe %>%
  filter(!`Serial Number` %in% intersects_a1$Serial.Number)

write_csv(invalid_gps,"Invalid GPS Captured")



invalid_gps__ <- intersects_a1 %>%
  filter(!(WardName=="Giginyu" & WardName=="Dorayi" & WardName=="Fagge D2" & WardName=="Gobirawa" & WardName=="Zango"))




invalid_gps_ <- invalid_gps %>%
    group_by(Ward) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward, total_issues )

ggplot(invalid_gps_, aes(reorder(Ward,-total_issues), total_issues ), colour="brown", fill="khaki") +
  geom_bar(stat = "identity", colour="brown", fill="tomato") +
  geom_point(aes(size=total_issues), color="brown" , colour="khaki")+
  scale_size_continuous(range = c(5, 10), guide = FALSE)  +
  geom_text(size=3,color="white",aes(label = paste0(total_issues)))+
  
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Quality Check Update",
       subtitle = "GPS Position of households surveyed outside of Kano Metropolis",
       caption = "Data source : Dry Season Cross Sectional Survey, Kano",
  )


serials_ <- invalid_gps %>%
  group_by(Ward, `INTERVIEWER'S NAME...15`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward,`INTERVIEWER'S NAME...15`, total_issues )

ggplot(serials_, aes(reorder(`INTERVIEWER'S NAME...15`,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(aes(size=total_issues, color=Ward))+
  scale_size_continuous(range = c(5, 13), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=2,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Cross Sectional Survey QA Update",
       subtitle = "GPS Position of households surveyed outside of Kano Metropolis/Invalid GPS by RAs",
       caption = "Data source : Cross Sectional Survey, Kano",
  )






all <- shp %>%
  filter(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa"  )

zangodf <- intersects_a1 %>%
  filter(Ward=="Zango")

zango <- all %>%
  filter(WardName=="Zango")
  
  
Giginyudf <- intersects_a1 %>%
  filter(Ward=="Others")

Giginyu <- all %>%
  filter(WardName=="Giginyu")

Faggedf <- intersects_a1 %>%
  filter(Ward=="Fagge D2")

Fagge <- all %>%
  filter(WardName=="Fagge D2")

Gobirawadf <- intersects_a1 %>%
  filter(Ward=="Gobirawa")

Gobirawa <- all %>%
  filter(WardName=="Gobirawa")


Dorayidf <- intersects_a1 %>%
  filter(Ward=="Dorayi")

Dorayi <- all %>%
  filter(WardName=="Dorayi")


#plot
ggplot(all)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, colour=Settlement.Type), size=0.5, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  #geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey Geo-points of Households Surveyed",
       subtitle = "GPS Position of HHs surveyed in Kano by Settlement type",
       caption = "Data source : Wet Season Cross Sectional Survey, Kano"
  )+
  coord_sf()



#plot
ggplot(all)+
  geom_point(data = intersects_a1,  aes(geometry = geometry, colour=settlement_type), size=0.5, stat= "sf_coordinates")+
  #scale_color_manual(values = c(formal = "#00A08A", informal = "#F2A6A2" , slum = "#923159"))+
  geom_sf_text(data=all,aes(label = WardName , geometry=geometry) ) +
  geom_sf_text(data=intersects_a1, aes(label = ea_names , geometry=geometry),size=1 ) +
  geom_sf(fill = NA) +
  guides(size = FALSE)+
 map_theme()+
 theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey GeoPoints of EAs Centroid",
       subtitle = "GPS Position of EA's surveyed in Kano by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()



zango <- all %>%
  filter(WardName=="Zango")

zangodf <- intersects_a1 %>%
  filter(Ward=="Zango")

#plot
ggplot(zango)+
    geom_point(data = zangodf,  aes(geometry = geometry, colour = Settlement.Type), size=1.3, stat= "sf_coordinates")+
    scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=zango,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
     subtitle = "GPS Position of households surveyed in Zango by Settlement type",
     caption = "Data source : Cross Sectional Survey, Kano"
  )+
coord_sf()


#####Dorayi Plot

#plot
ggplot(Dorayi)+
  geom_point(data = Dorayidf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Dorayi,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Dorayi by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()

#####Giginyu Plot

ggplot(Giginyu)+
  geom_point(data = Giginyudf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Giginyu,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Giginyu by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#####Fagge Plot

ggplot(Fagge)+
  geom_point(data = Faggedf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Fagge,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Fagge D2 by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#####Gobirawa Plot

ggplot(Gobirawa)+
  geom_point(data = Gobirawadf,  aes(geometry = geometry, colour = Settlement.Type), size=1, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf_text(data=Gobirawa,aes(label = WardName , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed in Gobirawa by Settlement type",
       caption = "Data source : Cross Sectional Survey, Kano"
  )+
  coord_sf()


#Nigeria Shape File
shapengn <- read_sf(dsn = "/Users/user/Downloads/NGPR7ADT/shapefile/", layer = "gadm36_NGA_1")



dff_ <- invalid_gps%>%
  filter(!is.na(Longitude) & !is.na(Latitude))
#remove other wards from the shape file
#define the coordinates
gps <- sf::st_as_sf(dff_, coords=c("Longitude", "Latitude"), crs=4326)
# Perform spatial intersection
st_crs(gps) <- 4326
st_crs(shapengn) <- 4326
intersects_ng <- st_intersection(gps, shapengn)

nga <- shapengn %>%
  filter(NAME_1=="Kano")

#####NGN Plot

ggplot(nga)+
  geom_point(data = intersects_ng,  aes(geometry = geometry, colour = Settlement.Type), size=0.4, stat= "sf_coordinates")+
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  geom_sf(fill = NA) +
  geom_sf(data = all ,fill = NA) +
  geom_sf_text(data=nga,aes(label = NAME_1 , geometry=geometry) ) +
  guides(size = FALSE)+
  map_theme()+
  theme_manuscript()+
  ylab("")+
  xlab("")+
  labs(title = "Cross Sectional Survey",
       subtitle = "GPS Position of households surveyed falling outside of Kano Metropolis within and outside Kano state",
       caption = "Data source : Dry Season Cross Sectional Survey, Kano"
  )+
  coord_sf()




Wrongwards <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(Ward) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward, total_issues )

ggplot(Wrongwards, aes(reorder(Ward,-total_issues), total_issues ), colour="brown", fill="khaki") +
  geom_bar(stat = "identity", colour="brown", fill="tomato") +
  geom_point(aes(size=total_issues), color="brown" , colour="khaki")+
  scale_size_continuous(range = c(5, 10), guide = FALSE)  +
  geom_text(size=3,color="white",aes(label = paste0(total_issues)))+

  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Quality Check Update",
       subtitle = "GPS Position of households surveyed outside of supposed wards",
       caption = "Data source : Cross Sectional Survey, Kano",
  )

Serials <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(`INTERVIEWER.S.NAME...15`,Ward) %>%
  select(1,2,3,4,7,14)


library(openxlsx)

Serials <- do.call(cbind, Serials) 
write.xlsx(Serials, "Cross_sectional_errors.xlsx") 


write.csv(Serials, "Cross sectional households surveyed outside of supposed wards_1.csv", row.names = FALSE)

Wrongwards <- intersects_a1 %>%
  filter(!(WardName=="Zango"| WardName=="Dorayi" | WardName =="Giginyu" | WardName =="Fagge D2" | WardName =="Gobirawa")) %>%
  group_by(Ward, `INTERVIEWER.S.NAME`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(Ward,`INTERVIEWER.S.NAME`, total_issues )
ggplot(Wrongwards, aes(reorder(`INTERVIEWER.S.NAME`,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(aes(size=total_issues, color=Ward))+
  scale_size_continuous(range = c(5, 13), guide = FALSE)  +
 # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=2,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
 # theme_minimal()   # Use a minimal theme
 # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Cross Sectional Survey QA Update",
       subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       caption = "Data source : Cross Sectional Survey, Kano",
  )


dframelong<-read_csv('/Users/user/Downloads/UrbanMalariaLongitud_DATA_LABELS_2024-05-14_1753.csv')

dframelongib<-read_csv('/Users/user/Downloads/UrbanMalariaLongitud_DATA_LABELS_2024-05-14_1751.csv')



dframelong <- dframelong %>%
  group_by(`Serial Number`) %>%
  fill(5:7)


dframelong <- dframelong %>%
  mutate(`INTERVIEWER'S NAME...15` = recode(`INTERVIEWER'S NAME...15`, "AISHa AHMAD" = "AISHA AHMAD", "LAWAN GAMBO" = "LAWAN GAMBO MUHAMMAD", "ABDUL" = "ABDUL", "KABIRU IDRIS" = "KABIRU IDRIS GARBA", "BULKISU"= "BILKISU", "MASHKUR AHAMAD" = "MASHKUR AHMAD", "AISHA ABDULLAHI"= "AISHA MUHAMMAD", "IDRIS" = "KABIRU IDRIS GARBA"))


dframelong <- dframelong %>%
  mutate(`INTERVIEWER'S NAME...31` = recode(`INTERVIEWER'S NAME...31`, "AISHa AHMAD" = "AISHA AHMAD", "LAWAN GAMBO" = "LAWAN GAMBO MUHAMMAD", "ABDUL" = "ABDUL", "KABIRU IDRIS" = "KABIRU IDRIS GARBA", "BULKISU"= "BILKISU", "MASHKUR AHAMAD" = "MASHKUR AHMAD", "AISHA ABDULLAHI"= "AISHA MUHAMMAD", "ASHA ABDULLAHI" = "AISHA MUHAMMAD","28-4-24"="AISHA AHMAD" ,"AISHA"="AISHA AHMAD", "IDRIS" = "KABIRU IDRIS GARBA"))


Baseline <- dframelong %>%
  filter(`Event Name` == "Baseline", `Complete?...25`=="Complete")%>%
group_by(`Event Name`,`INTERVIEWER'S NAME...15`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(`Event Name`,`INTERVIEWER'S NAME...15`, total_issues )

ggplot(Baseline, aes(reorder(`INTERVIEWER'S NAME...15`,-total_issues), total_issues )) +
  geom_bar(stat = "identity", colour="brown", fill="khaki") +
  geom_point(aes(size=total_issues))+
  scale_size_continuous(range = c(5, 13), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=4,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )




followup <- dframelong %>%
  filter(`Complete?...39`=="Complete")%>%
  group_by(`Event Name`, `INTERVIEWER'S NAME...31`) %>%
  summarise(
    total_issues = n()
  )%>%
  select(`Event Name`, `INTERVIEWER'S NAME...31`, total_issues )


colnames(Baseline)[colnames(Baseline) == "INTERVIEWER'S NAME...15"] <- "INTERVIEWER'S NAME"
colnames(followup)[colnames(followup) == "INTERVIEWER'S NAME...31"] <- "INTERVIEWER'S NAME"

baseline_fol <- rbind(followup, Baseline)



ggplot(followup, aes(reorder(`INTERVIEWER'S NAME...31`,-total_issues), total_issues )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =1) +
  geom_point(aes(size=total_issues, color=`Event Name`))+
  
  scale_size_continuous(range = c(3, 10), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=4,colour="white",aes(label = paste0(total_issues)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )


ggplot(baseline_fol, aes(reorder(`INTERVIEWER'S NAME`,-total_issues), total_issues )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =1) +
  geom_point(aes(size=total_issues, color=`Event Name`))+
  scale_size_continuous(range = c(1, 6), guide = FALSE)  +
  # geom_line(aes(colour = Ward, group = Ward))+
  geom_text(size=2,colour="white",aes(label = paste0(total_issues)))+
 # geom_text(size=4,colour="white",aes(label = paste0(total_issues.y)))+
  #geom_text(label = Wrongwards$`INTERVIEWER.S.NAME`,size =3, nudge_y = 2, vjust = 0.4, angle = 30) +
  #scale_fill_manual(values = rep("khaki", length(unique(Wrongwards$`INTERVIEWER.S.NAME`)))) +  # Set fill color to yellow
  # theme_minimal()   # Use a minimal theme
  # theme(legend.position = "none")  # Remove legend
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )



######

Baselinew <- dframelong %>%
  filter(`Event Name` == "Baseline", `Complete?...25`=="Complete")%>%
  group_by(WARD, `Event Name`,`INTERVIEWER'S NAME...15`) %>%
  fill(WARD)%>%
  summarise(
    total_issues = n()
  )%>%
  select(WARD, `Event Name`, total_issues )


#Follow up script

followupw <- dframelong %>%
  filter(`Complete?...39`=="Complete")

summary_bs_fl <- followupw %>%
  group_by(WARD, `Event Name`,`INTERVIEWER'S NAME...31`) %>%
  fill(WARD)%>%
  summarise(
    total_issues = n()
  )%>%
  select(WARD , `Event Name`, total_issues )


colnames(Baselinew)[colnames(Baselinew) == "INTERVIEWER'S NAME...15"] <- "INTERVIEWER'S NAME"
colnames(followupw)[colnames(followupw) == "INTERVIEWER'S NAME...31"] <- "INTERVIEWER'S NAME"

baseline_folw <- rbind(summary_bs_fl, Baselinew)


#####-----Total by Survey type by Ward 


alldf <- dframelong %>%
  filter(`Complete?...39`=="Complete" | `Complete?...25`=="Complete")%>%
  mutate(`INTERVIEWER'S NAME...15` = ifelse(is.na(`INTERVIEWER'S NAME...15`), `INTERVIEWER'S NAME...31`, `INTERVIEWER'S NAME...15`))
  

alldf <- dframelongib %>%
  filter(`Complete?...40`=="Complete" | `Complete?...25`=="Complete")%>%
  mutate(`INTERVIEWER'S NAME...15` = ifelse(is.na(`INTERVIEWER'S NAME...15`), `INTERVIEWER'S NAME...31`, `INTERVIEWER'S NAME...15`))



baseline_folw_sum <- alldf %>%
    group_by(WARD ,`INTERVIEWER'S NAME...15`, `Event Name`)%>%
    summarise(total_count = n())%>%
      select(WARD ,`INTERVIEWER'S NAME...15`, `Event Name`, total_count)

baseline_folw_sum <- baseline_folw_sum %>%
  group_by(`INTERVIEWER'S NAME...15`) %>%
  fill(WARD)




t_sum <- baseline_folw_sum %>%
  group_by(WARD, `Event Name`)%>%
  summarise(total_count = sum(total_count))%>%
  select(WARD , `Event Name`, total_count)
  


ggplot(t_sum, aes(reorder(WARD,-total_count), total_count )) +
  geom_line(aes(colour=`Event Name`, group = `Event Name`),size =0.2) +
  geom_point(aes(size=total_count, color=`Event Name`))+
  scale_size_continuous(range = c(3, 10), guide = FALSE)  +
  geom_text(size=2,colour="white",aes(label = paste0(total_count)))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Numbers of Issues")+
  xlab("Interviewer Name")+
  labs(title = "Longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       #caption = "Data source : Cross Sectional Survey, Kano",
  )



m_sum <-t_sum %>%
  group_by(`Event Name`)%>%
  summarise(total_count = sum(total_count))%>%
  select(`Event Name`, total_count)



ggplot(m_sum, aes(`Event Name`, total_count )) +
  geom_line(aes(colour=`Event Name`),size =0.2) +
  geom_point(aes(size=total_count, color=`Event Name`))+
  scale_size_continuous(range = c(4, 12), guide = FALSE)  +
  geom_text(size=3,colour="white",aes(label = paste0(total_count)))+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 90, size =10, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels by 90 degrees
  ylab("Total Counts")+
  xlab("")+
  labs(title = "Total longitudinal Baseline Update",
       #subtitle = "GPS Position of households surveyed outside of supposed wards by RAs",
       caption = "Data source : Longitudunal Survey, Kano",
  ) + geom_hline(yintercept = 573, color = "red", size = 0.5) +
  annotate("text", x = Inf, y = 573, label = "Target = 573",size=3, hjust = 1.1, vjust = -0.5, color = "blue")



