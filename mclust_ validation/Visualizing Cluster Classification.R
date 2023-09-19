rm(list = ls())

#loading the relevant packages for plotting##

library(ggplot2)
library(ggpubr)
library(fossil)
library(caret)

NuDir <- file.path(Drive, "urban_malaria")
DataDir <-file.path(NuDir, "data", "nigeria")
ProjDir <- file.path(NuDir, "projects")
SampDir <- file.path(ProjDir, "sampling")
DHS_CDir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data', 'DHS')
Raster_Dir <- file.path(NuDir,'data', 'Raster_files')
mod_file = file.path(ProjDir, "mathematical_model")

#loading in the new data set with cluster information from the DHS prevalence and Covariate

newdf <- read.csv(file.path(DHS_CDir, "clust_dataset_11.csv"))

#Extracting the cluster classification              
newdf$prevalence_clusters <- as.numeric(newdf$prevalence_clusters)
newdf$covariate_clusters <- as.numeric(newdf$covariate_clusters)

#Visualizing each cluster classification using Histogram              
hist(newdf$prevalence_clusters)
hist(newdf$covariate_clusters)

#Viisualing the cluster information using simle scatter plot#

ggplot(newdf, aes(x = prevalence_clusters, y = covariate_clusters)) +
  geom_point()


rand.index(newdf$prevalence_clusters, newdf$covariate_clusters)

## Generating the Contigency table 

cluster_table <- table(newdf$prevalence_clusters, newdf$covariate_clusters)

## Converting contigency table into dataframe
cluster_table1 <- as.data.frame(cluster_table)


ggballoonplot(cluster_table1, fill = "value", show.label = TRUE)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

ggballoonplot(cluster_table1, fill = "value")+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)


##Using Row percentage
rowdf <- read.csv(file.path(CDataDir, "clust_row_percent.csv"))

row_cluster_table <- as.data.frame(rowdf)

ggballoonplot(row_cluster_table, fill = "value", show.label = TRUE)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)

##Using Col percentage
coldf <- read.csv(file.path(CDataDir, "clust_col_percent.csv"))

col_cluster_table <- as.data.frame(coldf)

ggballoonplot(col_cluster_table, fill = "value", show.label = TRUE)+
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)


##Scatterplot adding cluster as factor variable##

ggplot(newdf, aes(x = prevalence_clusters, y = covariate_clusters)) +
  geom_point(aes(color = factor(covariate_clusters)))

#Random Forest Plot##
ggplot(data=cluster_table1) + 
  geom_tile(aes(x=Var1, y=Var2, fill=Freq)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4)+
  ggtitle('Prediction Accuracy for Classes in Cross-Validation (Random Forest Model)') +
  xlab('Prevalance Classification') +
  ylab('Covariate Classification')




###Ploting scenarios using facet wrap

#Scene 4 using means of 20,30,40,50
scene94 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene94.csv")
scene104 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene104.csv")
scene114 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene114.csv")
scene154 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene154.csv")
scene204 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene204.csv")
scene404 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene404.csv")


scene4_all <- rbind(scene94,scene104,scene114,scene154,scene204,scene404)

plot4 <- ggplot(scene4_all, aes(x=factor(clusters), y=prevalence, color=factor(clusters) ))+
  geom_boxplot()+
  theme( legend.position = "none" )+
  facet_wrap(~scenario, ncol=3)+
  labs(title= "Clustering classification based on 4 varying means(20,30,40,50)")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Scene 5 using means of 20,30,40,50,60
scene95 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene95.csv")
scene105 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene105.csv")
scene115 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene115.csv")
scene155 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene155.csv")
scene205 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene205.csv")
scene405 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene405.csv")


scene5_all <- rbind(scene95,scene105,scene115,scene155,scene205,scene405)

plot5 <- ggplot(scene5_all, aes(x=factor(clusters), y=prevalence, color=factor(clusters) ))+
  geom_boxplot()+
  theme( legend.position = "none" )+
  facet_wrap(~scenario, ncol=3)+
  labs(title= "Clustering classification based on 5 varying means(20,30,40,50,60)")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Scene 6 using means of 20,30,40,50,60,70
scene96 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene96.csv")
scene106 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene106.csv")
scene116 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene116.csv")
scene156 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene156.csv")
scene206 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene206.csv")
scene406 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene406.csv")


scene6_all <- rbind(scene96,scene106,scene116,scene156,scene206,scene406)

plot6 <- ggplot(scene6_all, aes(x=factor(clusters), y=prevalence, color=factor(clusters) ))+
  geom_boxplot()+
  theme( legend.position = "none" )+
  facet_wrap(~scenario, ncol=3)+
  labs(title= "Clustering classification based on 6 varying means(20,30,40,50,60,70)")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Scene 7 using means of 20,30,40,50,60,70,80
scene97 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene97.csv")
scene107 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene107.csv")
scene117 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene117.csv")
scene157 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene157.csv")
scene207 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene207.csv")
scene407 <- read.csv("C:\\Users\\ebn2804\\OneDrive - Northwestern University\\scene407.csv")


scene7_all <- rbind(scene97,scene107,scene117,scene157,scene207,scene407)

plot7 <- ggplot(scene7_all, aes(x=factor(clusters), y=prevalence, color=factor(clusters) ))+
  geom_boxplot()+
  theme( legend.position = "none" )+
  facet_wrap(~scenario, ncol=3)+
  labs(title= "Clustering classification based on 7 varying means(20,30,40,50,60,70,80)")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")
