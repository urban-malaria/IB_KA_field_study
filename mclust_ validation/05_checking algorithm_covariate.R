rm(list=ls())

library(ggplot2)
library(mcclust)
library(clustvarsel)
library(faux)


user <- Sys.getenv("USER") 


if ("ifeomaozodiegwu" %in% user) {
  user_path <- file.path("/Users", user, "Library", "CloudStorage")
  Drive <- file.path(user_path, "OneDrive-NorthwesternUniversity")
  
} else {
  
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
}

NuDir <- file.path(Drive, "urban_malaria")
DataDir <-file.path(NuDir, "data", "nigeria")
ProjDir <- file.path(NuDir, "projects")
SampDir <- file.path(ProjDir, "sampling")
DHS_Dir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data')
Raster_Dir <- file.path(DataDir, 'Raster_files')
DHS_data <- file.path(DHS_Dir, 'DHS')
mod_file = file.path(ProjDir, "mathematical_model")
#CsvDir = file.pa

#Running covariates scenarios with varying means
-------------------------------------------------------------------------------

#create fake dataset 
  
#info on setting up https://www.r-bloggers.com/2017/07/simstudy-update-two-new-functions-that-generate-correlated-observations-from-non-normal-distributions/

df_all_var_c <- read.csv(file.path(mod_file, "Kano_variables.cluster.csv"))
df_fake <- sim_df(df_all_var_c, 500) %>%  dplyr::select(-id)

  
#Clustering
out5 <- clustvarsel(df_fake)
summary(out5$model)
  
#Getting data ready for plots
covariate_clusters = data.frame(out5$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covatiates with varying means @ sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")





---------------------------------------------------------------------------------------------------------------




#5% standard deviation

#Creating scenarios
covariate0 <- c(rnorm(500, mean=20, sd=0.05*20)) #Using mean =20

covariate1 <- c(rnorm(500, mean=30, sd=0.05*30)) #Using mean = 30)

covariate2 <- c(rnorm(500, mean=40, sd=0.05*40))  #Using mean = 40)

covariate3 <- c(rnorm(500, mean=50, sd=0.05*50)) #Using mean = 50)


#Combining to dataframe
covariates <- cbind(covariate0, covariate1, covariate2, covariate3)

#Clustering
out5 <- clustvarsel(covariates)
summary(out5$model)

#Getting data ready for plots
covariate_clusters = data.frame(out5$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov5 <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covatiates with varying means(20,30,40,50 @ sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#10% standard deviation

#Creating scenarios
covariate0 <- c(rnorm(500, mean=20, sd=0.10*20)) #Using mean =20

covariate1 <- c(rnorm(500, mean=30, sd=0.10*30)) #Using mean = 30)

covariate2 <- c(rnorm(500, mean=40, sd=0.10*40))  #Using mean = 40)

covariate3 <- c(rnorm(500, mean=50, sd=0.10*50)) #Using mean = 50)


#Combining to dataframe
covariates <- cbind(covariate0, covariate1, covariate2, covariate3)

#Clustering
out10 <- clustvarsel(covariates)
summary(out10$model)

#Getting data ready for plots
covariate_clusters = data.frame(out10$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov10 <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covatiates with varying means(20,30,40,50 @ sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#20% standard deviation

#Creating scenarios

covariate0 <- c(rnorm(500, mean=20, sd=0.20*20)) #Using mean =20

covariate1 <- c(rnorm(500, mean=30, sd=0.20*30)) #Using mean = 30)

covariate2 <- c(rnorm(500, mean=40, sd=0.20*40))  #Using mean = 40)

covariate3 <- c(rnorm(500, mean=50, sd=0.20*50)) #Using mean = 50)


#Combining to dataframe
covariates <- cbind(covariate0, covariate1, covariate2, covariate3)

#Clustering
out20 <- clustvarsel(covariates)
summary(out20$model)

#Getting data ready for plots
covariate_clusters = data.frame(out20$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov20 <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covariates with varying means(20,30,40,50 @ sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#50% standard deviation

#Creating scenarios

covariate0 <- c(rnorm(500, mean=20, sd=0.50*20)) #Using mean =20

covariate1 <- c(rnorm(500, mean=30, sd=0.50*30)) #Using mean = 30)

covariate2 <- c(rnorm(500, mean=40, sd=0.50*40))  #Using mean = 40)

covariate3 <- c(rnorm(500, mean=50, sd=0.50*50)) #Using mean = 50)


#Combining to dataframe
covariates <- cbind(covariate0, covariate1, covariate2, covariate3)

#Clustering
out50 <- clustvarsel(covariates)
summary(out50$model)

#Getting data ready for plots
covariate_clusters = data.frame(out50$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov50 <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covariates with varying means(20,30,40,50 @ sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#70% standard deviation

#Creating scenarios

covariate0 <- c(rnorm(500, mean=20, sd=0.70*20)) #Using mean =20

covariate1 <- c(rnorm(500, mean=30, sd=0.70*30)) #Using mean = 30)

covariate2 <- c(rnorm(500, mean=40, sd=0.70*40))  #Using mean = 40)

covariate3 <- c(rnorm(500, mean=50, sd=0.70*50)) #Using mean = 50)


#Combining to dataframe
covariates <- cbind(covariate0, covariate1, covariate2, covariate3)

#Clustering
out70 <- clustvarsel(covariates)
summary(out70$model)

#Getting data ready for plots
covariate_clusters = data.frame(out70$model$classification)%>%
  rownames_to_column()

colnames(covariate_clusters)[2] <- "covariate_clusters"

clust_covariategrp <- cbind(covariate_clusters, covariates)

clust_covariate <- clust_covariategrp %>%
  group_by(covariate_clusters) %>%
  summarize(MaxValuebycluster = max(covariates, na.rm = T),
            Minvaluebycluster= min(covariates, na.rm = T)) %>%
  arrange(covariate_clusters)

#Plotting
pcov70 <- ggplot(data=clust_covariategrp, aes(x=as.factor(covariate_clusters), y=covariate1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on 4 covatiates with varying means(20,30,40,50 @ sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")
