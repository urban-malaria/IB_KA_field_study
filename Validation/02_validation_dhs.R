rm(list = ls())

## -----------------------------------------
### Paths
## -----------------------------------------
source("Validation/load_paths.R", echo= F)


# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("Validation/functions.R", echo= F)

#loading the relevant packages for plotting##
library(ggplot2)
library(ggpubr)
library(fossil)
library(caret)
library(reshape2)
library(clevr)
library(CrossClustering)
library(gridExtra)



# -----------------------------------------
### load data 
## -----------------------------------------
load("Validation/dhs_prevalence_clustering.RData") # DHS prevalence clusters, file name is all_c
load("Validation/clustering_covariates.RData") # file name is map_df_dhs_all
load("Validation/inla_predictions.RData") #file name is inla predictions

all_c = all_c %>%  rename(prev_cluster_num = prev_cluster)

covariate_clusters = map_df_dhs_all %>%  select(DHSCLUST, covariate_cluster_num=covariate_clusters)
st_geometry(covariate_clusters)<- NULL

inla_predictions = inla_predictions %>%  rename(DHSCLUST=spatial_data.cluster_numbers)

all_dat = left_join(all_c, covariate_clusters) %>%left_join(inla_predictions) %>%  drop_na() #need to investigate these NAs later


# -----------------------------------------
### predicted prevalence clustering 
## -----------------------------------------

p.prd= all_dat$p.prd
out<- Mclust(p.prd)
summary(out)
all_dat$predicted_prev_clust_num = out$classification



# -------------------------------------------------------------------------------------------------------
### some descriptive plots to assess clustering classification in the prevalence vs covariate clusters 
## -------------------------------------------------------------------------------------------------------
## Generating the contingency table
df1 <- table(all_dat$prev_cluster_num,all_dat$covariate_cluster_num)

## Converting contingency table into dataframe and melting
df <- as.data.frame(df1)

df<-melt(df)
head(df)

p1 <- ggplot(df, aes(x =Var1, y = factor(Var2, levels = rev(levels(Var2))), label=value))+
  #geom_point()+
  geom_label(aes(fill=value), colour="white")+
  scale_fill_viridis(name =NULL)+
  xlab("DHS prevalence clustering labels") +
  ylab("Covariate clustering labels") +
  theme_manuscript()


## Generating the contingency table
df2 <- table(all_dat$prev_cluster_num,all_dat$predicted_prev_clust_num)

## Converting contingency table into dataframe and melting
df <- as.data.frame(df2)

df<-melt(df)
head(df)

p2 <- ggplot(df, aes(x =Var1, y = factor(Var2, levels = rev(levels(Var2))), label=value))+
  #geom_point()+
  geom_label(aes(fill=value), colour="white")+
  scale_fill_viridis(name=NULL)+
  xlab("DHS prevalence clustering labels") +
  ylab("INLA predicted prevalence clustering labels") +
  theme_manuscript()


#Estimating clustering performance using rand index
rand.index(all_dat$prev_cluster_num, all_dat$covariate_cluster_num)
rand.index(all_dat$prev_cluster_num, all_dat$predicted_prev_clust_num)
report_cov=eval_report_clusters(all_dat$prev_cluster_num, all_dat$covariate_cluster_num) %>%  unlist() %>%  as.data.frame()
colnames(report_cov)[1] <-"Covariate"
report_inla<- eval_report_clusters(all_dat$prev_cluster_num, all_dat$predicted_prev_clust_num) %>%  unlist() %>%  as.data.frame()
colnames(report_inla)[1] <-"INLA"
all_report = cbind(report_cov, report_inla) %>% mutate_if(is.numeric, round, 3)



p3<-tableGrob(all_report)


# The adjusted Rand Index (ARI) should be interpreted as follows:
#ARI >= 0.90 excellent recovery; 0.80 =< ARI < 0.90 good recovery; 0.65 =< ARI < 0.80 moderate recovery; ARI < 0.65 poor recovery.
#As the confidence interval is based on the approximation to the Normal distribution, it is recommended to trust in the confidence interval only in cases of total number of object clustered greater than 100.

#compute adjusted rand index confidence interval
#devtools::install_github('CorradoLanera/CrossClustering', ref = 'develop')
ari(df1)
ari(df2)
ari = data.frame(name = c("covariate clustering", "inla clustering"),
                 ari = c(0, 0.19), lower_ari = c(-0.03, 0.18), upper_ari =c(0.03, 0.2)) 

p4=ggplot(ari, aes(x=name, y =ari, color=name))+
  geom_pointrange(aes(ymin=lower_ari, ymax=upper_ari))+
  scale_color_viridis(discrete = T, name =NULL)+
  ylab("Adjusted Rand Index") +
  xlab("") +
  theme_manuscript()

# -----------------------------------------------------
### predicted prevalence vs dhs prevalence comparison 
## ----------------------------------------------------

p5=ggplot(all_dat, aes(x=prevalence, y=p.prd)) +
  geom_point(size=2, shape=23) +
  geom_smooth()+theme_manuscript()+
  xlab("DHS prevalence")+
  ylab("predicted prevalence")

all = p1 + p2 + p3 + p4 +p5


# #Extracting the cluster classification
# newdf$prevalence_clusters <- as.numeric(newdf$prevalence_clusters)
# newdf$covariate_clusters <- as.numeric(newdf$covariate_clusters)
# 
# 
# #Estimating clustering performance using rand index
# rand.index(newdf$prevalence_clusters, newdf$covariate_clusters)
# 
# #Visualizing each cluster classification using Histogram
# hist(newdf$prevalence_clusters)
# hist(newdf$covariate_clusters)
# 
# 
# #Viisualing the cluster information using simle scatter plot#
# ggplot(newdf, aes(x = prevalence_clusters, y = covariate_clusters)) +
#   geom_point()
# 
# ## Generating the Contigency table
# cluster_table <- table(newdf$prevalence_clusters, newdf$covariate_clusters)
# 
# 
# ## Converting contigency table into dataframe
# cluster_table1 <- as.data.frame(cluster_table)
# 
# ##Visualizing using balloon plot#
# 
# ##Using actual frequency
# ggballoonplot(cluster_table1, fill = "value", show.label = TRUE)+
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4)
# 
# 
# ##Using Row percentage
# rowdf <- read.csv(file.path(CDataDir, "clust_row_percent.csv"))
# 
# row_cluster_table <- as.data.frame(rowdf)
# 
# ggballoonplot(row_cluster_table, fill = "value", show.label = TRUE)+
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4)
# 
# ##Using Col percentage
# 
# coldf <- read.csv(file.path(CDataDir, "clust_col_percent.csv"))
# 
# col_cluster_table <- as.data.frame(coldf)
# 
# ggballoonplot(col_cluster_table, fill = "value", show.label = TRUE)+
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4)
# 
# ##Scatterplot adding cluster as factor variable##
# ggplot(newdf, aes(x = prevalence_clusters, y = covariate_clusters)) +
#   geom_point(aes(color = factor(covariate_clusters)))
# 
# 
# #Random Forest Plot##
# 
# ggplot(data=cluster_table1) +
#   geom_tile(aes(x=Var1, y=Var2, fill=Freq)) +
#   scale_fill_viridis_c(trans = "sqrt", alpha = .4)+
#   ggtitle('Prediction Accuracy for Classes in Cross-Validation (Random Forest Model)') +
#   xlab('Prevalance Classification') +
#   ylab('Covariate Classification')
# 
# 
# 
# 
# 
# 
# 
# 
# 
