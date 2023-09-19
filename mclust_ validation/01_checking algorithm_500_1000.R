library(ggplot2)
library(mcclust)


##Exploring performance of mclust classification by setting up different scenarios using different means and sds##

##New Clustering Scenarios
#Scenarios
#500
#Scene 1%
scene0 <- c(rnorm(500, mean=20, sd=0.01*20))#Using only one mean(20)

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30))#Using mean(20 and 30)

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.01*40))

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.01*40), rnorm(500, mean=50, sd=0.01*50))

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30),#Using mean(20,30,40,50 and 60)
            rnorm(500, mean=40, sd=0.01*40), rnorm(500, mean=50, sd=0.01*50), 
            rnorm(500, mean=60, sd=0.01*60))

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.01*40), rnorm(500, mean=50, sd=0.01*50), 
            rnorm(500, mean=60, sd=0.01*60), rnorm(500, mean=70, sd=0.01*70))

scene0 <- c(rnorm(500, mean=20, sd=0.01*20), rnorm(500, mean=30, sd=0.01*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.01*40), rnorm(500, mean=50, sd=0.01*50), 
            rnorm(500, mean=60, sd=0.01*60), rnorm(500, mean=70, sd=0.01*70),
            rnorm(500, mean=80, sd=0.01*80))

#Clustering
oute0 <- Mclust(scene0)
summary(oute0)

all_scene0_clusters = data.frame(oute0$classification) %>%
  rownames_to_column()

colnames(all_scene0_clusters)[2] <- "scene0_clusters"

clust_scene0grp <- cbind(all_scene0_clusters, scene0)

clust_scene0 <- clust_scene0grp %>%
  group_by(scene0_clusters) %>%
  summarize(MaxValuebycluster = max(scene0, na.rm = T),
            Minvaluebycluster= min(scene0, na.rm = T)) %>%
  arrange(scene0_clusters)

p01 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p02 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0 ,color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p03 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p04 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss= 500, mean= 20,30,40 and 50, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p05 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p06 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60 and 70, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p07 <- ggplot(data=clust_scene0grp, aes(x=as.factor(scene0_clusters), y=scene0, color=as.factor(scene0_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

#Scene 2%
scene1 <- c(rnorm(500, mean=20, sd=0.02*20))#Using only one mean(20)

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30))#Using mean(20 and 30)

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.02*40))

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.02*40), rnorm(500, mean=50, sd=0.02*50))

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30),#Using mean(20,30,40,50 and 60)
            rnorm(500, mean=40, sd=0.02*40), rnorm(500, mean=50, sd=0.02*50), 
            rnorm(500, mean=60, sd=0.02*60))

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.02*40), rnorm(500, mean=50, sd=0.02*50), 
            rnorm(500, mean=60, sd=0.02*60), rnorm(500, mean=70, sd=0.02*70))

scene1 <- c(rnorm(500, mean=20, sd=0.02*20), rnorm(500, mean=30, sd=0.02*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.02*40), rnorm(500, mean=50, sd=0.02*50), 
            rnorm(500, mean=60, sd=0.02*60), rnorm(500, mean=70, sd=0.02*70),
            rnorm(500, mean=80, sd=0.02*80))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p11 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p12 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p13 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p14 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40 and 50, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p15 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p16 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60 and 70, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p17 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

##Scene 5%
scene2 <- c(rnorm(500, mean=20, sd=0.05*20))#Using only one mean(20)

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30))#Using mean(20 and 30)

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.05*40))

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.05*40), rnorm(500, mean=50, sd=0.05*50))

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30),#Using mean(20,30, 40,50 and 60)
            rnorm(500, mean=40, sd=0.05*40), rnorm(500, mean=50, sd=0.05*50), 
            rnorm(500, mean=60, sd=0.05*60))

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.05*40), rnorm(500, mean=50, sd=0.05*50), 
            rnorm(500, mean=60, sd=0.05*60), rnorm(500, mean=70, sd=0.05*70))

scene2 <- c(rnorm(500, mean=20, sd=0.05*20), rnorm(500, mean=30, sd=0.05*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.05*40), rnorm(500, mean=50, sd=0.05*50), 
            rnorm(500, mean=60, sd=0.05*60), rnorm(500, mean=70, sd=0.05*70),
            rnorm(500, mean=80, sd=0.05*80))

#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p21 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2,color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p22 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p23 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p24 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40 and 50, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p25 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p26 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classificati2on based on mean= 20,30,40,50,60 and 70, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p27 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2, color=as.factor(scene2_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

#7%
scene3 <- c(rnorm(500, mean=20, sd=0.07*20))#Using only one mean(20)

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30))#Using mean(20 and 30)

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.07*40))

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.07*40), rnorm(500, mean=50, sd=0.07*50))

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30),#Using mean(20,30, 40,50 and 60)
            rnorm(500, mean=40, sd=0.07*40), rnorm(500, mean=50, sd=0.07*50), 
            rnorm(500, mean=60, sd=0.07*60))

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.07*40), rnorm(500, mean=50, sd=0.07*50), 
            rnorm(500, mean=60, sd=0.07*60), rnorm(500, mean=70, sd=0.07*70))

scene3 <- c(rnorm(500, mean=20, sd=0.07*20), rnorm(500, mean=30, sd=0.07*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.07*40), rnorm(500, mean=50, sd=0.07*50), 
            rnorm(500, mean=60, sd=0.07*60), rnorm(500, mean=70, sd=0.07*70),
            rnorm(500, mean=80, sd=0.07*80))

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p31 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p32 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p33 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p34 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40 and 50, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p35 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p36 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60 and 70, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p37 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3, color=as.factor(scene3_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

#10%
scene4 <- c(rnorm(500, mean=20, sd=0.1*20))#Using only one mean(20)

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30))#Using mean(20 and 30)

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.1*40))

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.1*40), rnorm(500, mean=50, sd=0.1*50))

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30),#Using mean(20,30, 40,50 and 60)
            rnorm(500, mean=40, sd=0.1*40), rnorm(500, mean=50, sd=0.1*50), 
            rnorm(500, mean=60, sd=0.1*60))

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.1*40), rnorm(500, mean=50, sd=0.1*50), 
            rnorm(500, mean=60, sd=0.1*60), rnorm(500, mean=70, sd=0.1*70))

scene4 <- c(rnorm(500, mean=20, sd=0.1*20), rnorm(500, mean=30, sd=0.1*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.1*40), rnorm(500, mean=50, sd=0.1*50), 
            rnorm(500, mean=60, sd=0.1*60), rnorm(500, mean=70, sd=0.1*70),
            rnorm(500, mean=80, sd=0.1*80))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p41 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p42 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p43 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p44 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40 and 50, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p45 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p46 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60 and 70, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p47 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4, color=as.factor(scene4_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")


#20%
scene5 <- c(rnorm(500, mean=20, sd=0.2*20))#Using only one mean(20)

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30))#Using mean(20 and 30)

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30), #Using mean(20,30 and 40)
            rnorm(500, mean=40, sd=0.2*40))

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30), #Using mean(20,30, 40 and 50)
            rnorm(500, mean=40, sd=0.2*40), rnorm(500, mean=50, sd=0.2*50))

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30),#Using mean(20,30, 40,50 and 60)
            rnorm(500, mean=40, sd=0.2*40), rnorm(500, mean=50, sd=0.2*50), 
            rnorm(500, mean=60, sd=0.2*60))

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(500, mean=40, sd=0.2*40), rnorm(500, mean=50, sd=0.2*50), 
            rnorm(500, mean=60, sd=0.2*60), rnorm(500, mean=70, sd=0.2*70))

scene5 <- c(rnorm(500, mean=20, sd=0.2*20), rnorm(500, mean=30, sd=0.2*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(500, mean=40, sd=0.2*40), rnorm(500, mean=50, sd=0.2*50), 
            rnorm(500, mean=60, sd=0.2*60), rnorm(500, mean=70, sd=0.2*70),
            rnorm(500, mean=80, sd=0.2*80))

#Clustering
oute5 <- Mclust(scene5)
summary(oute5)

all_scene5_clusters = data.frame(oute5$classification) %>%
  rownames_to_column()

colnames(all_scene5_clusters)[2] <- "scene5_clusters"

clust_scene5grp <- cbind(all_scene5_clusters, scene5)

clust_scene5 <- clust_scene5grp %>%
  group_by(scene5_clusters) %>%
  summarize(MaxValuebycluster = max(scene5, na.rm = T),
            Minvaluebycluster= min(scene5, na.rm = T)) %>%
  arrange(scene5_clusters)

p51 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p52 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20 and 30, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p53 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30 and 40, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p54 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene5, color=as.factor(scene1_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40 and 50, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p55 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50 and 60, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p56 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60 and 70, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

p57 <- ggplot(data=clust_scene5grp, aes(x=as.factor(scene5_clusters), y=scene5, color=as.factor(scene5_clusters))) +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on mean= 20,30,40,50,60,70 and 80, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")+
  theme(legend.position = "none")

#Combine all plots on same pdf

library(cowplot)
bottom_plots1 <- plot_grid (p01, p11)
middle_plots1 <- plot_grid(p21, p31)
top_plots1 <- plot_grid(p41, p51)
All_clus_plots1 <- plot_grid(top_plots1, bottom_plots1, middle_plots1, label_size = 12, ncol = 1)

bottom_plots2 <- plot_grid (p02, p12)
middle_plots2 <- plot_grid(p22, p32)
top_plots2 <- plot_grid(p42, p52)
All_clus_plots2 <- plot_grid(top_plots2,bottom_plots2, middle_plots2, label_size = 12, ncol = 1)

bottom_plots3 <- plot_grid (p03, p13)
middle_plots3 <- plot_grid(p23, p33)
top_plots3 <- plot_grid(p43, p53)
All_clus_plots3 <- plot_grid(top_plots3,bottom_plots3, middle_plots3, label_size = 12, ncol = 1)

bottom_plots4 <- plot_grid (p04, p14)
middle_plots4 <- plot_grid(p24, p34)
top_plots4 <- plot_grid(p44, p54)
All_clus_plots4 <- plot_grid(top_plots4,bottom_plots4, middle_plots4, label_size = 12, ncol = 1)

bottom_plots5 <- plot_grid (p05, p15)
middle_plots5 <- plot_grid(p25, p35)
top_plots5 <- plot_grid(p45, p55)
All_clus_plots5 <- plot_grid(top_plots5,bottom_plots5, middle_plots5, label_size = 12, ncol = 1)

bottom_plots6 <- plot_grid (p06, p16)
middle_plots6 <- plot_grid(p26, p36)
top_plots6 <- plot_grid(p46, p56)
All_clus_plots6 <- plot_grid(top_plots6,bottom_plots6, middle_plots6, label_size = 12, ncol = 1)

bottom_plots7 <- plot_grid (p07, p17)
middle_plots7 <- plot_grid(p27, p37)
top_plots7 <- plot_grid(p47, p57)
All_clus_plots7 <- plot_grid(top_plots7,bottom_plots7, middle_plots7, label_size = 12, ncol = 1)



####Repeating scenarios using a sample size of 1000 ###-------------------------
#1%
scene1 <- c(rnorm(1000, mean=20, sd=0.01*20))#Using only one mean(20)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1001 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30))#Using mean(20 and 30)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1002 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.01*40))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1003 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.01*40), rnorm(1000, mean=50, sd=0.01*50))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1004 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30),#Using mean(20,30,40,50 and 60)
            rnorm(1000, mean=40, sd=0.01*40), rnorm(1000, mean=50, sd=0.01*50), 
            rnorm(1000, mean=60, sd=0.01*60))
#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1005 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")




scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.01*40), rnorm(1000, mean=50, sd=0.01*50), 
            rnorm(1000, mean=60, sd=0.01*60), rnorm(1000, mean=70, sd=0.01*70))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1006 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60 and 70, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene1 <- c(rnorm(1000, mean=20, sd=0.01*20), rnorm(1000, mean=30, sd=0.01*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.01*40), rnorm(1000, mean=50, sd=0.01*50), 
            rnorm(1000, mean=60, sd=0.01*60), rnorm(1000, mean=70, sd=0.01*70),
            rnorm(1000, mean=80, sd=0.01*80))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)


p1007 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=1%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Scene 2%
scene1 <- c(rnorm(1000, mean=20, sd=0.02*20))#Using only one mean(20)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1021 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30))#Using mean(20 and 30)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1022 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.02*40))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1023 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.02*40), rnorm(1000, mean=50, sd=0.02*50))
#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1024 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30),#Using mean(20,30,40,50 and 60)
            rnorm(1000, mean=40, sd=0.02*40), rnorm(1000, mean=50, sd=0.02*50), 
            rnorm(1000, mean=60, sd=0.02*60))
#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1025 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.02*40), rnorm(1000, mean=50, sd=0.02*50), 
            rnorm(1000, mean=60, sd=0.02*60), rnorm(1000, mean=70, sd=0.02*70))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1026 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60 and 70, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.02*20), rnorm(1000, mean=30, sd=0.02*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.02*40), rnorm(1000, mean=50, sd=0.02*50), 
            rnorm(1000, mean=60, sd=0.02*60), rnorm(1000, mean=70, sd=0.02*70),
            rnorm(1000, mean=80, sd=0.02*80))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p1027 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=2%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

##Scene 5%

scene2 <- c(rnorm(1000, mean=20, sd=0.05*20))#Using only one mean(20)

#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2051 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30))#Using mean(20 and 30)

#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2052 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.05*40))


#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2053 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.05*40), rnorm(1000, mean=50, sd=0.05*50))
#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2054 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1000, mean=40, sd=0.05*40), rnorm(1000, mean=50, sd=0.05*50), 
            rnorm(1000, mean=60, sd=0.05*60))

#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2055 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.05*40), rnorm(1000, mean=50, sd=0.05*50), 
            rnorm(1000, mean=60, sd=0.05*60), rnorm(1000, mean=70, sd=0.05*70))

#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2056 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classificati2on based on ss=1000 mean= 20,30,40,50,60 and 70, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1000, mean=20, sd=0.05*20), rnorm(1000, mean=30, sd=0.05*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.05*40), rnorm(1000, mean=50, sd=0.05*50), 
            rnorm(1000, mean=60, sd=0.05*60), rnorm(1000, mean=70, sd=0.05*70),
            rnorm(1000, mean=80, sd=0.05*80))


#Clustering
oute2 <- Mclust(scene2)
summary(oute2)

all_scene2_clusters = data.frame(oute2$classification) %>%
  rownames_to_column()

colnames(all_scene2_clusters)[2] <- "scene2_clusters"

clust_scene2grp <- cbind(all_scene2_clusters, scene2)

clust_scene2 <- clust_scene2grp %>%
  group_by(scene2_clusters) %>%
  summarize(MaxValuebycluster = max(scene2, na.rm = T),
            Minvaluebycluster= min(scene2, na.rm = T)) %>%
  arrange(scene2_clusters)

p2057 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=5%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

#7%
scene3 <- c(rnorm(1000, mean=20, sd=0.07*20))#Using only one mean(20)

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3071 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30))#Using mean(20 and 30)

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3072 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.07*40))
#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3073 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.07*40), rnorm(1000, mean=50, sd=0.07*50))

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3074 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1000, mean=40, sd=0.07*40), rnorm(1000, mean=50, sd=0.07*50), 
            rnorm(1000, mean=60, sd=0.07*60))

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3075 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.07*40), rnorm(1000, mean=50, sd=0.07*50), 
            rnorm(1000, mean=60, sd=0.07*60), rnorm(1000, mean=70, sd=0.07*70))

#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3076 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60 and 70, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1000, mean=20, sd=0.07*20), rnorm(1000, mean=30, sd=0.07*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.07*40), rnorm(1000, mean=50, sd=0.07*50), 
            rnorm(1000, mean=60, sd=0.07*60), rnorm(1000, mean=70, sd=0.07*70),
            rnorm(1000, mean=80, sd=0.07*80))
#Clustering
oute3 <- Mclust(scene3)
summary(oute3)

all_scene3_clusters = data.frame(oute3$classification) %>%
  rownames_to_column()

colnames(all_scene3_clusters)[2] <- "scene3_clusters"

clust_scene3grp <- cbind(all_scene3_clusters, scene3)

clust_scene3 <- clust_scene3grp %>%
  group_by(scene3_clusters) %>%
  summarize(MaxValuebycluster = max(scene3, na.rm = T),
            Minvaluebycluster= min(scene3, na.rm = T)) %>%
  arrange(scene3_clusters)

p3077 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=7%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#SD of 10%----------------------------------------------------------------------
scene4 <- c(rnorm(1000, mean=20, sd=0.1*20))#Using only one mean(20)

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p4101 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30))#Using mean(20 and 30)

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p4102 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.1*40))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p4103 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

p4104 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.1*40), rnorm(1000, mean=50, sd=0.1*50))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p4105 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1000, mean=40, sd=0.1*40), rnorm(1000, mean=50, sd=0.1*50), 
            rnorm(1000, mean=60, sd=0.1*60))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

p4106 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60 and 70, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.1*40), rnorm(1000, mean=50, sd=0.1*50), 
            rnorm(1000, mean=60, sd=0.1*60), rnorm(1000, mean=70, sd=0.1*70))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)

scene4 <- c(rnorm(1000, mean=20, sd=0.1*20), rnorm(1000, mean=30, sd=0.1*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.1*40), rnorm(1000, mean=50, sd=0.1*50), 
            rnorm(1000, mean=60, sd=0.1*60), rnorm(1000, mean=70, sd=0.1*70),
            rnorm(1000, mean=80, sd=0.1*80))

#Clustering
oute4 <- Mclust(scene4)
summary(oute4)

all_scene4_clusters = data.frame(oute4$classification) %>%
  rownames_to_column()

colnames(all_scene4_clusters)[2] <- "scene4_clusters"

clust_scene4grp <- cbind(all_scene4_clusters, scene4)

clust_scene4 <- clust_scene4grp %>%
  group_by(scene4_clusters) %>%
  summarize(MaxValuebycluster = max(scene4, na.rm = T),
            Minvaluebycluster= min(scene4, na.rm = T)) %>%
  arrange(scene4_clusters)


p4107 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=10%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

#SD of 20%---------------------------------------------------------------------- 
scene1 <- c(rnorm(1000, mean=20, sd=0.2*20))#Using only one mean(20)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5201 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30))#Using mean(20 and 30)

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5202 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20 and 30, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30), #Using mean(20,30 and 40)
            rnorm(1000, mean=40, sd=0.2*40))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5203 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30 and 40, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30), #Using mean(20,30, 40 and 50)
            rnorm(1000, mean=40, sd=0.2*40), rnorm(1000, mean=50, sd=0.2*50))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5204 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40 and 50, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1000, mean=40, sd=0.2*40), rnorm(1000, mean=50, sd=0.2*50), 
            rnorm(1000, mean=60, sd=0.2*60))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5205 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50 and 60, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1000, mean=40, sd=0.2*40), rnorm(1000, mean=50, sd=0.2*50), 
            rnorm(1000, mean=60, sd=0.2*60), rnorm(1000, mean=70, sd=0.2*70))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p5206 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60 and 70, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1000, mean=20, sd=0.2*20), rnorm(1000, mean=30, sd=0.2*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1000, mean=40, sd=0.2*40), rnorm(1000, mean=50, sd=0.2*50), 
            rnorm(1000, mean=60, sd=0.2*60), rnorm(1000, mean=70, sd=0.2*70),
            rnorm(1000, mean=80, sd=0.2*80))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)


p5207 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based on ss=1000 mean= 20,30,40,50,60,70 and 80, sd=20%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Combine all plots on same pdf

library(cowplot)
top_plots1 <- plot_grid (p1001, p1021)
middle_plots1 <- plot_grid(p2051, p3071)
bottom_plots1 <- plot_grid(p4101, p5201)
All_clus_plots1 <- plot_grid(top_plots1, middle_plots1, bottom_plots1, label_size = 12, ncol = 1)

top_plots2 <- plot_grid (p1002, p1022)
middle_plots2 <- plot_grid(p2052, p3072)
bottom_plots2 <- plot_grid(p4102, p5202)
All_clus_plots2 <- plot_grid(top_plots2, middle_plots2, bottom_plots2, label_size = 12, ncol = 1)

top_plots3 <- plot_grid (p1003, p1023)
middle_plots3 <- plot_grid(p2053, p3073)
bottom_plots3 <- plot_grid(p4103, p5203)
All_clus_plots3 <- plot_grid(top_plots3, middle_plots3, bottom_plots3, label_size = 12, ncol = 1)

top_plots4 <- plot_grid (p1004, p1024)
middle_plots4 <- plot_grid(p2054, p3074)
bottom_plots4 <- plot_grid(p4104, p5204)
All_clus_plots4 <- plot_grid(top_plots4,middle_plots4,bottom_plots4, label_size = 12, ncol = 1)

top_plots5 <- plot_grid (p1005, p1025)
middle_plots5 <- plot_grid(p2055, p3075)
bottom_plots5 <- plot_grid(p4105, p5205)
All_clus_plots5 <- plot_grid(top_plots5, middle_plots5, bottom_plots5, label_size = 12, ncol = 1)

top_plots6 <- plot_grid (p1006, p1026)
middle_plots6 <- plot_grid(p2056, p3076)
bottom_plots6 <- plot_grid(p4106, p5206)
All_clus_plots6 <- plot_grid(top_plots6, middle_plots6, bottom_plots6, label_size = 12, ncol = 1)

top_plots7 <- plot_grid (p1007, p1027)
middle_plots7 <- plot_grid(p2057, p3077)
bottom_plots7 <- plot_grid(p4107, p5207)
All_clus_plots7 <- plot_grid(top_plots7, middle_plots7, bottom_plots7, label_size = 12, ncol = 1)
