library(ggplot2)
library(mcclust)

##Running scenarios using a fixed sample size but increasing the variation from 40% up to 90%
-----------------------------------------------------------------------------------------------------
  #SD 6%
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20))  #Using only one mean(20)
  
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
  
  p61 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30))#Using mean(20 and 30)
  
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
  
  p62 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 and 30, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position="none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30), #Using mean(20,30 and 40)
              rnorm(1500, mean=40, sd=0.06*40))
  
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
  
  p63 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30 and 40, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30), #Using mean(20,30, 40 and 50)
              rnorm(1500, mean=40, sd=0.06*40), rnorm(1500, mean=50, sd=0.06*50))
  
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
  
  p64 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30),#Using mean(20,30,40,50 and 60)
              rnorm(1500, mean=40, sd=0.06*40), rnorm(1500, mean=50, sd=0.06*50), 
              rnorm(1500, mean=60, sd=0.06*60))
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
  
  p65 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30), #Using mean(20,30,40,50,60 and 70)
              rnorm(1500, mean=40, sd=0.06*40), rnorm(1500, mean=50, sd=0.06*50), 
              rnorm(1500, mean=60, sd=0.06*60), rnorm(1500, mean=70, sd=0.06*70))
  
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
  
  p66 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.06*20), rnorm(1500, mean=30, sd=0.06*30), #Using mean(20,30,40,50,60,70 and 80)
              rnorm(1500, mean=40, sd=0.06*40), rnorm(1500, mean=50, sd=0.06*50), 
              rnorm(1500, mean=60, sd=0.06*60), rnorm(1500, mean=70, sd=0.06*70),
              rnorm(1500, mean=80, sd=0.06*80))
  
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
  
  
  p67 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=6%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
  theme(legend.position = "none")
  
  #SD 8%
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20))  #Using only one mean(20)
  
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
  
  p81 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position= "none")
    
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30))#Using mean(20 and 30)
  
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
  
  p82 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 and 30, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30), #Using mean(20,30 and 40)
              rnorm(1500, mean=40, sd=0.08*40))
  
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
  
  p83 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30 and 40, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30), #Using mean(20,30, 40 and 50)
              rnorm(1500, mean=40, sd=0.08*40), rnorm(1500, mean=50, sd=0.08*50))
  
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
  
  p84 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30),#Using mean(20,30,40,50 and 60)
              rnorm(1500, mean=40, sd=0.08*40), rnorm(1500, mean=50, sd=0.08*50), 
              rnorm(1500, mean=60, sd=0.08*60))
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
  
  p85 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30), #Using mean(20,30,40,50,60 and 70)
              rnorm(1500, mean=40, sd=0.08*40), rnorm(1500, mean=50, sd=0.08*50), 
              rnorm(1500, mean=60, sd=0.08*60), rnorm(1500, mean=70, sd=0.08*70))
  
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
  
  p86 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.08*20), rnorm(1500, mean=30, sd=0.08*30), #Using mean(20,30,40,50,60,70 and 80)
              rnorm(1500, mean=40, sd=0.08*40), rnorm(1500, mean=50, sd=0.08*50), 
              rnorm(1500, mean=60, sd=0.08*60), rnorm(1500, mean=70, sd=0.08*70),
              rnorm(1500, mean=80, sd=0.08*80))
  
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
  
  
  p87 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=8%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  

  
  #SD 9%
  scene1 <- c(rnorm(1500, mean=20, sd=0.09*20))  #Using only one mean(20)
  
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
  
  p91 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30))#Using mean(20 and 30)
  
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
  
  p92 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 and 30, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
  theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30), #Using mean(20,30 and 40)
              rnorm(1500, mean=40, sd=0.09*40))
  
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
  
  p93 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30 and 40, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30), #Using mean(20,30, 40 and 50)
              rnorm(1500, mean=40, sd=0.09*40), rnorm(1500, mean=50, sd=0.09*50))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene94.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p94 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30),#Using mean(20,30,40,50 and 60)
              rnorm(1500, mean=40, sd=0.09*40), rnorm(1500, mean=50, sd=0.09*50), 
              rnorm(1500, mean=60, sd=0.09*60))
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene95.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p95 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
scene1_96 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30), #Using mean(20,30,40,50,60 and 70)
              rnorm(1500, mean=40, sd=0.09*40), rnorm(1500, mean=50, sd=0.09*50), 
              rnorm(1500, mean=60, sd=0.09*60), rnorm(1500, mean=70, sd=0.09*70))
  
#Clustering
oute1_96 <- Mclust(scene1_96)
summary(oute1_96)
  
  all_scene1_96_clusters = data.frame(oute1_96$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_96_clusters)[2] <- "scene1_96_clusters"
  
  clust_scene1_96grp <- cbind(all_scene1_96_clusters, scene1_96)
  
  write.csv(clust_scene1_96grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene96.csv")
  
  
    clust_scene1_96 <- clust_scene1_96grp %>%
    group_by(scene1_96_clusters) %>%
    summarize(MaxValuebycluster = max(scene1_96, na.rm = T),
              Minvaluebycluster= min(scene1_96, na.rm = T)) %>%
    arrange(scene1_96_clusters)
  
  p96 <- ggplot(data=clust_scene1_96grp, aes(x=as.factor(scene1_96_clusters), y=scene1_96, color=as.factor(scene1_96_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  
  
  scene1_97 <- c(rnorm(1500, mean=20, sd=0.09*20), rnorm(1500, mean=30, sd=0.09*30), #Using mean(20,30,40,50,60,70 and 80)
              rnorm(1500, mean=40, sd=0.09*40), rnorm(1500, mean=50, sd=0.09*50), 
              rnorm(1500, mean=60, sd=0.09*60), rnorm(1500, mean=70, sd=0.09*70),
              rnorm(1500, mean=80, sd=0.09*80))
  mean(scene1_97)
  
  #Clustering
  oute1_97 <- Mclust(scene1_97)
  summary(oute1_97)
  
  all_scene1_97_clusters = data.frame(oute1_97$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_97_clusters)[2] <- "scene1_97_clusters"
  
  clust_scene1_97grp <- cbind(all_scene1_97_clusters, scene1_97)
  
  write.csv(clust_scene1_97grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene97.csv")
  
  clust_scene1_97 <- clust_scene1_97grp %>%
    group_by(scene1_97_clusters) %>%
    summarize(MaxValuebycluster = max(scene1_97, na.rm = T),
              Minvaluebycluster= min(scene1_97, na.rm = T)) %>%
    arrange(scene1_97_clusters)
  
  
  p97 <- ggplot(data=clust_scene1_97grp, aes(x=as.factor(scene1_97_clusters), y=scene1_97, color=as.factor(scene1_97_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=9%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence") +
    theme(legend.position = "none")
  

  #SD 11%
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20))  #Using only one mean(20)
  
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
  
  p111 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30))#Using mean(20 and 30)
  
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
  
  p112 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 and 30, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30), #Using mean(20,30 and 40)
              rnorm(1500, mean=40, sd=0.11*40))
  
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
  
  p113 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30 and 40, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30), #Using mean(20,30, 40 and 50)
              rnorm(1500, mean=40, sd=0.11*40), rnorm(1500, mean=50, sd=0.11*50))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene114.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p114 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30),#Using mean(20,30,40,50 and 60)
              rnorm(1500, mean=40, sd=0.11*40), rnorm(1500, mean=50, sd=0.11*50), 
              rnorm(1500, mean=60, sd=0.11*60))
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene115.csv")
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p115 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30), #Using mean(20,30,40,50,60 and 70)
              rnorm(1500, mean=40, sd=0.11*40), rnorm(1500, mean=50, sd=0.11*50), 
              rnorm(1500, mean=60, sd=0.11*60), rnorm(1500, mean=70, sd=0.11*70))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene116.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p116 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.11*20), rnorm(1500, mean=30, sd=0.11*30), #Using mean(20,30,40,50,60,70 and 80)
              rnorm(1500, mean=40, sd=0.11*40), rnorm(1500, mean=50, sd=0.11*50), 
              rnorm(1500, mean=60, sd=0.11*60), rnorm(1500, mean=70, sd=0.11*70),
              rnorm(1500, mean=80, sd=0.11*80))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene117.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
    p117 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=11%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
      
  #SD 15%
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20))  #Using only one mean(20)
  
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
  
  p151 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30))#Using mean(20 and 30)
  
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
  
  p152 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based mean= 20 and 30, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30), #Using mean(20,30 and 40)
              rnorm(1500, mean=40, sd=0.15*40))
  
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
  
  p153 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30 and 40, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30), #Using mean(20,30, 40 and 50)
              rnorm(1500, mean=40, sd=0.15*40), rnorm(1500, mean=50, sd=0.15*50))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene154.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p154 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30),#Using mean(20,30,40,50 and 60)
              rnorm(1500, mean=40, sd=0.15*40), rnorm(1500, mean=50, sd=0.15*50), 
              rnorm(1500, mean=60, sd=0.15*60))
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene155.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p155 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30), #Using mean(20,30,40,50,60 and 70)
              rnorm(1500, mean=40, sd=0.15*40), rnorm(1500, mean=50, sd=0.15*50), 
              rnorm(1500, mean=60, sd=0.15*60), rnorm(1500, mean=70, sd=0.15*70))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene156.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  p156 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")
  
  
  scene1 <- c(rnorm(1500, mean=20, sd=0.15*20), rnorm(1500, mean=30, sd=0.15*30), #Using mean(20,30,40,50,60,70 and 80)
              rnorm(1500, mean=40, sd=0.15*40), rnorm(1500, mean=50, sd=0.15*50), 
              rnorm(1500, mean=60, sd=0.15*60), rnorm(1500, mean=70, sd=0.15*70),
              rnorm(1500, mean=80, sd=0.15*80))
  
  #Clustering
  oute1 <- Mclust(scene1)
  summary(oute1)
  
  all_scene1_clusters = data.frame(oute1$classification) %>%
    rownames_to_column()
  
  colnames(all_scene1_clusters)[2] <- "scene1_clusters"
  
  clust_scene1grp <- cbind(all_scene1_clusters, scene1)
  
  write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene157.csv")
  
  clust_scene1 <- clust_scene1grp %>%
    group_by(scene1_clusters) %>%
    summarize(MaxValuebycluster = max(scene1, na.rm = T),
              Minvaluebycluster= min(scene1, na.rm = T)) %>%
    arrange(scene1_clusters)
  
  
  p157 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1, color=as.factor(scene1_clusters))) +
    geom_boxplot()+
    geom_jitter()+
    labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=15%")+
    labs(x = "clustering classification", fill = "cluster classes",
         y = "Prevalence")+
    theme(legend.position = "none")


#SD 40%
scene1 <- c(rnorm(1500, mean=20, sd=0.40*20))  #Using only one mean(20)

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

p401 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based mean= 20 sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30))#Using mean(20 and 30)

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

p402 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based mean= 20 and 30, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.40*40))

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

p403 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.40*40), rnorm(1500, mean=50, sd=0.40*50))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene404.csv")

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p404 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30),#Using mean(20,30,40,50 and 60)
            rnorm(1500, mean=40, sd=0.40*40), rnorm(1500, mean=50, sd=0.40*50), 
            rnorm(1500, mean=60, sd=0.40*60))
#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene405.csv")

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p405 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")




scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.40*40), rnorm(1500, mean=50, sd=0.40*50), 
            rnorm(1500, mean=60, sd=0.40*60), rnorm(1500, mean=70, sd=0.40*70))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene406.csv")

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)

p406 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene1 <- c(rnorm(1500, mean=20, sd=0.40*20), rnorm(1500, mean=30, sd=0.40*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.40*40), rnorm(1500, mean=50, sd=0.40*50), 
            rnorm(1500, mean=60, sd=0.40*60), rnorm(1500, mean=70, sd=0.40*70),
            rnorm(1500, mean=80, sd=0.40*80))

#Clustering
oute1 <- Mclust(scene1)
summary(oute1)

all_scene1_clusters = data.frame(oute1$classification) %>%
  rownames_to_column()

colnames(all_scene1_clusters)[2] <- "scene1_clusters"

clust_scene1grp <- cbind(all_scene1_clusters, scene1)

write.csv(clust_scene1grp, "C:\\Users\\ebn2804\\OneDrive - Northwestern University\\Desktop\\scene407.csv")

clust_scene1 <- clust_scene1grp %>%
  group_by(scene1_clusters) %>%
  summarize(MaxValuebycluster = max(scene1, na.rm = T),
            Minvaluebycluster= min(scene1, na.rm = T)) %>%
  arrange(scene1_clusters)


p407 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=40%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#SD of 50%

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20))#Using only one mean(20)

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

p501 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30))#Using mean(20 and 30)

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

p502 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20 and 30, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.50*40))

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

p503 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.50*40), rnorm(1500, mean=50, sd=0.50*50))
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

p504 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30),#Using mean(20,30,40,50 and 60)
            rnorm(1500, mean=40, sd=0.50*40), rnorm(1500, mean=50, sd=0.50*50), 
            rnorm(1500, mean=60, sd=0.50*60))
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

p505 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.50*40), rnorm(1500, mean=50, sd=0.50*50), 
            rnorm(1500, mean=60, sd=0.50*60), rnorm(1500, mean=70, sd=0.50*70))

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

p506 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.50*20), rnorm(1500, mean=30, sd=0.50*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.50*40), rnorm(1500, mean=50, sd=0.50*50), 
            rnorm(1500, mean=60, sd=0.50*60), rnorm(1500, mean=70, sd=0.50*70),
            rnorm(1500, mean=80, sd=0.50*80))

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

p507 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=50%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

##SD of 60%

scene2 <- c(rnorm(1500, mean=20, sd=0.60*20))#Using only one mean(20)

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

p601 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30))#Using mean(20 and 30)

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

p602 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20 and 30, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.60*40))


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

p603 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.60*40), rnorm(1500, mean=50, sd=0.60*50))
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

p604 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1500, mean=40, sd=0.60*40), rnorm(1500, mean=50, sd=0.60*50), 
            rnorm(1500, mean=60, sd=0.60*60))

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

p605 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.60*40), rnorm(1500, mean=50, sd=0.60*50), 
            rnorm(1500, mean=60, sd=0.60*60), rnorm(1500, mean=70, sd=0.60*70))

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

p606 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classificati2on based  mean= 20,30,40,50,60 and 70, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene2 <- c(rnorm(1500, mean=20, sd=0.60*20), rnorm(1500, mean=30, sd=0.60*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.60*40), rnorm(1500, mean=50, sd=0.60*50), 
            rnorm(1500, mean=60, sd=0.60*60), rnorm(1500, mean=70, sd=0.60*70),
            rnorm(1500, mean=80, sd=0.60*80))


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

p607 <- ggplot(data=clust_scene2grp, aes(x=as.factor(scene2_clusters), y=scene2), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=60%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

# SD of 70%
scene3 <- c(rnorm(1500, mean=20, sd=0.70*20))#Using only one mean(20)

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

p701 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30))#Using mean(20 and 30)

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

p702 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20 and 30, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.70*40))
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

p703 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.70*40), rnorm(1500, mean=50, sd=0.70*50))

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

p704 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1500, mean=40, sd=0.70*40), rnorm(1500, mean=50, sd=0.70*50), 
            rnorm(1500, mean=60, sd=0.70*60))

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

p705 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.70*40), rnorm(1500, mean=50, sd=0.70*50), 
            rnorm(1500, mean=60, sd=0.70*60), rnorm(1500, mean=70, sd=0.70*70))

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

p706 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene3 <- c(rnorm(1500, mean=20, sd=0.70*20), rnorm(1500, mean=30, sd=0.70*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.70*40), rnorm(1500, mean=50, sd=0.70*50), 
            rnorm(1500, mean=60, sd=0.70*60), rnorm(1500, mean=70, sd=0.70*70),
            rnorm(1500, mean=80, sd=0.70*80))
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

p707 <- ggplot(data=clust_scene3grp, aes(x=as.factor(scene3_clusters), y=scene3), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=70%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#SD of 80%
scene4 <- c(rnorm(1500, mean=20, sd=0.8*20))#Using only one mean(20)

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

p801 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30))#Using mean(20 and 30)

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

p802 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20 and 30, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.8*40))

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

p803 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.8*40), rnorm(1500, mean=50, sd=0.8*50))

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

p804 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1500, mean=40, sd=0.8*40), rnorm(1500, mean=50, sd=0.8*50), 
            rnorm(1500, mean=60, sd=0.8*60))

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

p805 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.8*40), rnorm(1500, mean=50, sd=0.8*50), 
            rnorm(1500, mean=60, sd=0.8*60), rnorm(1500, mean=70, sd=0.8*70))

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

p806 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene4 <- c(rnorm(1500, mean=20, sd=0.8*20), rnorm(1500, mean=30, sd=0.8*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.8*40), rnorm(1500, mean=50, sd=0.8*50), 
            rnorm(1500, mean=60, sd=0.8*60), rnorm(1500, mean=70, sd=0.8*70),
            rnorm(1500, mean=80, sd=0.8*80))

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


p807 <- ggplot(data=clust_scene4grp, aes(x=as.factor(scene4_clusters), y=scene4), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=80%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

#SD of 90% 
scene1 <- c(rnorm(1500, mean=20, sd=0.9*20))#Using only one mean(20)

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

p901 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30))#Using mean(20 and 30)

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

p902 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20 and 30, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30), #Using mean(20,30 and 40)
            rnorm(1500, mean=40, sd=0.9*40))

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

p903 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30 and 40, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30), #Using mean(20,30, 40 and 50)
            rnorm(1500, mean=40, sd=0.9*40), rnorm(1500, mean=50, sd=0.9*50))

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

p904 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40 and 50, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30),#Using mean(20,30, 40,50 and 60)
            rnorm(1500, mean=40, sd=0.9*40), rnorm(1500, mean=50, sd=0.9*50), 
            rnorm(1500, mean=60, sd=0.9*60))

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

p905 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50 and 60, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30), #Using mean(20,30,40,50,60 and 70)
            rnorm(1500, mean=40, sd=0.9*40), rnorm(1500, mean=50, sd=0.9*50), 
            rnorm(1500, mean=60, sd=0.9*60), rnorm(1500, mean=70, sd=0.9*70))

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

p906 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60 and 70, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")

scene1 <- c(rnorm(1500, mean=20, sd=0.9*20), rnorm(1500, mean=30, sd=0.9*30), #Using mean(20,30,40,50,60,70 and 80)
            rnorm(1500, mean=40, sd=0.9*40), rnorm(1500, mean=50, sd=0.9*50), 
            rnorm(1500, mean=60, sd=0.9*60), rnorm(1500, mean=70, sd=0.9*70),
            rnorm(1500, mean=80, sd=0.9*80))

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


p907 <- ggplot(data=clust_scene1grp, aes(x=as.factor(scene1_clusters), y=scene1), color="blue") +
  geom_boxplot()+
  geom_jitter()+
  labs(title= "Clustering classification based  mean= 20,30,40,50,60,70 and 80, sd=90%")+
  labs(x = "clustering classification", fill = "cluster classes",
       y = "Prevalence")


#Combine all plots on same pdf

library(cowplot)
top_plots1 <- plot_grid (p401, p501)
middle_plots1 <- plot_grid(p601, p701)
bottom_plots1 <- plot_grid(p801, p901)
All_clus_plots1v <- plot_grid(top_plots1, middle_plots1, bottom_plots1, label_size = 12, ncol = 1)

top_plots2 <- plot_grid (p402, p502)
middle_plots2 <- plot_grid(p602, p702)
bottom_plots2 <- plot_grid(p802, p902)
All_clus_plots2v <- plot_grid(top_plots2, middle_plots2, bottom_plots2, label_size = 12, ncol = 1)

top_plots3 <- plot_grid (p403, p503)
middle_plots3 <- plot_grid(p603, p703)
bottom_plots3 <- plot_grid(p803, p903)
All_clus_plots3v <- plot_grid(top_plots3, middle_plots3, bottom_plots3, label_size = 12, ncol = 1)

top_plots4 <- plot_grid (p404, p504)
middle_plots4 <- plot_grid(p604, p704)
bottom_plots4 <- plot_grid(p804, p904)
All_clus_plots4v <- plot_grid(top_plots4,middle_plots4,bottom_plots4, label_size = 12, ncol = 1)

top_plots5 <- plot_grid (p405, p505)
middle_plots5 <- plot_grid(p605, p705)
bottom_plots5 <- plot_grid(p805, p905)
All_clus_plots5v <- plot_grid(top_plots5, middle_plots5, bottom_plots5, label_size = 12, ncol = 1)

top_plots6 <- plot_grid (p406, p506)
middle_plots6 <- plot_grid(p606, p706)
bottom_plots6 <- plot_grid(p806, p906)
All_clus_plots6v <- plot_grid(top_plots6, middle_plots6, bottom_plots6, label_size = 12, ncol = 1)

top_plots7 <- plot_grid (p407, p507)
middle_plots7 <- plot_grid(p607, p707)
bottom_plots7 <- plot_grid(p807, p907)
All_clus_plots7v <- plot_grid(top_plots7, middle_plots7, bottom_plots7, label_size = 12, ncol = 1)

###-----------End




