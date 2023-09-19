rm(list=ls())
## -----------------------------------------
### Paths
## -----------------------------------------
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
#CsvDir = file.path(DHS_data, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all', 'New_082321')


### Required functions and settings
## -----------------------------------------
source("sampling/functions.R")


location = "Kano"


if (location == "Ibadan"){
  
  # ----------------------------------------------
  ### Visualizing urban ward and LGA boundaries  
  ## ---------------------------------------------
  
  ## read ward shape files
  df = st_read(file.path(DataDir, "kano_ibadan_shape_files", "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>% 
    mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) 
  
  p1=ggplot(df) +
    geom_sf(aes(fill = WardName))+
    #geom_text_repel(
      #data = df,
      #aes(label =  WardName, geometry = geometry),color ='black',
      #stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
    map_theme() + 
    #labs(title= "Wards in Ibadan")+
    xlab("")+
    ylab("")+
    theme(legend.position = "none")
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), '_ibadan_metro_colored_by_wards.pdf'), p1, width = 10, height =8)
  
  #create long and lat column 
  df_sim = df %>% st_centroid() %>%  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                                   lat = sf::st_coordinates(.)[,2])
  st_geometry(df_sim) <- NULL
  
  
  #read and visualize LGA shape files
  df_LGA = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Ibadan_metro_fiveLGAshapes", "Ibadan_metro_fiveLGAshapes.shp"))
  
  p2=ggplot(df_LGA ) +
    geom_sf(fill=alpha('#8971B3', 0.5))+
    geom_text_repel(
      data = df_LGA,
      aes(label =  LGAName, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    map_theme() + 
    #labs(title= "LGAs in Ibadan")+
    xlab("")+
    ylab("")
  
  #make LGA and ward map
 p3 <- ggplot(data =  df_LGA) +
    geom_sf(data=df, aes(fill = LGACode))+
    geom_text_repel(
      data = df_LGA,
      aes(label =  LGAName, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() +
    xlab("") +
    ylab("")
  
  #get LGA name and code and add to data file for simulation 
  sim_LGA = df_LGA  %>%  dplyr::select(LGACode, LGAName)
  df_sim = left_join(df_sim, sim_LGA)
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_LGAs_ibadan.pdf'), ib_lmall, width = 4, height =5)
  
  p =  p1 + p2 + plot_annotation(tag_levels = 'A')&
    theme(plot.tag = element_text(size = 12, face = 'bold'))
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_LGAs_wards_Ibadan.pdf'), p, width = 8, height =5)
  
  #change crs to faciliate join 
  st_crs(df)<- 4326
  st_crs(df_LGA)<- 4326
  
  
  #join ward files to LGAs files to obtain data from LGA files and visualize 
  # df_viz=st_join(df, df_LGA, join =st_within)
  # 
  # p=ggplot(df_viz) +
  #   geom_sf(color=alpha('#8971B3', 0.5))+
  #   geom_text_repel(
  #     data = df_wall,
  #     aes(label =  LGAName, geometry = geometry),color ='black',
  #     stat = "sf_coordinates", min.segment.length = 0, size = 2.5, force = 1)+
  #   map_theme() + 
  #   #labs(title= "All LGAs and wards in Ibadan")+
  #   xlab("")+
  #   ylab("")
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'LGAs_wards_ibadan.png'),p, width = 10, height =5)
  
  
  ## --------------------------------------------------------------
  ### Visualizing population sizes for all wards  
  ## -------------------------------------------------------------
  
  #read visualize Ward Population size shape files
  df_pop = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp"))%>%
    mutate(wrd_nm_x = ifelse(wrd_nm_x == 'Oranyan' & lga_cod == '31007', 'Oranyan_7', wrd_nm_x)) %>% 
    filter(wrd_nm_x %in% df$WardName)%>%
    filter(stat_cd=='OY', amap_cd!='NIE OYS GBH', amap_cd!='NIE OYS JND',amap_cd!='NIE OYS AJW', amap_cd!='NIE OYS YYY', amap_cd!='NIE OYS RUW', amap_cd!='NIE OYS FMT')
  
  p = con_gplot(df_pop,quo(mean), quo(wrd_nm_x))+
    scale_fill_continuous(name='Ward population \n count, Ibadan', low="thistle2", high="darkred", guide="colorbar",na.value="transparent")
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'GRID3_wards_population_ibadan.pdf'), p, width = 12, height =8)
  
  #combine ward, LGA location map with ward population count for ibadan
p_all= p3 + p
  
#ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'ward_LGA_maap_GRID3_wards_population_ibadan.pdf'), p_all, width = 8, height =7)
  
  ##creating dataframe for simulation 
  df_pop_sim = df_pop %>% dplyr::select(wrd_nm_x, population=median)
  st_geometry(df_pop)<- NULL
  
  
  df_lo = df_sim %>% 
    mutate(nodeid = 1:59) %>%  
    dplyr::select(nodeid, WardName, lon, lat, LGACode)
  
  
  df_sim = left_join(df_lo, df_pop, by= c("WardName" = "wrd_nm_x"))
  
  write.csv(df_sim, file.path(mod_file, "Ibadan_ward_pop_1.csv"))
  
  
  
  # ---------------------------------------------------------------
  ### Visualizing population density for all wards  
  ## -------------------------------------------------------------
  raster <- raster(file.path(Raster_Dir, "NGA_pop_density", "gpw_v4_population_density_rev11_2020_1_deg.tif"))
  pop_den <- raster::extract(raster,df, buffer = buffer, fun = mean, df =TRUE)
  pop_den = pop_den$gpw_v4_population_density_rev11_2020_1_deg
  df_popd = cbind(df, pop_den)
  
  pd = con_gplot(df_popd,quo(pop_den), quo(WardName))+
    scale_fill_continuous(name='Population \n density', low="cornsilk", high="cornsilk4", guide="colorbar",na.value="transparent")
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'GRID3_wards_population_density_ibadan.pdf'), pd, width = 12, height =8)
  
  
  # scatter plot and bivariate map of population size and density 
  pop_den_b = df_popd %>%  dplyr::select(WardName, pop_den) 
  
  p_size_den = left_join(df_pop, pop_den_b, by = c('wrd_nm_x'= 'WardName'))
  
  p=ggplot(p_size_den, aes(mean, pop_den)) +
    geom_point(shape = 21, color=alpha('skyblue3', 0.7), fill = alpha('skyblue3', 0.7), size = 4) +
    geom_smooth(se = FALSE)+
    theme_manuscript()+
    xlab('Population size')+
    ylab('Population density')
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'ibadan_population_size_density_smooth_plot.pdf'),p, width=4, height=3)
  
  dt = bi_class(p_size_den, x = mean, y = pop_den, style = "fisher", dim = 3)
  
  dt_np = left_join(df, dt, by = c('WardName'= 'wrd_nm_x'))
  
  map <- ggplot() +
    geom_sf(data = dt_np, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    geom_text_repel(
      data = dt_np,
      aes(label = WardName, geometry = geometry.x),color ='black',
      stat = "sf_coordinates", 
      min.segment.length = 0, size = 5, force = 1)+
    xlab('')+
    ylab('')+
    bi_theme()
  legend <- bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "High population size",
                      ylab = "High population density",
                      size = 4)
  finalPlot <- ggdraw() +
    draw_plot(map, 0.1, 0, 1, 1) +
    draw_plot(legend, 0, 0.65, 0.2, 0.2)
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_population_size_density.pdf'),finalPlot) 
  
  # ---------------------------------------------------------------
  ### Visualizing settlements  
  ## --------------------------------------------------------------
  
  #Ibadan
  #settlement type overall 
  df_settle = st_read(file.path(DataDir, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Oyo', landuse =='Residential')
  df_settle=st_join(df, df_settle, join =st_overlaps)
  bar_dat = df_settle
  st_geometry(bar_dat) <- NULL
  
  bar_overall = df_settle %>% dplyr::select(type) %>%  group_by(type) %>%  summarise(number=n())
  scale_fill = c("darkorchid1", "firebrick1", "forestgreen", "darksalmon", "burlywood4")
  settle_type = bar_fun(bar_overall, quo(type), quo(number), quo(type), scale_fill, 20, 20, 20, 20, 'Settlement type per neighborhood')
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_overall_bars.png'),settle_type, width = 6, height =3)
  
  #prepare settlement data (convert settlement data to proportion)
  settle_type_all = df_settle %>% 
    mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) %>% 
    dplyr::select(WardName, settle_type=type, LGACode)%>% group_by(WardName, settle_type)%>% summarise(number=n())
  #make dataset for computation of proportion 
  st_geometry(settle_type_all) <- NULL 
  
  settle_type_wide_all = settle_type_all %>%  pivot_wider(names_from = settle_type, values_from=number)   
  settle_type_wide_all[is.na(settle_type_wide_all)]= 0
  colnames(settle_type_wide_all) = paste('settlement', colnames(settle_type_wide_all), sep = '_')
  colnames(settle_type_wide_all)[1] = 'WardName'
  settle_type_wide_all$prop_A_settlement = settle_type_wide_all$settlement_A/rowSums(settle_type_wide_all[, c(2:6)]) 
  settle_prop = settle_type_wide_all %>%  dplyr::select(WardName, prop_A_settlement)
  
  settle_A = left_join(df, settle_prop, by=c('WardName'))
  
  settle_A_map = con_gplot(settle_A,quo(prop_A_settlement), quo(WardName))+
    scale_fill_continuous(name='Proportion of \n settlement type A', low="slategray1", high="blue", guide="colorbar",na.value="transparent")
  
  
  # ---------------------------------------------------------------
  ### Visualizing informality 
  ## --------------------------------------------------------------
  
  #ibadan 
  df_inform <- geojson_read(file.path(DataDir, 'kano_ibadan_informality', "kano_ibadan_settlements.geojson"),  what = "sp")
  inform_sf <- st_as_sf(df_inform) %>%  filter(regions == 'Ibadan')
  df_wall = df%>%dplyr::select(-c(StateCode, Timestamp, AMAPCODE))
  w_inform=st_join(df_wall, inform_sf, join =st_overlaps)%>% 
    mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName)) %>% 
    dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity))
  #st_geometry(w_inform)<- NULL
  
  # all wards 
  settle_type_w = ggplot() + 
    geom_sf(data =w_inform, mapping = aes(fill = mean_k)) +
    scale_fill_gradient(name = 'K complexity', low = 'mintcream', high = 'plum')+
    geom_text_repel(
      data = w_inform,
      aes(label = WardName, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 5, force = 1)+
    map_theme()+
    xlab('')+
    ylab('')
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_all_wards.pdf'),settle_type_w) 
  
  #creating a population size and informality visualization combined 
  # pop_inform = left_join(df_pop, w_inform, by = c('wrd_nm_x' = 'WardName'))
  # 
  # dt_i = bi_class(pop_inform, x = mean, y = mean_k, style = "fisher", dim = 3) 
  # 
  # dt_ni = left_join(df, dt_i, by = c('WardName'= 'wrd_nm_x'))
  # 
  # map <- ggplot() +
  #   geom_sf(data = dt_ni, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  #   bi_scale_fill(pal = "DkBlue", dim = 3) +
  #   geom_text_repel(
  #     data = dt_ni,
  #     aes(label = WardName, geometry = geometry.x),color ='black',
  #     stat = "sf_coordinates",
  #     min.segment.length = 0, size = 5, force = 1)+
  #   xlab('')+
  #   ylab('')+
  #   bi_theme()
  # 
  # legend <- bi_legend(pal = "DkBlue",
  #                     dim = 3,
  #                     xlab = "High population size ",
  #                     ylab = "High K complexity ",
  #                     size = 7.5)
  # finalPlot <- ggdraw() +
  #   draw_plot(map, 0, 0, 1, 1) +
  #   draw_plot(legend, 0, .65, 0.2, 0.2)
  # 
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_population_all_wards.pdf'),finalPlot)
  
  
  #------------------------------------------------------------------------------
  ###Visualizing EVI data 
  #------------------------------------------------------------------------------
  #ibadan 
  files = list.files(path = file.path(DataDir, "EVI_nigeria_2020"), pattern = ".tif", full.names = TRUE)
  files= sapply(files, raster, simplify = F)
  EVI = files %>%  purrr::map(~raster::extract(., df, buffer = buffer, fun = mean, df =TRUE))
  EVI_all = EVI  %>%  purrr::reduce(left_join, by =c("ID"))
  EVI_all$EVI_mean = rowMeans(EVI_all[ , c(2:13)], na.rm=TRUE)
  df_walld_EVI = cbind(df_popd, EVI_all$EVI_mean)
  
  w_EVI = con_gplot(df_walld_EVI,quo(EVI_all.EVI_mean), quo(WardName))+
    scale_fill_continuous(name='EVI', low="darkseagreen1", high="darkseagreen4", guide="colorbar",na.value="transparent")
  
  
  #------------------------------------------------------------------------------
  ###Visualizing housing data 
  #------------------------------------------------------------------------------
  #housing 2015 
  #Ibadan 
  raster <- raster(file.path(Raster_Dir, "housing/2019_Nature_Africa_Housing_2015_NGA.tiff"))
  val_housing <- raster::extract(raster,df, buffer = buffer, fun = mean, df =TRUE)
  val_housing_15 = val_housing$X2019_Nature_Africa_Housing_2015_NGA
  df_wall_house = cbind(df_walld_EVI, val_housing_15)
  
  w_house= con_gplot(df_wall_house,quo(val_housing_15), quo(WardName))+
    scale_fill_continuous(name='Housing quality', low="red", high="lightpink", guide="colorbar",na.value="transparent")
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_housing_quality_wards.pdf'),ib_w_house)
  
  #------------------------------------------------------------------------------
  ###Visualizing dump site data 
  #------------------------------------------------------------------------------
  
  # dump site
  # Ibadan
  df_dump <- geojson_read(file.path(DataDir, 'oyo_dump_sites', "dump-sites.geojson"),  what = "sp")
  dump_sf <- st_as_sf(df_dump)
  st_crs(dump_sf)<- 4326
  
  w_dump=st_join(df, dump_sf, join =st_contains)
  dump_n_ward = w_dump %>% drop_na(name) %>% group_by(WardName) %>% summarise(num_dumpsites = n())
  st_geometry(dump_n_ward) <- NULL
  
  dump_dat = left_join(df_wall_house, dump_n_ward, by = c("WardName")) %>% 
    mutate_at(vars(num_dumpsites), ~replace(., is.na(.), 0))
  
  w_dumps= con_gplot(dump_dat,quo(num_dumpsites), quo(WardName))+
    scale_fill_continuous(name='# dumpsites', low="wheat1", high="darkgoldenrod", guide="colorbar",na.value="transparent")
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_dump_sites_wards.pdf'),ib_w_dumps) 
  
  
  #------------------------------------------------------------------------------
  ###combine plots 
  #------------------------------------------------------------------------------
  
  pplot =(pd + settle_A_map)/ (settle_type_w + w_EVI)/(w_house + w_dumps)+ plot_annotation(tag_levels = 'A')&
    theme(plot.tag = element_text(size = 12, face = 'bold')) 
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_ibadan_clust_var.pdf'),p, width = 8, height =10) 
  
  
  #------------------------------------------------------------------------------
  ###prepare dataset for model-based clustering 
  #------------------------------------------------------------------------------
  
  #ibadan 
  #make dataset for population density, EVI, housing, dump site 
  df_all_var = dump_dat %>%  dplyr::select(WardName, pop_den, EVI_all.EVI_mean, val_housing_15, num_dumpsites)
  
  
  #join A settlement proportion and k complexity data 
  inform_k = left_join(settle_prop, w_inform, by =c("WardName"))
  
  dfnn <- data.frame (df_all_var)
  str(dfnn)
  #join all data 
  df_all_var= left_join(dfnn, inform_k, by = c("WardName")) 
  all_wards = dfnn$WardName
  
  df_all_var_c= df_all_var %>% dplyr::select(-c(WardName, geometry.x, geometry.y))
  
  #st_geometry(df_all_var_c) <- NULL
  
  #write.csv(df_all_var_c, file.path(mod_file, "Ibadan_variables.cluster.csv"), row.names = FALSE)
  df_all_var_cn = read.csv (file.path(mod_file, "Ibadan_variables.cluster.csv")) %>% dplyr::select(pop_den, EVI_all.EVI_mean, 
                                                                                                   val_housing_15, num_dumpsites, mean_k, prop_A_settlement)
  
  view(df_all_var_cn)
  
  #------------------------------------------------------------------------------
  ###Variable Selection using clustvarsel packagae
  #------------------------------------------------------------------------------
  
  #ibadan
  out<- clustvarsel(df_all_var_c)
  summary(out$model) #VII model suggests the presence of 4 clusters 
  
  
  df_bic = data.frame(dumpsites = df_all_var$num_dumpsites, housing = df_all_var$val_housing_15)
  
  # BIC plot clustering for all wards with variable selection 
  BIC <- mclustBIC(df_bic, G=1:7)  
  pdf(paste0(SampDir, '/new_results/', Sys.Date(), '_model_selection_BIC_var_selection.pdf'))
  plot(BIC)
  dev.off()
  summary(BIC) #model with the lowest BIC is EII but clustering chooses wrong model
  
  #getting clusters for mapping 
  all_clusters=data.frame(out$model$classification) %>% rownames_to_column()
  all_= df_all_var_c%>% rownames_to_column() 
  all_class = left_join(all_, all_clusters)
  all_class_1 = cbind(all_wards, all_class) %>%  dplyr::select(WardName = all_wards,
                                                               out.model.classification)
  
  
  
  #make a map of all clusters 
  map_df_all = df%>% left_join(all_class_1) %>% filter(!is.na(out.model.classification))
  
  all_map_selected =con_gplot(map_df_all,quo(factor(out.model.classification)), quo(WardName))+
    scale_fill_manual(name='Cluster Number', values = c("#8FCD9B", "#EBD362", "#EB8F52", "#FFF2D2"))
  
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'urban_clusters_var_selection_based.pdf'), all_map_selected, width = 8, height =9)
  
  #make high population size plot with cluster labels 
  p_dat = dt_np %>% dplyr::select(WardName, bi_class)  
  st_geometry(p_dat) <- NULL
  
  map_df_all = map_df_all %>%  left_join(p_dat)
  
  p <- ggplot() +
    geom_sf(data =   map_df_all, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    geom_text_repel(
      data =   map_df_all,
      aes(label = out.model.classification, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 5, force = 1)+
    xlab('')+
    ylab('')+
    bi_theme()
  
  legend <- bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "High population size ",
                      ylab = "High population density ",
                      size = 7.5)
  finalPlot <- ggdraw() +
    draw_plot(p, 0, 0, 1, 1) +
    draw_plot(legend, 0, .65, 0.2, 0.2)
  
  all_p = all_map_selected + finalPlot
  
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Ibadan_clusters_high_popden_size.png'), all_p)
  
  #adding archetype wards 
  ward_arch = all_class_1 %>%  mutate(Archetype = case_when(
    out.model.classification == 1 ~ "Bashorun",
    out.model.classification == 2 ~ "Agugu",
    out.model.classification == 3 ~ "Challenge",
    TRUE ~ "Olopomewa")) 
  
  #read in old ward population file and join with clustering output
  pop_ibadan =read.csv(file.path(mod_file, "Ibadan_ward_pop_1.csv")) %>%  dplyr::select(-X, LGA=WardName) %>%  mutate(State = 'Oyo')
  sim_dat = left_join(pop_ibadan, ward_arch, by =c("LGA" = "WardName"))
  sim_dat = sim_dat %>%  dplyr::select(nodeid, Ward = LGA, lon, lat,LGACode, population=median, out.model.classification, Archetype)
  
  #add LGA name 
  LGA_df= df_LGA %>% dplyr::select(LGACode, LGAName)
  st_geometry(LGA_df)<-NULL
  LGA_df$LGACode = as.character(LGA_df$LGACode)
  sim_dat$LGACode = as.character(sim_dat$LGACode)
  sim_dat = sim_dat %>%  left_join(LGA_df)
  
  write.csv(sim_dat, paste0(mod_file, "/", Sys.Date(), "_Ibadan_ward_pop.csv"), row.names = FALSE)
  
  
  
  ##End of Ibadan Clustering##------------------------------------------------------------------------------------------  
  
  
}else {
  
  # ----------------------------------------------
  ### Visualizing urban ward and LGA boundaries  
  ## ---------------------------------------------
  
  #kano 
  df = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 
  
  p1 <- ggplot(df) +
    geom_sf(aes(fill= WardName))+
    #geom_text_repel(
      #data = df,
      #aes(label = WardName, geometry = geometry),color ='black',
      #stat = "sf_coordinates", 
      #min.segment.length = 0, size = 4, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')+
    map_theme() 
  #labs(title= "Wards in Kano")+
  
  #ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'wards_kano_metro.pdf'), p1, width = 10, height =8)
  
  #create long and lat column 
  df_sim = df  %>% st_centroid() %>%  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                                      lat = sf::st_coordinates(.)[,2])
  st_geometry(df_sim) <- NULL
  
  
  #read and visualize LGA shape files
  df_LGA = st_read(file.path(DataDir, "kano_ibadan_shape_files", "Kano_metro_sixLGA_shapes", "Kano_metro_sixLGA_shapes.shp"))
  
  p2= ggplot(df_LGA) +
    geom_sf(fill=alpha('tan3', 0.5))+
    geom_text_repel(
      data = df_LGA,
      aes(label =  LGAName, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    map_theme() + 
    #labs(title= "LGAs in Kano")+
    xlab("")+
    ylab("")
  
  st_crs(df)<- 4326
  st_crs(df_LGA)<- 4326
  
  #make LGA and ward map
  p3 <- ggplot(data =df_LGA) +
    geom_sf(data=df, aes(fill = LGACode))+
    geom_text_repel(
      data = df_LGA,
      aes(label =  LGAName, geometry = geometry),color ='black',
      stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
    map_theme() +
    xlab("") +
    ylab("")
  
  
  #get LGA name and code and add to data file for simulation 
  sim_LGA = df_LGA %>%  dplyr::select(LGACode, LGAName)
  st_geometry(sim_LGA) <- NULL
  df_sim = left_join(df_sim, sim_LGA)
  
  p = p1 + p2  + plot_annotation(tag_levels = 'A')&
    theme(plot.tag = element_text(size = 12, face = 'bold'))
  
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), '_LGAs_kano.pdf'), p2, width = 8, height =5)
  
  
  #change CRS to facilitate join
  
  st_crs (df) <- 4326
  st_crs (df_LGA) <- 4326
  
  
  ## --------------------------------------------------------------
  ### Visualizing population sizes for all wards  
  ## -------------------------------------------------------------
  
  #requires some data cleaning 
  df_pop = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp"))%>%
    filter(wrd_nm_x %in% df$WardName) %>% 
    filter(stat_cd=='KN') %>%
    filter(amap_cd!='NIE BOS JRE DAL')%>%
    filter(amap_cd!='NIE JIS GML ZAN')%>%
    filter(amap_cd!='NIE KNS SNN GRD')%>%
    arrange(wrd_nm_x)
  
  st_crs(df)<- 4326
  
  df_pop = st_join(df_pop, df, join =st_equals) %>% group_by(wrd_nm_x) %>% filter(duplicated(wrd_nm_x) | n()==1) %>% 
    filter(wrd_nm_x != 'Yalwa') %>%  dplyr::select(c(1:9, 11:14))
  st_geometry(df_pop)  <- NULL
  
  #get 4 missing LGAs 
  df_zango = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp")) %>% 
    filter(stat_cd=='KN') %>% 
    filter(wrd_nm_x == "Zango" & lga_cod == "20043")  %>% dplyr::select(-c('wrd_nm_y'))
  st_geometry( df_zango) <- NULL 
  
  df_dala = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp")) %>% 
    filter(stat_cd=='KN') %>% 
    filter(wrd_nm_x == "Dala" & lga_cod == "20024")  %>% dplyr::select(-c('wrd_nm_y'))
  st_geometry(df_dala) <- NULL 
  
  
  df_yalwa = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp")) %>% 
    filter(stat_cd=='KN') %>% 
    filter(wrd_nm_x == "Yalwa" & lga_cod == "20024")  %>% dplyr::select(-c('wrd_nm_y'))
  st_geometry(df_yalwa) <- NULL 
  
  Goron = st_read(file.path(DataDir, "nigeria_wardpop", "GRID3_ward_admin_pop.shp")) %>% 
    filter(stat_cd=='KN') %>% 
    filter(wrd_nm_x == 'Goron Dutse' & urban == '1')%>% dplyr::select(-c('wrd_nm_y'))
  st_geometry(Goron)<- NULL 
  
  df_pop_ = rbind(df_pop, df_zango, df_dala, df_yalwa, Goron)
  
  #join to ward 
  df_pop_n = left_join(df, df_pop_, by =c('WardName'  = 'wrd_nm_x'))
  
  
  pop = df_pop_n %>% dplyr::select(Ward=WardName, population=median)
  st_geometry(pop)<- NULL
  
  df_lo = df_sim %>% mutate(nodeid = 1:66) %>%  dplyr::select(nodeid, Ward=WardName, lon, lat)
  
  df_ko = left_join(df_lo,pop)
  
  #write.csv(df_ko, file.path(mod_file, "Kano_ward_pop.csv"), row.names = FALSE)
  
  
  p = con_gplot(df_pop_n,quo(mean), quo(WardName))+
    scale_fill_continuous(name='Ward population \n count, kano', low="thistle2", high="darkred", guide="colorbar",na.value="transparent")
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'GRID3_wards_population_kano.pdf'), p, width = 12, height =8)
 
#combine ward and population map 
p_all = p3 + p
ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'ward_LGA_GRID3_wards_population_kano.pdf'), p_all, width = 8, height =7)

  #----------------------------------------------------------
  
  ### Visualizing population density for all wards
  
  ##---------------------------------------------------------
  
  raster <- raster(file.path(Raster_Dir, "NGA_pop_density", "gpw_v4_population_density_rev11_2020_1_deg.tif"))
  pop_den <- raster::extract(raster, df, buffer = buffer, fun = mean, df =TRUE)
  pop_den = pop_den$gpw_v4_population_density_rev11_2020_1_deg
  df_popd = cbind(df, pop_den)
  
  pd = con_gplot(df_popd,quo(pop_den), quo(WardName))+
    scale_fill_continuous(name='Population \n density', low="cornsilk", high="cornsilk4", guide="colorbar",na.value="transparent")
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'GRID3_wards_population_density_kano.pdf'), pd, width = 12, height =8)
  
  
  
  # scatter plot and bivariate map of population size and density 
  pop_den_b = df_popd %>%  dplyr::select(WardName, pop_den) 
  
  #st_geometry(pop_den_ko)<- NULL
  
  st_geometry(df_pop_n)<-NULL
  p_size_den = left_join(df_pop_n, pop_den_b)
 
  
  #p_data = p_size_den %>%  filter(mean > 50000)
  
  
  p=ggplot(p_size_den, aes(mean, pop_den)) +
    geom_point(shape = 21, color=alpha('skyblue3', 0.7), fill = alpha('skyblue3', 0.7), size = 4) +
    geom_smooth(se = FALSE)+
    theme_manuscript()+
    xlab('Population size')+
    ylab('Population density')
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'kano_population_size_density_smooth_plot.pdf'),p, width=4, height=3)
  
  dt = bi_class(p_size_den, x = mean, y =  pop_den, style = "fisher", dim = 3)
  
  dt_np = left_join (df, dt, by = c("WardName"))
  
  map <- ggplot() +
    geom_sf(data = dt_np, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    geom_text_repel(
      data = dt_np,
      aes(label = WardName, geometry = geometry.y),color ='black',
      stat = "sf_coordinates", 
      min.segment.length = 0, size = 5, force = 1)+
    xlab('')+
    ylab('')+
    bi_theme()
  legend <- bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "High population size",
                      ylab = "High population density",
                      size = 4)
  finalPlot <- ggdraw() +
    draw_plot(map, 0.1, 0, 1, 1) +
    draw_plot(legend, 0, 0.65, 0.2, 0.2)
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'kano_population_size_density.pdf'),finalPlot)
  
  
  #}
  
  
  # ---------------------------------------------------------------
  ### Visualizing settlements  
  ## --------------------------------------------------------------
  
  #kano
  #settlement type overall 
  
  df_settle = st_read(file.path(DataDir, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Kano', landuse =='Residential')
  
  
  df_settle=st_join(df, df_settle, join =st_overlaps)
  
  bar_dat = df_settle
  st_geometry(bar_dat) <- NULL
  
  bar_overall = df_settle %>% dplyr::select(type) %>%  group_by(type) %>%  summarise(number=n())
  scale_fill = c("darkorchid1", "firebrick1", "forestgreen", "darksalmon", "burlywood4")
  settle_type = bar_fun(bar_overall, quo(type), quo(number), quo(type), scale_fill, 20, 20, 20, 20, 'Settlement type per neighborhood')
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_settlement_types_overall_bars.png'),settle_type, width = 6, height =3)
  
  
  #prepare settlement data (convert settlement data to proportion)
  
  settle_type_all = df_settle %>% 
    dplyr::select(WardName, settle_type=type, LGACode)%>% group_by(WardName, settle_type)%>% summarise(number=n())
  
  #make dataset for computation of proportion 
  st_geometry(settle_type_all) <- NULL 
  
  settle_type_wide_all = settle_type_all %>%  pivot_wider(names_from = settle_type, values_from=number)   
  settle_type_wide_all[is.na(settle_type_wide_all)]= 0
  colnames(settle_type_wide_all) = paste('settlement', colnames(settle_type_wide_all), sep = '_')
  colnames(settle_type_wide_all)[1] = 'WardName'
  settle_type_wide_all$prop_B_settlement = settle_type_wide_all$settlement_B/rowSums(settle_type_wide_all[, c(2:6)]) 
  settle_prop = settle_type_wide_all %>%  dplyr::select(WardName, prop_B_settlement)
  
  settle_B = left_join(df, settle_prop, by=c('WardName'))
  
  settle_B_map = con_gplot(settle_B,quo(prop_B_settlement), quo(WardName))+
    scale_fill_continuous(name='Proportion of \n settlement type B', low="slategray1", high="blue", guide="colorbar",na.value="transparent")
  
  
  # ---------------------------------------------------------------
  ### Visualizing informality 
  ## --------------------------------------------------------------
  
  #kano 
  df_inform <- geojson_read(file.path(DataDir, 'kano_ibadan_informality', "kano_ibadan_settlements.geojson"),  what = "sp")
  inform_sf <- st_as_sf(df_inform) %>%  filter(regions == 'Kano')
  df_wall = df%>%dplyr::select(-c(StateCode, Timestamp, AMAPCODE))
  w_inform=st_join(df_wall, inform_sf, join =st_overlaps) %>% 
    dplyr::select(WardName, k_complexity) %>%  group_by(WardName) %>%  summarise(mean_k=mean(k_complexity))
  
  # all wards 
  settle_type_w = ggplot() + 
    geom_sf(data =w_inform, mapping = aes(fill = mean_k)) +
    scale_fill_gradient(name = 'K complexity', low = 'mintcream', high = 'plum')+
    geom_text_repel(
      data = w_inform,
      aes(label = WardName, geometry = geometry),color ='black',
      stat = "sf_coordinates", 
      min.segment.length = 0, size = 1.5, force = 1)+
    map_theme()+
    xlab('')+
    ylab('')
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_all_wards.pdf'),settle_type_w) 
  
  #creating a population and informality visualization combined 
  #st_geometry(ib_w_inform)<- NULL
  # pop_inform = left_join(df_pop, w_inform, by = c('wrd_nm_x' = 'WardName'))
  # 
  # dt_i = bi_class(pop_inform, x = mean, y = mean_k, style = "fisher", dim = 3)
  # 
  # dt_ni = left_join(df, dt_i, by = c('WardName' = 'wrd_nm_x'))
  # 
  # map <- ggplot() +
  #   geom_sf(data = dt_ni, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  #   bi_scale_fill(pal = "DkBlue", dim = 3) +
  #   geom_text_repel(
  #     data = dt_ni,
  #     aes(label = WardName, geometry = geometry.x),color ='black',
  #     stat = "sf_coordinates", 
  #     min.segment.length = 0, size = 5, force = 1)+
  #   xlab('')+
  #   ylab('')+
  #   bi_theme()
  # 
  # legend <- bi_legend(pal = "DkBlue",
  #                     dim = 3,
  #                     xlab = "High population ",
  #                     ylab = "High K complexity ",
  #                     size = 7.5)
  # 
  # finalPlot <- ggdraw() +
  #   draw_plot(map, 0, 0, 1, 1) +
  #   draw_plot(legend, 0, .65, 0.2, 0.2)
  # 
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_informality_population_all_wards.pdf'),finalPlot)
  
  
  
  
  #plotting informality
  #settle_type_ko = ggplot() + 
  # geom_sf(data =ko_w_inform, mapping = aes(fill = mean_k)) +
  #scale_fill_gradient(name = 'K complexity', low = 'mintcream', high = 'plum')+
  # geom_text_repel(
  #   data = ko_w_inform,
  #   aes(label = WardName, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", 
  #   min.segment.length = 0, size = 5, force = 1)+
  # map_theme()+
  # xlab('')+
  # ylab('')
  
  
  #------------------------------------------------------------------------------
  ###Visualizing EVI data 
  #------------------------------------------------------------------------------
  
  #kano 
  files = list.files(path = file.path(DataDir, "EVI_nigeria_2020"), pattern = ".tif", full.names = TRUE)
  files= sapply(files, raster, simplify = F)
  EVI = files %>%  purrr::map(~raster::extract(., df, buffer = buffer, fun = mean, df =TRUE))
  EVI_all = EVI  %>%  purrr::reduce(left_join, by =c("ID"))
  EVI_all$EVI_mean = rowMeans(EVI_all[ , c(2:13)], na.rm=TRUE)
  df_walld_EVI = cbind(df_popd, EVI_all$EVI_mean)
  
  w_EVI = con_gplot(df_walld_EVI,quo(EVI_all.EVI_mean), quo(WardName))+
    scale_fill_continuous(name='EVI', low="darkseagreen1", high="darkseagreen4", guide="colorbar",na.value="transparent")
  
  
  #------------------------------------------------------------------------------
  ###Visualizing housing data 
  #------------------------------------------------------------------------------
  
  #housing 2015 
  #kano
  
  raster <- raster(file.path(Raster_Dir, "housing/2019_Nature_Africa_Housing_2015_NGA.tiff"))
  val_housing <- raster::extract(raster,df, buffer = buffer, fun = mean, df =TRUE)
  val_housing_15 = val_housing$X2019_Nature_Africa_Housing_2015_NGA
  df_wall_house = cbind(df_walld_EVI, val_housing_15)
  
  w_house= con_gplot(df_wall_house,quo(val_housing_15), quo(WardName))+
    scale_fill_continuous(name='Housing quality', low="red", high="lightpink", guide="colorbar",na.value="transparent")
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_housing_quality_wards.pdf'),w_housek)
  
  
  
  #------------------------------------------------------------------------------
  ###Visualizing dump site data 
  #------------------------------------------------------------------------------
  
  # # dump site
  # # Kano
  # df_dumpk <- geojson_read(file.path(DataDir, 'kano_dump_sites', "dump-sites.geojson"),  what = "sp")
  # dump_sfk <- st_as_sf(df_dumpk)
  # st_crs(dump_sfk)<- 4326
  # 
  # w_dumpk=st_join(dfk, dump_sfk, join =st_contains)
  # dump_n_wardk = w_dumpk %>% drop_na(name) %>% group_by(WardName) %>% summarise(num_dumpsites = n())
  # st_geometry(dump_n_wardk) <- NULL
  # 
  #dump_datk = left_join(df_wall_housek, dump_n_wardk, by = c("WardName")) %>% 
    #   mutate_at(vars(num_dumpsites), ~replace(., is.na(.), 0))
    # 
    # w_dumps= con_gplot(dump_datk,quo(num_dumpsites), quo(WardName))+
    #   scale_fill_continuous(name='# dumpsites', low="wheat1", high="darkgoldenrod", guide="colorbar",na.value="transparent")
    # 
    # #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_dump_sites_wards.pdf'),w_dumps) 
    
    
    #------------------------------------------------------------------------------
  ###combine plots 
  #------------------------------------------------------------------------------
  
  pplot =(pd + settle_B_map)/ (settle_type_w + w_EVI)+ plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size = 12, face = 'bold')) #can add housing quality plot in the illustrator
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), '_kano_clust_var.pdf'),p_ko, width = 8, height =10) 
  
  #------------------------------------------------------------------------------
  ###prepare dataset for model-based clustering --population density and dumpsite not included
  #------------------------------------------------------------------------------
  
  #kano
  #make dataset for population density, EVI, housing
  df_all_var = df_wall_house %>%  dplyr::select(WardName,  EVI_all.EVI_mean, val_housing_15)
  
  
  #join B settlement proportion and k complexity data 
  
  inform_k = left_join(settle_prop, w_inform, by =c("WardName"))
  
  dfnn <- data.frame(df_all_var)
  
  #join all data 
  df_all_var = left_join(dfnn, inform_k, by = c("WardName")) 
  
  all_wards = dfnn$WardName
  
  df_all_var_c= df_all_var%>%  dplyr::select(-c(WardName, geometry.x, geometry.y))
  
  #st_geometry(df_var_ko)  <- NULL #stopped here
  
  
  write.csv(df_all_var_c, file.path(mod_file, "Kano_variables.cluster.csv"), row.names = FALSE)
  #df_all_var_cnk = read.csv (file.path(mod_file, "Kano_variables.cluster.csv")) %>% dplyr::select(pop_denk, EVI_allk.EVI_mean, 
                                                                                                 # val_housing_15k, mean_k, prop_A_settlement)
  head(df_all_var_c)
  #------------------------------------------------------------------------------
  ###Variable Selection using clustvarsel packagae
  #------------------------------------------------------------------------------
  
  #kano
  out<- clustvarsel(df_all_var_c)
  summary(out$model) 
  
  
  df_bic = data.frame(housing = df_all_var$val_housing_15)
  
  # BIC plot clustering for all wards with variable selection 
  
  BIC <- mclustBIC(df_bic, G=1:7)  
  
  #pdf(paste0(SampDir, '/results/', Sys.Date(), 'kano_model_selection_BIC_var_selection.pdf'))
  
  plot(BIC)
  
  #dev.off()
  
  summary(BIC) #model with the lowest BIC is EII but clustering chooses wrong model
  
  #getting clusters for mapping 
  all_clusters=data.frame(out$model$classification) %>% rownames_to_column()
  all_= df_all_var_c%>% rownames_to_column() 
  all_class = left_join(all_, all_clusters)
  
  all_class_1 = cbind(all_wards, all_class) %>%  dplyr::select(Ward=all_wards,
                                                                  out.model.classification)
  #make a map of all clusters 
  
  map_df_all= df%>% left_join(all_class_1, by = c("WardName" = "Ward"))
  view(map_df_all)
  
  all_map_selected =con_gplot(map_df_all,quo(factor(out.model.classification)), quo(WardName))+
    scale_fill_manual(name='Cluster Number', values =  c("#8FCD9B", "#EBD362", "#EB8F52", "#FFF2D2", "#939598"))
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Kano_urban_clusters_var_selection_based.pdf'), all_map_selected, width = 8, height =9)
  
  #make high population size plot with cluster labels 
  p_dat = dt_np %>% dplyr::select(WardName, bi_class)  
  st_geometry(p_dat) <- NULL
  
  map_df_all = map_df_all %>%  left_join(p_dat)
  
  p <- ggplot() +
    geom_sf(data =   map_df_all, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkBlue", dim = 3) +
    geom_text_repel(
      data =   map_df_all,
      aes(label = out.model.classification, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 5, force = 1)+
    xlab('')+
    ylab('')+
    bi_theme()
  
  legend <- bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "High population size ",
                      ylab = "High population density ",
                      size = 7.5)
  finalPlot <- ggdraw() +
    draw_plot(p, 0, 0, 1, 1) +
    draw_plot(legend, 0, .65, 0.2, 0.2)
  
  all_p = all_map_selected + finalPlot
  
  ggsave(paste0(SampDir, '/new_results/', Sys.Date(), 'Kano_clusters_high_popden_size.png'), all_p)
  
  #adding archetype wards 
  ward_arch = all_class_1 %>%  mutate(Archetype = case_when(
    out.model.classification == 1 ~ "Dorayi",
    out.model.classification == 2 ~ "Fagge D2",
    out.model.classification == 3 ~ "Tudun Wazurchi",
    out.model.classification == 4 ~ "Gobirawa",
    TRUE ~ "Zango")) 
  
  #read in old ward population file and join with clustering output
  pop_kano =read.csv(file.path(mod_file, "Kano_ward_pop.csv"))%>%  mutate(State = 'Kano')
  sim_dat = left_join(pop_kano, ward_arch)
  
 
  #add LGA name 
  lga_c = map_df_all %>%  dplyr::select(WardName, LGACode) #get LGA code 
  st_geometry(lga_c) <- NULL
  sim_dat = sim_dat %>%  left_join(lga_c, by =c("Ward" = "WardName"))
  LGA_df= df_LGA %>% dplyr::select(LGACode, LGAName)
  st_geometry(LGA_df)<-NULL
  LGA_df$LGACode = as.character(LGA_df$LGACode)
  sim_dat$LGACode = as.character(sim_dat$LGACode)
  sim_dat = sim_dat %>%  left_join(LGA_df)
  
  
 

  
  
  
 
  
  #ggsave(paste0(SampDir, '/results/', Sys.Date(), 'kano_urban_clusters_var_selection_based.pdf'),ko_all_map_selected, width = 8, height =9)
  
  
  
}

##End of Kano Clustering##------------------------------------------------------
