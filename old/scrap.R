# kadf <- data.frame(read.csv("C:\\Users\\USER\\Documents\\UrbanMalaria\\kanourban LGAs.csv"))
# 
# r_map_df <- left_join(cnames, kadf, by =c("id"))
# 
# kshp <- read_sf("C:\\Users\\USER\\Box\\NU-malaria-team\\data\\nigeria\\kano_ibadan_shape_files\\kano_metro_wards_shapes\\kano_metro_wards.shp")
# 
# r_map_df <- st_as_sf(r_map_df)
# 
# r_map <- tm_shape(kshp)+
#   tm_polygons(col = "mean", midpoint =NA, palette = "NULL")+
#   tm_text("id") +
#   tm_shape(kshp, col = "mean", midpoint =NA, palette = "NULL") +
#   tm_symbols(col = "azure4", scale = .3)
# 
# r_map
# r_map_df
# # tm_shape(r_map_df) + tm_polygons("mean")
# 
# ib_lwm=ggplot()+
#   geom_sf(data=ib_w, color=alpha('#8971B3', 0.5)) +
#   geom_sf(data=ib_l, fill = 'transparent', size =0.4, color=alpha('salmon', 0.5))+
#   geom_text_repel(
#     data = ib_l,
#     aes(label = LGAName, geometry = geometry),color ='black',
#     stat = "sf_coordinates", 
#     min.segment.length = 0, size = 4, force = 1)+ #geom_sf_text(aes(label=name_long))+ 
#   map_theme() +
#   xlab('')+
#   ylab('')