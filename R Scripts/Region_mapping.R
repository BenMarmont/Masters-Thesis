
# Packages ----

library(tidyverse)
library(sf)
#library(sp)
library(ggrepel)
#library(maps)
library(here)
#library(raster)
#library(ggthemes)

# You can import shape files with st_read...

#key verbs: 
  # union: 
  # intersect: removes overlap of layers

# Creating ShapeFiles ----

#  https://mhallwor.github.io/_pages/Tidyverse_intro
#  https://gis.stackexchange.com/questions/372728/efficient-clipping-of-polygons-by-other-polygons-using-r


## Region Outlines (contains some ocean)
#nz_regions <- raster::shapefile("C:/Users/MarmontB/OneDrive - DairyNZ Limited/Documents/R/Thesis_new/Economics/linz_download/nz-land-districts.shp")
nz_regions <- st_read("C:/Users/MarmontB/OneDrive - DairyNZ Limited/Documents/R/Thesis_new/Economics/linz_download/nz-land-districts.shp")
class(nz_regions)
nz_regions_sf <- st_as_sf(nz_regions)

#nz_regions_sf <- (st_transform(nz_regions_sf, crs = 4326))
#class(nz_regions_sf)

ggplot() +
geom_sf(data = nz_regions_sf)

## Coastlines (to get around massive shapes)
nz_outline <- st_read("C:/Users/MarmontB/OneDrive - DairyNZ Limited/Documents/R/Thesis_new/Economics/linz_outline/nz-coastlines-and-islands-polygons-topo-150k.shp")
#nz_outline <- raster::shapefile("C:/Users/MarmontB/OneDrive - DairyNZ Limited/Documents/R/Thesis_new/Economics/linz_outline/nz-coastlines-and-islands-polygons-topo-150k.shp")
#class(nz_outline)
nz_outline_sf <- st_as_sf(nz_outline)

#nz_outline_sf<- (st_transform(nz_outline_sf, crs = 4326))
#class(nz_regions_sf)

## Plotting the two layers to check overlap
ggplot() +  
  geom_sf(data = nz_regions_sf, alpha = .25, fill = "red") +
  geom_sf(data = nz_outline_sf) 
 
#cropped <- st_crop()

trimmed <- st_intersection(nz_outline_sf, nz_regions_sf)

# nz_regions_sf <- sf_geometry(nz_regions_sf) %>% 
#                 mutate(nz_regions_sf,
#                         x = purrr::map_dbl(geometry, 1),
#                         y = purrr::map_dbl(geometry, 2))

# ggplot() +
#   geom_sf(data = trimmed) +
#   coord_sf(xlim = c(160,180)) +
#   geom_text(data = name)
# 
# ggplot(data = trimmed) +
#   geom_sf() +
#   coord_sf(xlim = c(160,180)) +
#   geom_sf_label(label = trimmed$name)

region_centroids <- nz_regions_sf %>% 
  st_centroid() %>% 
  mutate(x = purrr::map_dbl(geometry, 1),
         y = purrr::map_dbl(geometry, 2))

ggplot() +
  geom_sf(data = trimmed) +
  coord_sf(xlim = c(160,180)) +
  geom_label_repel(data = region_centroids, aes(x = x, y = y,label = name))

#Joining regions (union)

#North Island Regions
# 
# trimmed <- trimmed %>% 
#   mutate(Northland = (filter(id == 1001)))
# 
# mutate(trimmed, Northland = 'North Auckland')

# "Northland" <- trimmed %>% 
#   filter(id == 1001) %>% 
#   st_union()%>% 
#   mutate(x = purrr::map_dbl(geometry, 1),
#          y = purrr::map_dbl(geometry, 2)) %>% 
#   st_centroid()
# 
# 


"Northland" <- trimmed %>% 
  filter(id == 1001) %>% 
  st_union() 
# 
# Northland_centroid <- (st_centroid(Northland))
# 
# 
# Centroid_coord <- st_coordinates(Northland_centroid) %>% 
#   as_tibble() %>%
#   add_column(Region = "Northland", .before = "X") 
# 
# Centroid_coord <- tibble(x = mutate(st_coordinates(Northland_centroid))$x) %>% 
#   as_tibble() %>%
#   add_column(Region = "Northland", .before = "X") 


# this is all the labelling needed, then call it like in the following plot
northland_label <- st_coordinates(st_centroid(Northland)) %>% 
  as_tibble() %>%
  add_column(Region = "Northland", .before = "X") 
  
  

Northland %>% 
  ggplot() +
  geom_sf() +
  geom_label_repel(data = northland_label, x = northland_label$X, y = northland_label$Y,label = "Northland")
# geom_text_repel(data = Northland, label = name)
#geom_label_repel(data = region_centroids, aes(x = x, y = y,label = name))



# ggplot()+
#   geom_sf(nz_regions_sf) +
#   geom_sf(data = Northland_centroid, colour = "red")
#   
# 
# 



# Northland %>% st_transform(crs = 4326, geom = sf)
# 
# Northland %>% st_geometry()
# 
# Northland %>% 
#   ggplot() +
#   geom_sf_()
# nz_outline_sf <- st_as_sf(nz_outline)
# 
# northland_centroid <- st_as_sf(Northland) %>% 
#   st_centroid() %>% 
#   mutate(x = purrr::map_dbl(geometry, 1),
#          y = purrr::map_dbl(geometry, 2))
# 
# Northland_centroid <- st_as_af(Northland) %>% 
#   st_centroid() %>% 
#   mutate(x = purrr::map_dbl(geometry, 1),
#          y = purrr::map_dbl(geometry, 2))
# 
# Northland_label <- Northland %>% 
#   st_centroid() %>% 
#   mutate(x = purrr::map_dbl(geometry, 1),
#          y = purrr::map_dbl(geometry, 2))

"Waikato"<- trimmed %>% 
  filter(id == 1002) %>% 
  st_union()

waikato_label <- st_coordinates(st_centroid(Waikato)) %>% 
  as_tibble() %>%
  add_column(Region = "Waikato", .before = "X") 

"Bay_of_Plenty" <- trimmed %>% 
  filter(id == 1004) %>% 
  st_union()

Bay_of_plenty_label <- st_coordinates(st_centroid(Bay_of_Plenty)) %>% 
  as_tibble() %>%
  add_column(Region = "Bay of Plenty", .before = "X") 

"Taranaki" <- trimmed %>% 
  filter(id == 1005) %>% 
  st_union()

taranaki_label <- st_coordinates(st_centroid(Taranaki)) %>% 
  as_tibble() %>%
  add_column(Region = "Taranaki", .before = "X") 


"Lower_North_Island" <- trimmed %>% 
  filter(id == 1003 | id == 1006) %>% 
  st_union()

LNI_label <- st_coordinates(st_centroid(Lower_North_Island)) %>% 
  as_tibble() %>%
  add_column(Region = "LNI", .before = "X") 

# "North Island Regions" <- 
#   filter("Northland", "Waikato", "Bay of Plenty", "Taranaki", "Lower North Island")) %>% 
#   st_union()

# South Island Regions

"Otago_Southland" <- trimmed %>% 
  filter(id == 1011 | id == 1012) %>% 
  st_union()

otago_southland_label <- st_coordinates(st_centroid(Otago_Southland)) %>% 
  as_tibble() %>%
  add_column(Region = "Otago_Southland", .before = "X") 


"West_Coast_Tasman"<- trimmed %>% 
  filter(id == 1009 | id == 1007) %>% 
  st_union() 
  
wc_tasman_label <- st_coordinates(st_centroid(West_Coast_Tasman)) %>% 
  as_tibble() %>%
  add_column(Region = "West_coast_tasman", .before = "X") 

"Marlborough_Canterbury"<- trimmed %>% 
  filter(id == 1008 | id == 1010) %>% 
  st_union()

marlborough_canterbury_label <- st_coordinates(st_centroid(Marlborough_Canterbury)) %>% 
  as_tibble() %>%
  add_column(Region = "Marlborough_Canterbury", .before = "X") 

# Placing region geoms
nz_outline_sf %>% 
  ggplot() +
  geom_sf(data = `Northland`) +
  geom_sf(data = `Waikato`) +
  geom_sf(data = `Bay_of_Plenty`) +
  geom_sf(data = `Taranaki`) +  
  geom_sf(data = `Lower_North_Island`) +
  geom_sf(data = `West_Coast_Tasman`) +
  geom_sf(data = `Marlborough_Canterbury`) +
  geom_sf(data = `Otago_Southland`) +
  coord_sf(xlim = c(160,180)) 
                   
# centroid_tmp <- c(Northland, Waikato, `Bay of Plenty`, `Lower North Island`,
#   `West Coast-Tasman`,`Marlborough-Canterbury`,`Otago-Southland`) %>% st_centroid() %>% 
#   mutate(x = purrr::map_dbl(geometry, 1),
#          y = purrr::map_dbl(geometry, 2))
  #labels over geoms, note the right labels though

# This is not the graph you are looking for (jedi mind tricks)
        # it is
  ggplot() +
  geom_sf(data = `Northland`) +
  geom_sf(data = `Waikato`) +
  geom_sf(data = `Bay_of_Plenty`) +
  geom_sf(data = `Taranaki`) +  
  geom_sf(data = `Lower_North_Island`) +
  geom_sf(data = `West_Coast_Tasman`) +
  geom_sf(data = `Marlborough_Canterbury`) +
  geom_sf(data = `Otago_Southland`) +
  coord_sf(xlim = c(160,180)) +
  geom_label_repel(data = northland_label, x = northland_label$X, y = northland_label$Y,label = "Northland") +
  geom_label_repel(data = waikato_label, x = waikato_label$X, y = waikato_label$Y,label = "Waikato") +
  geom_label_repel(data = Bay_of_plenty_label, x = Bay_of_plenty_label$X, y = Bay_of_plenty_label$Y,label = "Bay of Plenty") +
  geom_label_repel(data = taranaki_label, x = taranaki_label$X, y = taranaki_label$Y,label = "Taranaki") +
  geom_label_repel(data = LNI_label, x = LNI_label$X, y = LNI_label$Y,label = "Lower North Island") +
  geom_label_repel(data = wc_tasman_label, x = wc_tasman_label$X, y = wc_tasman_label$Y,label = "West Coast-Tasman") +
  geom_label_repel(data = marlborough_canterbury_label, x = marlborough_canterbury_label$X, y = marlborough_canterbury_label$Y,label = "Marlborough-Canterbury") + 
  geom_label_repel(data = otago_southland_label, x = otago_southland_label$X, y = otago_southland_label$Y,label = "Otago-Southland") +
  labs(title = "Dairy Farming Regions of New Zealand",
       caption = "Data Source: Land Information New Zealand (LINZ)") +
  theme_bw()
  
ggsave("dairy_region_map.jpeg")
 
# If the map fails making the marlborough_canterbury_label, uncomment and run the following line
#sf_use_s2(FALSE)