#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-05-30 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(mapview)
library(tidyverse)
library(tidyterra)

#      Functions                                                            ####

#      Data                                                                 ####

sf_use_s2(FALSE)

missoula_boundary <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp") %>% 
  filter(NAME == "Missoula")

nlcd <- rast("1.Data\\data_clean\\NLCD\\nlcd_2019.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

ndvi <- rast("1.Data\\data_clean\\NDVI_summaries\\summaries_yearly\\ndvi_avg_yr_2019_250m.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

gap <- st_read("1.Data\\data_clean\\gap_clean_nearvista\\gap_nearvista.shp") %>% 
  st_intersection(missoula_boundary)

parks_shp <- st_read("1.Data\\data_clean\\Parkserve\\mt_parks.shp") %>% 
  st_intersection(missoula_boundary)

trails_shp <- st_read("1.Data\\data_clean\\Parkserve\\mt_trails.shp") %>% 
  st_intersection(missoula_boundary)

playgrounds_shp <- st_read("1.Data\\data_clean\\Parkserve\\mt_playgrounds.shp") %>% 
  st_intersection(missoula_boundary)

parks_rast <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2parks_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

trails_rast <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2trails_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

playgrounds_rast <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2playgrounds_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

rec_birdwatch_fishing <- st_read("1.Data\\data_clean\\Recreation_bird_fishing\\layer_bw_ff.shp")%>% 
  st_intersection(missoula_boundary)

LCcodes <- unique(nlcd)

color_codes <- c('#5475a8'
                 ,'#ffffff','#e8d1d1'
                 ,'#e29e8c','#ff0000'
                 ,'#b50000','#d2cdc0'
                 ,'#85c77e','#38814e'
                 ,'#d4e7b0','#dcca8f'
                 ,'#e2e2c1','#fbf65d'
                 ,'#ca9146','#c8e6f8'
                 ,'#64b3d5')

color_codes_df <- data.frame(
  land_cover = unique(nlcd)[,1],
  hex_codes = color_codes
)

###############################################################################
#   Visualizations without Legends                                          ####
#   Missoula Boundary                                                       ####

map <- ggplot(missoula_boundary)+
  geom_sf(fill = "darkorchid",alpha = 0.6)+
  theme_void()

ggsave(filename = "3.Outputs/proposal_maps/missoula_boundary.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   GAP_status                                                              ####
map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = GAP_Sts))+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/gap_status.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Total Visits                                               ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = vst_ttl))+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/nearvista_totalvisits.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Avg Visits per visitor                                     ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = avg_vst))+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/nearvista_avgvisits.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Median Distance Traveled                                   ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = mdn_dst))+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/nearvista_mediandist.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   RecDays - Birdwatching                                                  ####

map <- ggplot(rec_birdwatch_fishing)+
  geom_sf(aes(fill = bw_recdays))+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/bw_recdays.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   RecDays - Fishing                                                       ####

map <- ggplot(rec_birdwatch_fishing)+
  geom_sf(aes(fill = ff_recdays))+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/ff_recdays.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NLCD                                                                    ####

map <- ggplot()+
  geom_spatraster(data = nlcd,na.rm = T)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_discrete(na.value = NA)+
  scale_fill_manual(name = "Land cover",
                    values = color_codes_df$hex_codes,
                    labels = color_codes_df$land_cover,
                    na.translate = FALSE) 

ggsave(filename = "3.Outputs/proposal_maps/nlcd.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NDVI                                                                    ####

map <- ggplot()+
  geom_spatraster(data = ndvi,na.rm = T)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradientn(colours = terrain.colors(7),
                       na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/ndvi.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Trails                                                      ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = trails_shp)+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/trails.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = trails_rast,na.rm = T)+
  geom_sf(data = trails_shp)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2trails.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Parks                                                       ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = parks_shp,aes(fill = Park_Desig))+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/parks.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = parks_rast,na.rm = T)+
  geom_sf(data = parks_shp)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2parks.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Playground                                                  ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = playgrounds_shp)+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/playgrounds.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = playgrounds_rast,na.rm = T)+
  geom_sf(data = playgrounds_shp)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2playgrounds.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

###############################################################################
#   Visualizations with Legends                                             ####
#   GAP_status                                                              ####
map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = GAP_Sts))+
  theme_void()+
  scale_fill_discrete(name = "Gap Status")

ggsave(filename = "3.Outputs/proposal_maps/legend_gap_status.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Total Visits                                               ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = vst_ttl))+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA,
                      name = "Total Visits", labels = comma)

ggsave(filename = "3.Outputs/proposal_maps/legend_nearvista_totalvisits.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Avg Visits per visitor                                     ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = avg_vst))+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      na.value = NA,
                      name = "Avg Number of Visits", 
                      labels = comma)

ggsave(filename = "3.Outputs/proposal_maps/legend_nearvista_avgvisits.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Median Distance Traveled                                   ####

map <- ggplot(gap)+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(aes(fill = mdn_dst))+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      na.value = NA,
                      name = "Median Distance Traveled", 
                      labels = comma)

ggsave(filename = "3.Outputs/proposal_maps/legend_nearvista_mediandist.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   RecDays - Birdwatching                                                  ####

map <- ggplot(rec_birdwatch_fishing)+
  geom_sf(aes(fill = bw_recdays))+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                    na.value = NA,
                    name = "Rec Days: Birdwatching", 
                    labels = comma)

ggsave(filename = "3.Outputs/proposal_maps/legend_bw_recdays.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   RecDays - Fishing                                                       ####

map <- ggplot(rec_birdwatch_fishing)+
  geom_sf(aes(fill = ff_recdays))+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      na.value = NA,
                      name = "Rec Days: Fishing", 
                      labels = comma)

ggsave(filename = "3.Outputs/proposal_maps/legend_ff_recdays.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NLCD                                                                    ####

map <- ggplot()+
  geom_spatraster(data = nlcd,na.rm = T)+
  theme_void()+
  scale_fill_discrete(na.value = NA)+
  scale_fill_manual(name = "Land cover",
                    values = color_codes_df$hex_codes,
                    labels = color_codes_df$land_cover,
                    na.translate = FALSE) 

ggsave(filename = "3.Outputs/proposal_maps/legend_nlcd.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NDVI                                                                    ####

map <- ggplot()+
  geom_spatraster(data = ndvi,na.rm = T)+
  theme_void()+
  scale_fill_gradientn(colours = terrain.colors(7,rev = T),
                       na.value = NA,
                       name = "NDVI")

ggsave(filename = "3.Outputs/proposal_maps/legend_ndvi.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Trails                                                      ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = trails_shp)+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/legend_trails.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = trails_rast,na.rm = T)+
  geom_sf(data = trails_shp)+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Distance to Trail (m)",
                      na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/legend_dist2trails.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Parks                                                       ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = parks_shp,aes(fill = Park_Desig))+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/legend_parks.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = parks_rast,na.rm = T)+
  geom_sf(data = parks_shp)+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Distance to Parks (m)",
                      na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/legend_dist2parks.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Playground                                                  ####

map <- ggplot()+
  geom_sf(data = missoula_boundary,fill = "gray50",alpha = 0.6)+
  geom_sf(data = playgrounds_shp)+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/legend_playgrounds.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

map <- ggplot()+
  geom_spatraster(data = playgrounds_rast,na.rm = T)+
  geom_sf(data = playgrounds_shp)+
  theme_void()+
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Distance to Playgrounds (m)",
                      na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/legend_dist2playgrounds.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

###############################################################################
