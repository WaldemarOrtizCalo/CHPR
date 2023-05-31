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

parks <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2parks_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

trails <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2trails_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

playgrounds <- rast("1.Data\\data_clean\\Parkserve\\mt_dist2playgrounds_clippedraster.tif") %>% 
  crop(missoula_boundary) %>% 
  mask(missoula_boundary)

rec_birdwatch_fishing <- st_read("1.Data\\data_clean\\Recreation_bird_fishing\\layer_bw_ff.shp")%>% 
  st_intersection(missoula_boundary)

###############################################################################
#   Missoula Boundary                                                       ####

base_map <- mapview(missoula_boundary)

mapshot(base_map,
        file = "3.Outputs/proposal_maps/missoula.png")

#   GAP_status                                                              ####
map <- ggplot(gap)+
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
  geom_sf(aes(fill = vst_ttl))+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/nearvista_totalvisits.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   NEAR VISTA - Avg Visits per visitor                                     ####

map <- ggplot(gap)+
  geom_sf(aes(fill = avg_vst))+
  theme_void()+
  theme(legend.position = "none") 

ggsave(filename = "3.Outputs/proposal_maps/nearvista_avgvisits.png",
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
  theme(legend.position = "none") 

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
  theme(legend.position = "none") 

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
  scale_fill_discrete(na.value = NA)

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
  scale_fill_continuous(na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/ndvi.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")

#   Parkserve - Trails                                                      ####

map <- ggplot()+
  geom_spatraster(data = trails,na.rm = T)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_continuous(na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2trails.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")


#   Parkserve - Parks                                                       ####

map <- ggplot()+
  geom_spatraster(data = parks,na.rm = T)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_continuous(na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2parks.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")


#   Parkserve - Playground                                                  ####

map <- ggplot()+
  geom_spatraster(data = playgrounds,na.rm = T)+
  theme_void()+
  theme(legend.position = "none") +
  scale_fill_continuous(na.value = NA)

ggsave(filename = "3.Outputs/proposal_maps/dist2playgrounds.png",
       plot = map,
       device = "png",
       dpi = 1200,
       width = 12,
       height = 6, 
       units = "in")



###############################################################################