#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-01 

# Purpose: This script is designed as a test run of the cell phone data subset
# 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(tidyverse)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# Montana Country Polygons
missoula_boundary <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp") %>% 
  filter(NAME == "Missoula")

# Montana Protected Areas
protected_areas <- st_read("1.Data/data_clean/Montana_ProtectedAreas/Montana_ProtectedAreas.shp") %>% 
  st_make_valid() %>% 
  st_intersection(missoula_boundary)

# Roads
missoula_roads <- read_sf("1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp") %>% 
  st_intersection(missoula_boundary)

# Montana Cadastral Data
missoula_cadastral <- read_sf("1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership.shp") %>% 
  filter(CountyName == "Missoula")

###############################################################################
#  Road Buffer Layer                                                        ####

# Creating a 50m buffer 
roads_buffered <- missoula_roads %>% st_buffer(50)

# Exporting
st_write(obj = roads_buffered,
         dsn = "1.Data/data_clean/RoadLayer/road_buffers.shp",
         append = F)

###############################################################################
#   [Exporting Maps]                                                        ####

# Road and Road Buffer
map_road <- ggplot() +
  geom_sf(data = missoula_boundary,color = "black",size = 1) +
  geom_sf(data = missoula_roads)+
  theme_bw()


ggsave(filename = "3.Outputs/MissoulaDevTest/maps/missoula_roads.png",
       plot = map_road,
       device = "png",
       width = 8,
       height = 6, 
       units = "in")

map_bufferedroads <- ggplot() +
  geom_sf(data = missoula_boundary,color = "black",size = 1) +
  geom_sf(data = roads_buffered)+
  theme_bw()


ggsave(filename = "3.Outputs/MissoulaDevTest/maps/missoula_bufferedroads.png",
       plot = map_bufferedroads,
       device = "png",
       width = 8,
       height = 6, 
       units = "in")

###############################################################################
