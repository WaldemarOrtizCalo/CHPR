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
library(ggpubr)

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
#  Road Buffer Layer Creation and Export                                    ####

#      Creating Buffered Layer                                              ####

# Creating a 50m buffer 
roads_buffered <- missoula_roads %>% st_buffer(50)

# Exporting
st_write(obj = roads_buffered,
         dsn = "1.Data/data_clean/RoadLayer/road_buffers.shp",
         append = F)

#      Creating and Exporting Maps                                          ####

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
#   Protected Area Cleaning                                                 ####
#       Making Maps of Protected Areas                                      ####

map_GAPstatus <- ggplot(protected_areas)+
  geom_sf(data = missoula_boundary) +
  geom_sf(fill = "#458B00") +
  facet_wrap(~ GAP_Sts)+
  theme_bw()+
  ggtitle("Protected Areas by GAP status")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

ggsave(filename = "3.Outputs/MissoulaDevTest/maps/protected_areas.png",
       plot = map_GAPstatus,
       device = "png",
       width = 12,
       height = 6, 
       units = "in")

#       Eliminating Overlapping Statuses                                    ####

base1 <- protected_areas %>%  filter(GAP_Sts == "1") 
base2 <- protected_areas %>%  filter(GAP_Sts == "2")
base3 <- protected_areas %>%  filter(GAP_Sts == "3")
base4 <- protected_areas %>%  filter(GAP_Sts == "4")

overlaps1 <- st_intersection(base1) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps2 <- st_intersection(base2) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps3 <- st_intersection(base3) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps4 <- st_intersection(base4) %>% mutate(n.overlaps = as.character(n.overlaps))

p1 <- ggplot(overlaps1)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 1")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p2 <- ggplot(overlaps2)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 2")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p3 <- ggplot(overlaps3)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 3")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p4 <- ggplot(overlaps4)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 4")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

ggarrange(p1,p2,p3,p4,
          ncol = 2,nrow = 2)

###############################################################################
#  Exporting Maps                                                           ####





###############################################################################