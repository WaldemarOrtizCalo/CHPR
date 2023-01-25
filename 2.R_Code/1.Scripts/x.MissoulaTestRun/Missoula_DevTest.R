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
library(foreach)

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
#      Making Maps of Protected Areas                                       ####

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

#      Eliminating Overlapping Polygons                                     ####
#        [Filtering by Status]                                              ####

GAP1 <- protected_areas %>%  filter(GAP_Sts == "1") 
GAP2 <- protected_areas %>%  filter(GAP_Sts == "2")
GAP3 <- protected_areas %>%  filter(GAP_Sts == "3")
GAP4 <- protected_areas %>%  filter(GAP_Sts == "4")

#        [Eliminating WITHIN Statuses]                                      ####

overlaps1 <- st_intersection(GAP1) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps2 <- st_intersection(GAP2) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps3 <- st_intersection(GAP3) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps4 <- st_intersection(GAP4) %>% mutate(n.overlaps = as.character(n.overlaps))

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

#        [Eliminating BETWEEN Statuses]                                     ####

GAP1_combined <- GAP1 %>% st_combine() %>% st_make_valid()
GAP2_combined <- GAP2 %>% st_combine() %>% st_make_valid()
GAP3_combined <- GAP3 %>% st_combine() %>% st_make_valid()
GAP4_combined <- GAP4 %>% st_combine() %>% st_make_valid()

sf_use_s2(F)
GAP2_clean <- st_difference(GAP2,GAP1_combined)
GAP3_clean <- st_difference(GAP3,GAP2_combined)%>% 
  st_difference(GAP1_combined)
GAP4_clean <- st_difference(GAP4,GAP3_combined) %>% 
  st_difference(GAP2_combined) %>% 
  st_difference(GAP1_combined)
sf_use_s2(T)

mapview(GAP1, col.region = "red")+ 
  mapview(GAP2, col.region = "blue")+ 
  mapview(GAP3, col.region = "yellow")+ 
  mapview(GAP4, col.region = "green")+ 
  mapview(GAP2_clean, col.region = "orange") +
  mapview(GAP3_clean, col.region = "pink") +
  mapview(GAP4_clean, col.region = "#EE30A7")

GAP_layer_final <- bind_rows(GAP1,GAP2_clean,GAP3_clean,GAP4_clean)

mapview(GAP_layer_final,zcol = "GAP_Sts")
#      Exporting Final Layer                                                ####

export_dir <- "D:/Drive/Research/CPHR/CPHR_Workspace/1.Data/data_clean/Montana_ProtectedAreas/county_based"
county_name <- "Missoula"

st_write(GAP_layer_final,
         paste0(export_dir,"/",county_name,"_GAP_areas.shp"),
         append = F)

t <- st_read(paste0(export_dir,"/",county_name,"_GAP_areas.shp"))

mapview(t) + mapview(GAP_layer_final)
###############################################################################
#   GAP Status: Road Cleaning [DEV]                                         ####


missoula_gap_fp <- "1.Data/data_clean/Montana_ProtectedAreas/county_based/Missoula_GAP_areas.shp"
roads_combined_fp <- "D:/Drive/Research/CPHR/CPHR_Workspace/1.Data/test_folder/roads_combined.shp"
roads_buffered_fp <- "D:/Drive/Research/CPHR/CPHR_Workspace/1.Data/test_folder/roads_buffered.shp"


# Top 5 Roads
roads <- missoula_roads %>% st_transform(5070)

gap_sf <- st_read(missoula_gap_fp) %>% st_transform(5070)

layer <- gap_sf

sf_use_s2(F)

print(paste0("Start Time: ",Sys.time()))

log <- foreach(i = 1:nrow(roads),
               .combine=rbind,
               .errorhandling = "pass") %do% {
                 
                 road_sub <- roads[i,] %>% 
                   st_buffer(dist = 50) %>% 
                   st_combine() %>% 
                   st_make_valid()
                 
                 layer <- st_difference(x = layer,
                                        y = road_sub)
                 
                 return(data.frame(iteration = i,
                                   time = Sys.time()))
               }

print(paste0("End Time: ",Sys.time()))

sub_samp <- roads %>% sample_n(1000)





mapview(layer) + mapview(sub_samp, color = "red")


mapview(missoula_cadastral)+mapview(layer)

mapview(layer,)



###############################################################################
#  Exporting Maps                                                           ####
###############################################################################