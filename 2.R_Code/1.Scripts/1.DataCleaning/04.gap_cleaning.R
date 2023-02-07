#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-07 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(tidyverse)
library(mapview)
library(ggpubr)
library(foreach)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####
#        Shapefiles                                                         ####

# Montana Country Polygons
county_boundaries <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp")

# Montana Protected Areas
protected_areas <- st_read("1.Data/data_clean/Montana_ProtectedAreas/Montana_ProtectedAreas.shp") %>% 
  st_make_valid()

# Roads
montana_roads <- read_sf("1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp") 

# Montana Cadastral Data
montana_cadastral <- read_sf("1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership_exception.shp") 

#        List of counties                                                   ####

# Making a list of county names 
county_list <- unique(county_boundaries$NAME)

# Checking if the list of county names are the same across layers
county_list_cadastral <- unique(montana_cadastral$CountyName)

county_list %in% county_list_cadastral

# Fixing The one county that is a problem. Lewis and Clark is the problem
# because one list has the word and and the other has an ampersand (&).

county_list <- str_replace(county_list,
                           pattern = "Lewis & Clark",
                           replacement = "Lewis and Clark")

###############################################################################
#   Gap Cleaning Protocol                                                   ####

#      Protocol Settings                                                    ####

# Error Buffer (in meters)
buffer_dist <- 20

# Working Projection EPSG code 
working_proj <- 5070

#      Cleaning Protocol                                                    ####

i <- 1
# Start of the For loop 

###############################################################################
#   County-specific sub-setting                                             ####

# County Name
county_name <- county_list[[i]]

# County 
county_boundary <- county_boundaries %>% 
  filter(NAME == county_name) 

# Protected Areas
county_protectedareas <- protected_areas %>% 
  st_make_valid() %>% 
  st_intersection(county_boundary) 

# Roads
county_roads <- montana_roads %>% 
  st_intersection(county_boundary) %>% 
  sample_n(100)

# Cadastral 
county_cadastral <- montana_cadastral %>% 
  filter(CountyName == county_name) %>% 
  sample_n(100)

#   GAP Cleaning: Overlapping Statuses                                      ####
#      Filtering by Status                                                  ####

GAP1 <- county_protectedareas %>%  filter(GAP_Sts == "1") 
GAP2 <- county_protectedareas %>%  filter(GAP_Sts == "2")
GAP3 <- county_protectedareas %>%  filter(GAP_Sts == "3")
GAP4 <- county_protectedareas %>%  filter(GAP_Sts == "4")

#      Eliminating Between Status Overlap                                   ####

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

#      Packaging for Further Cleaning                                       ####

GAP_layer_base <- bind_rows(GAP1,GAP2_clean,GAP3_clean,GAP4_clean) 

#   GAP Cleaning: Roads                                                     ####
#      Preparing Base Gap Layer                                             ####

layer_gap_cleanroads <- GAP_layer_base %>% st_transform(5070)
county_roads <- county_roads %>% st_transform(5070)

#      Eliminating Roads                                                    ####

# Eliminating Roads from the GAP data
sf_use_s2(F)

log_roads <- foreach(i = 1:nrow(county_roads),
                     .combine=rbind,
                     .errorhandling = "pass") %do% {
                       
                       road_sub <- county_roads[i,] %>% 
                         st_buffer(dist = buffer_dist) %>% 
                         st_combine() %>% 
                         st_make_valid()
                       
                       layer_gap_cleanroads <- st_difference(x = layer_gap_cleanroads,
                                                  y = road_sub)
                       
                       return(data.frame(iteration = i,
                                         time = Sys.time()))
                     }

# Exporting Foreach Log
write_csv(log_roads, 
          paste0("3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_",county_name,".csv"))

# Exporting Layer Temp 
sf_use_s2(T)

layer_gap_cleanroads <- st_collection_extract(layer_gap_cleanroads, "POLYGON")

#   GAP Cleaning: Cadastral                                                 ####
#      Preparing Base Gap Layer that has been cleaned                       ####

layer_gap_cleanroads_cleancadastral <- layer_gap_cleanroads
cadastral_exemption_shp <- county_cadastral %>% st_transform(5070)

#      Cadastral Relevancy                                                  ####
###############################################################################
