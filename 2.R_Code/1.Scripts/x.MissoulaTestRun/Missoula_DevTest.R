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
#   [Making leaflet maps]                                                   ####
#      [Cadastral Map]                                                      ####

cad_map <- mapview(missoula_cadastral,layer.name = "Missoula Cadastral", 
                   color = "#E06E72",
                   col.regions = "#E06E72") +
  mapview(missoula_boundary,layer.name = "Missoula", 
          color = "royalblue2",
          col.regions = "royalblue2",
          alpha.regions = 0.1, aplha = 1) 

cad_map %>% 
  mapshot(url = "D:/Drive/Research/CPHR/CPHR_Workspace/2.R_Code/4.LeafletMaps/CadastralMap.html")

