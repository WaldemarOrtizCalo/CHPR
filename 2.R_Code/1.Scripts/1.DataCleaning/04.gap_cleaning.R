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
montana_cadastral <- read_sf("1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership.shp") 

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

