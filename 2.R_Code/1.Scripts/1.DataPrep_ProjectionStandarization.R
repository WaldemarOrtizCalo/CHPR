#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-07-30 

# Purpose: This script is meant to import all of the raw data and transform it
# to a standarized format which is WGS 84 long-lat. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(mapview)
library(tidyverse)

#      Functions                                                            ####

#      Data                                                                 ####

# Road Layer
roads <- read_sf("1.Data\\data_raw\\Roads\\rds2000.shp")
publiclands <- read_sf("1.Data/data_raw/MontanaCadestral/Montana_Cadastral/OWNERPARCEL.shp")
counties <- read_sf("1.Data\\data_raw\\MontanaCounties\\county.shp")

###############################################################################
#   [Road Data]                                                             ####
#      [Data Import]                                                        ####

# Data Import
roads <- read_sf("1.Data\\data_raw\\Roads\\rds2000.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(roads)

# Changing Projection
roads <- st_transform(roads, crs = 4326)

#      [Data Export]                                                        ####
st_write(obj = roads,
         dsn = "1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp")

#      [Cleaning Environment]                                               ####

rm(list = ls())
###############################################################################

p <- publiclands[1:10,] %>% st_transform(crs = 4326)
c <- counties[1,] %>% st_transform(crs = st_crs(p))

mapview(p)

t <- st_intersection(p,c)


m <- subset(publiclands, CountyName == "Missoula")


mapview(m)
