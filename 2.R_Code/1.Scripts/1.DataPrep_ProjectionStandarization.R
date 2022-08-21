#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-07-30 

# Purpose: This script is meant to import all of the raw data and transform it
# to a standardized projection system which is WGS 84 long-lat. 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(mapview)
library(tidyverse)

#      Functions                                                            ####

#      Standardization Values                                               ####

targetProj <- 4326 # Target projection. WGS 84 

###############################################################################
#   [Road Data]                                                             ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\Roads\\rds2000.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Data Export]                                                        ####

# Export Script
st_write(obj = layer,
         dsn = "1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp",
         append = FALSE)

#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 

# Cleaning RAM 
gc()

###############################################################################
#   [Montana County Boundaries]                                             ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\MontanaCounties\\county.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Data Export]                                                        ####

# Export Script
st_write(obj = layer,
         dsn = "1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp",
         append = FALSE)

#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 

# Cleaning RAM 
gc()

###############################################################################
#   [Montana Protected Areas]                                               ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\MontanaCounties\\county.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Data Export]                                                        ####

# Export Script
st_write(obj = layer,
         dsn = "1.Data\\data_clean\\Montana_ProtectedAreas\\Montana_ProtectedAreas.shp",
         append = FALSE)

#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 

# Cleaning RAM 
gc()

###############################################################################
################## NEEDS DEVELOPMENT/SUBSETTING ###############################
###############################################################################
#   [Montana Cadastral Data - Needs Subsetting portion]                     ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\MontanaCadastral\\Montana_Cadastral\\OWNERPARCEL.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Data Export]                                                        ####

# Export Script
st_write(obj = layer,
         dsn = "1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership.shp",
         append = FALSE)

#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 

# Cleaning RAM 
gc()

###############################################################################