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
#      [Data Subsetting]                                                    ####

layer <- layer %>% filter(ROADCLASS %in% c("Local road or city street",
                                           "Driveway or service road",
                                           "State and secondary highway",
                                           "US highway without limited access",
                                           "Driveway or service roadl"))


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
layer <- read_sf("1.Data\\data_raw\\PAD_MT_ProtectedAreas\\PADUS3_0Combined_StateMT.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Data Subsetting]                                                    ####

layer <- layer %>% filter(d_Des_Tp %in% c("Research or Educational Area",
                                          "National Public Lands",
                                          "Recreation Management Area",
                                          "National Forest",
                                          "National Grassland",
                                          "Historic or Cultural Area",
                                          "National Monument",
                                          "National Park",
                                          "National Recreation Area",
                                          "National Wildlife Refuge",
                                          "State Park",
                                          "Local Conservation Area",
                                          "State Conservation Area",
                                          "State Recreation Area", 
                                          "State Resource Management Area",
                                          "State Wilderness",
                                          "Conservation Area",
                                          "National Scenic or Historic Trail",
                                          "National Scenic, Botanical or Volcanic Area",
                                          "Wilderness Area",
                                          "Wild and Scenic River",
                                          "Local Park",
                                          "Local Recreation Area",
                                          "Inventoried Roadless Area",
                                          "Wilderness Study Area"
                                          ))

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
#   [Montana Cadastral Data - Inclusion]                                    ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\MontanaCadastral\\Montana_Cadastral\\OWNERPARCEL.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Subsetting Important Parcel Types]                                  ####

# Listing Unique Parcel Types
unique(layer$PropType)

# Subsetting Parcels of Interest
layer <- layer %>% filter(PropType %in% c("EP - Exempt Property",
                                 "VAC_R - Vacant Land - Rural",
                                 "VAC_U - Vacant Land - Urban",
                                 "GOLF - Golf Course",
                                 "MINE - Mining Claim",
                                 "TP - Tribal Property",
                                 "MC - Mining Claim",
                                 "VU - Vacant Land Urban"))

#      [Data Export]                                                        ####

# Creating a list of unique Counties
CountyNames <- unique(layer$CountyName)

#        [All Data Export]                                                  ####

st_write(obj = layer,
         dsn = "1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership.shp",
         append = FALSE)

#        [County export]                                                    ####

# For Loop to do a per 

for (i in 1:length(CountyNames)) {
  
  # Export Script
  st_write(obj = subset(layer, layer$CountyName == CountyNames[i]),
           dsn = paste0("1.Data\\data_clean\\MontanaCadastral\\CountyBasedSubset\\MontanaCadastral_",
                        CountyNames[i],
                        ".shp"),
           append = FALSE)
  
  # Iterator Tracker
  print(i)
}


#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 
rm(i) 
rm(CountyNames)

# Cleaning RAM 
gc()

#   [Montana Cadastral Data - Exclusion]                                    ####
#      [Data Import]                                                        ####

# Data Import
layer <- read_sf("1.Data\\data_raw\\MontanaCadastral\\Montana_Cadastral\\OWNERPARCEL.shp")

#      [Projection Transformation]                                          ####

# Checking Projection
st_crs(layer)

# Changing Projection
layer <- st_transform(layer, crs = targetProj)

#      [Subsetting Important Parcel Types]                                  ####

# Listing Unique Parcel Types
unique(layer$PropType)

# Subsetting Parcels of Interest
layer <- layer %>% filter(!(PropType %in% c("EP - Exempt Property",
                                          "VAC_R - Vacant Land - Rural",
                                          "VAC_U - Vacant Land - Urban",
                                          "GOLF - Golf Course",
                                          "MINE - Mining Claim",
                                          "TP - Tribal Property",
                                          "MC - Mining Claim",
                                          "VU - Vacant Land Urban",
                                          NA)))


#      [Data Export]                                                        ####

# Creating a list of unique Counties
CountyNames <- unique(layer$CountyName)

#        [All Data Export]                                                  ####

st_write(obj = layer,
         dsn = "1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership_exception.shp",
         append = FALSE)

#        [County export]                                                    ####

# For Loop to do a per 

for (i in 1:length(CountyNames)) {
  
  # Export Script
  st_write(obj = subset(layer, layer$CountyName == CountyNames[i]),
           dsn = paste0("1.Data\\data_clean\\MontanaCadastral\\CountyBasedSubset\\MontanaCadastral_",
                        CountyNames[i],
                        "_exception.shp"),
           append = FALSE)
  
  # Iterator Tracker
  print(i)
}


#      [Cleaning Environment and Memory]                                    ####

# Cleaning Environment
rm(layer) 
rm(i) 
rm(CountyNames)

# Cleaning RAM 
gc()

###############################################################################
#   [Montana NLCD]                                                          ####
#      [Data]                                                               ####

# Montana shapefile
shp_montana <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp") %>% 
  st_transform(crs = "epsg:5070") %>% 
  vect()

# Rasters
raster_list <- list.files("1.Data/data_raw/NLCD",
                          pattern = "img",
                          full.names = T)

#      [Protocol]                                                           ####

# Export Directory
export_dir <- "1.Data/data_clean/NLCD"

for (i in 1:length(raster_list)) {
  
  # Import Raster filepath and make it into a raster
  raster <- rast(raster_list[[i]])
  
  raster_name <- str_extract(raster_list[[i]],
                             pattern = "(?<=_)\\d\\d\\d\\d(?=_)")
  
  # Raster Protocol
  raster_clean <- raster %>% 
    crop(shp_montana) %>% 
    mask(shp_montana) %>% 
    project("EPSG:4326")
  
  # Export
  writeRaster(raster_clean,
              filename = paste0(export_dir,
                                "/nlcd_",raster_name,".tif"),
              overwrite = T)
  
  # Iteration Tracker
  print(paste0(i, " out of ", length(raster_list), " completed"))
}
###############################################################################
#   [Montana NDVI]                                                          ####

# Sourcing to another script for execution
source("1.Scripts\\1.DataCleaning\\NDVI_calculation.R")

###############################################################################
