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
#   [Montana Cadastral Data]                                                ####
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

###############################################################################