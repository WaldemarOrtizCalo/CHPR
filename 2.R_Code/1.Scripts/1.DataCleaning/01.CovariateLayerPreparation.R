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
library(terra)
library(foreach)
library(raster)

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
#   [Montana Mobility Report]                                               ####

# Import Mobility Report
Mobility_Report <- list.files("1.Data/data_raw/GoogleMobilityReports",
                              pattern = "US",
                              full.names = T) %>% lapply(read_csv) %>% bind_rows()

# Montana Counties 
county_boundaries <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp") %>% 
  mutate(NAME = str_replace(NAME,"&", "and")) %>% 
  rename(county = NAME)

# Making Monthly Summaries
mobilityreport_montana <- Mobility_Report %>% 
  filter(sub_region_1 == "Montana") %>% 
  drop_na(sub_region_2) %>% 
  rename(state = sub_region_1,
         county = sub_region_2) %>% 
  mutate(county = str_remove(county," County")) %>% 
  mutate(year = year(date),.after = date) %>% 
  mutate(month = month(date),.after = year) %>% 
  group_by(county,year,month) %>% 
  summarise(retail_and_recreation_percent_change_from_baseline = mean(retail_and_recreation_percent_change_from_baseline),
            grocery_and_pharmacy_percent_change_from_baseline = mean(grocery_and_pharmacy_percent_change_from_baseline),
            parks_percent_change_from_baseline = mean(parks_percent_change_from_baseline),
            transit_stations_percent_change_from_baseline = mean(transit_stations_percent_change_from_baseline),
            workplaces_percent_change_from_baseline = mean(workplaces_percent_change_from_baseline),
            residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline)) %>% 
  rename(v1 = retail_and_recreation_percent_change_from_baseline,
         v2 = grocery_and_pharmacy_percent_change_from_baseline,
         v3 = parks_percent_change_from_baseline,
         v4 = transit_stations_percent_change_from_baseline,
         v5 = workplaces_percent_change_from_baseline,
         v6 = residential_percent_change_from_baseline)

# Appending Summaries to spatial polygons of the counties
mobilityreport_spatial <- mobilityreport_montana %>% 
  left_join(county_boundaries) %>% st_as_sf(sf_column_name = "geometry")

# Export Shapefile
st_write(mobilityreport_spatial,
          "1.Data/data_clean/montana_mobilityreport/montana_mobilityreport.shp",
         append = F)

# Export Key to shapefile
data.frame(key = c("v1","v2","v3","v4","v5","v6"),
           variable = c("retail_and_recreation_percent_change_from_baseline",
                        "grocery_and_pharmacy_percent_change_from_baseline",
                        "parks_percent_change_from_baseline",
                        "transit_stations_percent_change_from_baseline",
                        "workplaces_percent_change_from_baseline",
                        "residential_percent_change_from_baseline")) %>% 
  write_csv(file = "1.Data/data_clean/montana_mobilityreport/key_montana_mobilityreport.csv")

###############################################################################
#   [Global Surface Water]                                                  ####

# List of raw data
layers <- list.files("D:/Drive/Research/CPHR/CPHR_Workspace/1.Data/data_raw/GlobalSurfaceWater",
                     full.names = T,
                     pattern = ".tif")

# Montana Polygon
montana_boundary <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp")

# Variable type
data_type <- c("change",
               "extent",
               "occurrence",
               "recurrence",
               "seasonality",
               "transitions")

# Protocol
foreach(i = 1:length(data_type),
        .combine = rbind, 
        .errorhandling = "pass") %do% {
          
          # Isolating variable tiles
          tiles <- layers %>% 
            str_subset(pattern = data_type[[i]]) %>% 
            lapply(rast)
          
          # Mosaic
          mos <- mosaic(tiles[[1]],tiles[[2]],fun="mean")
          
          # Cropping and Masking raster to Montana boundary
          raster_clean <- mos %>% 
            crop(montana_boundary) %>% 
            mask(montana_boundary)
          
          # Export
          writeRaster(raster_clean,
                      filename = paste0("1.Data/data_clean/global_surface_water/",
                                        data_type[[i]],"_gsw_2021",".tif"),
                      overwrite = T)
          
          return(i)
        }

###############################################################################
#   [Fishing Access]                                                        ####

fish_access_polygons <- st_read("D:\\Drive\\Research\\CPHR\\CPHR_Workspace\\1.Data\\data_raw\\FishingAccess\\Fishing_Access_Sites_-_Polygons.shp") %>% 
  st_write("1.Data/data_clean/fishing_access/fish_access_polygons.shp")

fish_access_points <- st_read("D:\\Drive\\Research\\CPHR\\CPHR_Workspace\\1.Data\\data_raw\\FishingAccess\\Fishing_Access_Sites_-_Points.shp") %>% 
  st_write("1.Data/data_clean/fishing_access/fish_access_points.shp")

###############################################################################