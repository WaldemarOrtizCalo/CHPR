#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-01-08 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(tidyverse)
library(sf)
library(stringr)
library(foreach)
library(doParallel)
library(lubridate)
  
#      Functions                                                            ####

date2character <- function(x){
  year_string <- str_sub(x,start = 1,end = 4)
  day_string <- str_sub(x,start = 5,end = 8) %>% as.numeric()
  origin_string <- ymd(as.numeric(paste0(year_string,"0101")))
  
  date_final <- as.Date(day_string,origin = origin_string) %>% 
    as.character()
  return(date_final)
}

#      Data                                                                 ####

# Montana shapefile
shp_montana <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp") %>% 
  vect()

# NDVI List
ndvi_list <- list.files("1.Data/data_raw/NDVI_Earthdata",
                        pattern = ".hdf",
                        full.names = T)

ndvi_names <- list.files("1.Data/data_raw/NDVI_Earthdata",
                         pattern = ".hdf",
                         full.names = F) %>% 
  str_extract("A\\d\\d\\d\\d\\d\\d\\d") %>% 
  str_remove("A") %>% unique()

###############################################################################
#   NDVI Calculation and Export                                             ####

# Export Directory
export_dir <- "1.Data/data_clean/NDVI_MODIS"

for (i in 1:length(ndvi_names)) {
  
  # Making a List of Rasters 
  raster_list <- str_subset(ndvi_list,
                            pattern = ndvi_names[[i]])
  
  # Creating the NIR layers
  R_NIR_1 <- ndvi_list[1] %>% rast() %>% .[[5]]
  R_red_1 <- ndvi_list[1] %>% rast() %>% .[[4]]
  R_NIR_2 <- ndvi_list[2] %>% rast() %>% .[[5]]
  R_red_2 <- ndvi_list[2] %>% rast() %>% .[[4]]
  R_NIR_3 <- ndvi_list[3] %>% rast() %>% .[[5]]
  R_red_3 <- ndvi_list[3] %>% rast() %>% .[[4]]
  
  # Calculating NDVI
  ndvi_1 <- (R_NIR_1 - R_red_1)/(R_NIR_1 + R_red_1)
  ndvi_2 <- (R_NIR_2 - R_red_2)/(R_NIR_2 + R_red_2)
  ndvi_3 <- (R_NIR_3 - R_red_3)/(R_NIR_3 + R_red_3)
  
  # Creating final NDVI layer
  ndvi_final <- merge(ndvi_1,ndvi_2,ndvi_3) %>% 
    project("EPSG:4326") %>% 
    mask(shp_montana) %>% 
    crop(shp_montana)
  
  # Exporting Raster
  writeRaster(ndvi_final,
              filename = paste0(export_dir,"/ndvi_",date2character(ndvi_names[[i]]),
                                ".tif"),
              overwrite = T)
  
  # Iteration Tracker
  print(paste0(i, " out of ", length(ndvi_names), " completed"))
  
}

###############################################################################