#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-12-29 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(MODISTools)
library(terra)
library(raster)
library(tidyverse)
library(stringr)
library(foreach)
library(doParallel)
library(sf)

#      Functions                                                            ####

#      Data                                                                 ####
shp_montana <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp") %>% 
  vect()

###############################################################################
#   NDVI Download                                                           ####
#      NDVI Details                                                         ####
products <- mt_products()
bands <- mt_bands(product = "VNP13A1")
dates <- mt_dates(product = "VNP13A1", lat = 46.8797, lon = -110.3626)

#      Extent Information                                                   ####

lat_list <- seq(from = 44, to = 49,by = 1)
lon_list <- seq(from = -116, to = -104,by = 1)
coord_grid <- expand.grid(lat_list,lon_list)

#      Foreach Version                                                      ####

# Cluster Number
cl <- makeCluster(6)
registerDoParallel(cl)

# Exporting Packages
clusterEvalQ(cl,
             {
               library(tidyverse)
               library(terra)
               library(foreach)
               library(stringr)
               library(MODISTools)
             })

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("coord_grid","dates"), envir=environment())

# Progress: 213 completed. Start at 214
foreach (t = 214:nrow(dates),
         .combine = c,
         .errorhandling = "pass") %do% {
           
           # Subsetting the date for the NDVI 
           date <- dates[t,2]
           
           # Foreach loop to download all tiles for that date
           foreach(i = 1:nrow(coord_grid),
                   .combine = c,
                   .errorhandling = "pass") %dopar% 
             {
               # Data downloader
               montana_ndvi<- mt_subset(product = "VNP13A1",
                                        lat = coord_grid[i,1],
                                        lon =  coord_grid[i,2],
                                        band = "500_m_16_days_NDVI",
                                        start = date,
                                        end = date,
                                        km_lr = 50,
                                        km_ab = 100,
                                        site_name = paste0("MT_NDVI_",date),
                                        internal = T,
                                        progress = F) %>% 
                 mt_to_raster(reproject = T) %>% 
                 rast() 
               
               # Raster Export
               writeRaster(montana_ndvi,
                           filename = paste0(
                             "1.Data/data_raw/NDVI/",
                             "MT_NDVI_",
                             date,
                             "_t",
                             str_pad(i, 3, pad = "0"),
                             ".tif"),
                           overwrite=TRUE)
               
               # Iteration 
               return(i)
             }
           
           # Making a list of the raster tiles
           tiles <- list.files("1.Data/data_raw/NDVI",
                               full.names = T,
                               pattern = "t\\d\\d\\d.tif") %>% 
             str_subset(pattern = date)
           
           # Compiling Tiles to raster
           tile_ras <- vrt(tiles, "test.vrt", 
                           overwrite=TRUE)
           
           # Final NDVI map for Montana and date
           final <- mask(tile_ras,shp_montana) 
           
           # Exporting Raster
           writeRaster(x = final,
                       filename = paste0("1.Data/data_clean/NDVI/NDVI_",
                                         date,
                                         ".tif"),
                       overwrite = T)
           
           # Deleting raw tiles
           file.remove(tiles)
           
           # Check
           return(paste0(t, " out of ", nrow(dates), " completed"))
         }

###############################################################################