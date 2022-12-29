library(MODISTools)
library(terra)
library(raster)
library(tidyverse)
library(stringr)

products <- mt_products()
head(products)


bands <- mt_bands(product = "VNP13A1")
head(bands)

dates <- mt_dates(product = "VNP13A1", lat = 46.8797, lon = -110.3626)
head(dates)

lat_list <- seq(from = 44, to = 49,by = 1)
lon_list <- seq(from = -116, to = -104,by = 1)
coord_grid <- expand.grid(lat_list,lon_list)

montana_ndvi<- mt_subset(product = "VNP13A1",
                         lat = coord_grid[1,1],
                         lon =  coord_grid[1,2],
                         band = "500_m_16_days_NDVI",
                         start = dates[1,2],
                         end = dates[1,2],
                         km_lr = 50,
                         km_ab = 100,
                         site_name = paste0("MT_NDVI_",dates[1,2]),
                         internal = T,
                         progress = F) %>% 
  mt_to_raster(reproject = T) %>% 
  rast() 

writeRaster(montana_ndvi,
            filename = paste0(
              "1.Data/data_raw/NDVI/",
              "MT_NDVI_",
              dates[1,2],
              "_t",
              str_pad(1, 3, pad = "0"),
              ".tif"))

for (i in 1:nrow(coord_grid)) {
  
  # Data downloader
  montana_ndvi<- mt_subset(product = "VNP13A1",
                           lat = coord_grid[i,1],
                           lon =  coord_grid[i,2],
                           band = "500_m_16_days_NDVI",
                           start = dates[1,2],
                           end = dates[1,2],
                           km_lr = 50,
                           km_ab = 100,
                           site_name = paste0("MT_NDVI_",dates[1,2]),
                           internal = T,
                           progress = F) %>% 
    mt_to_raster(reproject = T) %>% 
    rast() 
  
  # Raster Export
  writeRaster(montana_ndvi,
              filename = paste0(
                "1.Data/data_raw/NDVI/",
                "MT_NDVI_",
                dates[1,2],
                "_t",
                str_pad(i, 3, pad = "0"),
                ".tif"),
              overwrite=TRUE)
  
  # Iteration Tracker
  print(paste0(i, " out of ", nrow(coord_grid), " completed"))
}

###############################################################################

montana_ndvi1 <- mt_subset(product = "VNP13A1",
                         lat = 44.0,
                         lon =  -116,
                         band = "500_m_16_days_NDVI",
                         start = dates[1,2],
                         end = dates[1,2],
                         km_lr = 50,
                         km_ab = 100,
                         site_name = "MT_NDVI",
                         out_dir = "1.Data/data_raw/NDVI",
                         internal = T,
                         progress = T)

montana_ndvi2 <- mt_subset(product = "VNP13A1",
                           lat = 44.0,
                           lon =  -115,
                           band = "500_m_16_days_NDVI",
                           start = dates[1,2],
                           end = dates[1,2],
                           km_lr = 50,
                           km_ab = 100,
                           site_name = "MT_NDVI",
                           out_dir = "1.Data/data_raw/NDVI",
                           internal = T,
                           progress = T)

montana_ndvi3 <- mt_subset(product = "VNP13A1",
                           lat = 45.0,
                           lon =  -116,
                           band = "500_m_16_days_NDVI",
                           start = dates[1,2],
                           end = dates[1,2],
                           km_lr = 50,
                           km_ab = 100,
                           site_name = "MT_NDVI",
                           out_dir = "1.Data/data_raw/NDVI",
                           internal = T,
                           progress = T)

montana_ndvi4 <- mt_subset(product = "VNP13A1",
                           lat = 45.0,
                           lon =  -115,
                           band = "500_m_16_days_NDVI",
                           start = dates[1,2],
                           end = dates[1,2],
                           km_lr = 50,
                           km_ab = 100,
                           site_name = "MT_NDVI",
                           out_dir = "1.Data/data_raw/NDVI",
                           internal = T,
                           progress = T)


ras1 <- mt_to_raster(montana_ndvi1,
                    reproject = T)

ras2 <- mt_to_raster(montana_ndvi2,
                    reproject = T)

ras3 <- mt_to_raster(montana_ndvi3,
                     reproject = T)

ras4 <- mt_to_raster(montana_ndvi4,
                     reproject = T)

r1 <- ras1 %>% rast() %>% raster()
r2 <- ras2 %>% rast() %>% raster()
r3 <- ras3 %>% rast() %>% raster()
r4 <- ras4 %>% rast() %>% raster()


mapview::mapview(r1) + mapview::mapview(r2) +
  mapview::mapview(r3) + mapview::mapview(r4)

coord_grid[1:2,]
