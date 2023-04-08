#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-30 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(tidyverse)
library(mapview)
library(fasterize)
library(foreach)
library(raster)

#      Functions                                                            ####

#      Data                                                                 ####

# GAP Layer
gap_clean <- st_read("D:\\Drive\\Research\\CPHR\\CPHR_Workspace\\1.Data\\data_clean\\gap_clean\\GAP_clean_all.shp")

# NDVI template 
raster_template <- raster("D:\\Drive\\Research\\CPHR\\CPHR_Workspace\\1.Data\\data_clean\\NDVI\\ndvi_2006-12-20.tif") 
values(raster_template)<- NA

###############################################################################
#   Rasterization                                                           ####
# List of GAP statuses
GAP_Sts_list <- unique(gap_clean$GAP_Sts)

for (i in 1:length(GAP_Sts_list)) {
  
  # Subsetting Gap statuses
  gap_sub <- gap_clean %>% 
    filter(GAP_Sts == GAP_Sts_list[[i]])
  
  # Rasterization
  ras <- fasterize(gap_sub,
                   raster_template) %>% 
    rast()
  
  # Export
  writeRaster(ras,
              paste0("1.Data/data_clean/gap_clean_rasters/",
                     "gapclean_raster_status",
                     GAP_Sts_list[[i]],
                     ".tif"),
              overwrite = T)
}

###############################################################################
#   Rasterization - Native Land                                             ####
# List of GAP statuses
land_designation <- "Native American Land Area"

# Subsetting Gap statuses
gap_sub <- gap_clean %>% 
  filter(d_Ds_Tp == land_designation)

# Rasterization
ras <- fasterize(gap_sub,
                 raster_template) %>% 
  rast()

# Export
writeRaster(ras,
            paste0("1.Data/data_clean/gap_clean_rasters/",
                   "gapclean_raster_status_",
                   "tribal",
                   ".tif"),
            overwrite = T)

###############################################################################