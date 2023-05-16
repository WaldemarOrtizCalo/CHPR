#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-05-16 

# Purpose: Script to reproject shapefiles to EPSG 5070 for invest data

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(tidyverse)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####
all <- st_read("1.Data\\data_clean\\gap_clean\\GAP_clean_all.shp")
mt_poly <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp")

###############################################################################
#   GAP Cleaning                                                            ####
#      Adding unique polyid column for invest data extraction               ####

added_polyid <- all %>% 
  mutate(invest_id = paste0("pid_",str_pad(1:nrow(all),
                                          width = nchar(nrow(all)),
                                          pad = "0")),
         .after = "plygnID")

#      Reprojecting                                                         ####

reprojected <- added_polyid %>% 
  st_transform(5070)

#      Exporting                                                            ####

st_write(reprojected,
         "1.Data/data_clean/INVEST_AOI_polygons/gap_poly_invest.shp")
   
###############################################################################
#   MT Polygon                                                              ####
#      Combining Polygons                                                   ####

mt_joined <- mt_poly %>% 
  summarise()

#      Reprojecting                                                         ####

reprojected_mt <- mt_joined %>% 
  st_transform(5070)

#      Export                                                               ####

st_write(reprojected_mt,
         "1.Data/data_clean/INVEST_AOI_polygons/mt_poly_invest.shp")

###############################################################################