#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-05-16 

# Purpose: Script to reproject shapefiles to EPSG 5070 for invest data

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(tidyverse)

#      Functions                                                            ####

#      Data                                                                 ####
all <- st_read("1.Data\\data_clean\\gap_clean\\GAP_clean_all.shp")

###############################################################################
#   Adding unique polyid column for invest data extraction                  ####
