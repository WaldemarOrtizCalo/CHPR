#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-07-30 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(mapview)

#      Functions                                                            ####

#      Data                                                                 ####

# Road Layer
roads <- read_sf("1.Data\\raw_data\\Roads\\rds2000.shp")

###############################################################################
mapview(roads)
