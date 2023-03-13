#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-08 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(tidyverse)
library(readxl)

#      Functions                                                            ####

#      Data                                                                 ####
gap_data <- list.files("D:/Drive/Research/CPHR/CPHR_Workspace/1.Data/data_clean/gap_clean",
                       full.names = T,
                       pattern = "GAP_clean_all.shp") %>% st_read()

key_splitting_gapclean <- read_excel("1.Data/data_clean/gap_clean/key_splitting_gapclean.xlsx") %>% 
  rename(divisions = Divisions) %>% 
  mutate(divisions = round(divisions)) %>% 
  mutate(divisions = ifelse(divisions == 0, 1 ,divisions))
  

# Montana Country Polygons
county_boundaries <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp") %>% 
  rename(county = NAME)

###############################################################################
#   Summary of Data                                                         ####
summary_stats <- gap_data %>% 
  st_drop_geometry() %>% 
  group_by(county) %>% 
  summarize(n_poly = n(),
            area = sum(ar_hctr))

###############################################################################

gap_division <- gap_data %>% 
  left_join(key_splitting_gapclean)

county_names <- gap_division$county %>% unique()
i <- 1 

county_name <- county_names[[i]]

county_sub <- gap_division %>% 
  filter(county == county_name)

divisions <- unique(county_sub$divisions)



