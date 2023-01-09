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

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "winter",
          ifelse (d >= SE & d < SS, "spring",
                  ifelse (d >= SS & d < FE, "summer", "fall")))
}

#      Data                                                                 ####

# Montana shapefile
shp_montana <- st_read("1.Data\\data_clean\\MontanaBoundaries\\MontanaCountyBoundaries.shp") %>% 
  vect()

###############################################################################
#   NDVI Calculation and Export                                             ####
#      Importing Data                                                       ####

# NDVI List
ndvi_list <- list.files("1.Data/data_raw/NDVI_Earthdata",
                        pattern = ".hdf",
                        full.names = T)

ndvi_names <- list.files("1.Data/data_raw/NDVI_Earthdata",
                         pattern = ".hdf",
                         full.names = F) %>% 
  str_extract("A\\d\\d\\d\\d\\d\\d\\d") %>% 
  str_remove("A") %>% unique()


#      NDVI Calculation                                                     ####

# Export Directory
export_dir <- "1.Data/data_clean/NDVI_MODIS"

# Starting at 100 to resume.
for (i in 100:length(ndvi_names)) {
  
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

# Clearing everything for further coding
rm(list = ls())
gc()
###############################################################################
#   Summary Rasters                                                         ####
#      Yearly Summaries                                                     ####
#        Creating NDVI Master Database                                      ####

# List of NDVI tiles
ndvi_list <- list.files("1.Data/data_clean/NDVI_MODIS",
                        full.names = T)

# Extracting Dates 
ndvi_dates <- list.files("1.Data/data_clean/NDVI_MODIS",
                         full.names = F) %>% 
  str_remove("ndvi_") %>% 
  str_remove(".tif") %>% 
  ymd()

# Creating Database
ndvi_database <- data.frame(filepath = ndvi_list,
                            dates = ndvi_dates) %>% 
  mutate(year = year(dates)) %>% 
  filter(year != 2007)

#        Processing NDVI layers                                             ####

# Indicating Export Directory
export_dir <- "3.Outputs/Cindy_NDVI_Database/summaries_yearly"

# Making list of unique years
years_unique <- unique(ndvi_database$year)

for (i in 1:length(years_unique)) {
 
  # Extracting a certain year
   target_year <- years_unique[i]
  
  # Subsetting Database for target ndvi files
  target_layers <- ndvi_database %>% 
    filter(year == target_year) %>% 
    .$filepath
  
  # Stacking NDVI tiles and summarizing across
  ndvi_summary <- rast(target_layers) %>% 
    app(fun=mean)
  
  # Exporting NDVI summary raster
  writeRaster(ndvi_summary,
              filename = paste0(export_dir,
                                "/ndvi_avg_yr_",
                                target_year,"_250m.tif"))
  
  # Iteration Tracker
  print(paste0(i, " out of ", length(years_unique), " completed"))
}

#      Seasonal Summaries                                                   ####

#        Creating Seasonal Reference                                        ####


#           Season 1: Winter                                                ####


s_start <- expand.grid(season = "winter",
                       year = unique(ndvi_database$year),
                       month = "12",
                       day = "21") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(start_date = ymd(date_str),.keep = "unused")



s_end <- expand.grid(year = unique(ndvi_database$year)+1,
                     month = "03",
                     day = "19") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(end_date = ymd(date_str),.keep = "unused") %>% pull()


winter_matrix <- s_start %>% mutate(end_date = s_end)



#           Season 2: Spring                                                ####
s_start <- expand.grid(season = "spring",
                       year = unique(ndvi_database$year),
                       month = "03",
                       day = "20") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(start_date = ymd(date_str),.keep = "unused")



s_end <- expand.grid(year = unique(ndvi_database$year),
                     month = "06",
                     day = "20") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(end_date = ymd(date_str),.keep = "unused") %>% pull()


spring_matrix <- s_start %>% mutate(end_date = s_end)



#           Season 2: summer                                                ####
s_start <- expand.grid(season = "summer",
                       year = unique(ndvi_database$year),
                       month = "06",
                       day = "21") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(start_date = ymd(date_str),.keep = "unused")



s_end <- expand.grid(year = unique(ndvi_database$year),
                     month = "09",
                     day = "22") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(end_date = ymd(date_str),.keep = "unused") %>% pull()


summer_matrix <- s_start %>% mutate(end_date = s_end)



#           Season 4: fall                                                  ####
s_start <- expand.grid(season = "fall",
                       year = unique(ndvi_database$year),
                       month = "09",
                       day = "23") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(start_date = ymd(date_str),.keep = "unused")



s_end <- expand.grid(year = unique(ndvi_database$year),
                     month = "12",
                     day = "20") %>% 
  mutate(date_str = paste(year,month,day,sep = "-"),.keep = "unused") %>% 
  mutate(end_date = ymd(date_str),.keep = "unused") %>% pull()


fall_matrix <- s_start %>% mutate(end_date = s_end)



#           Season final                                                    ####

seasonal_reference <- bind_rows(fall_matrix,
                                 winter_matrix,
                                 spring_matrix,
                                 summer_matrix) %>% 
  mutate(int = interval(start_date,end_date)) %>% 
  mutate(year = year(start_date),.after = season)

#        Creating NDVI Master Database                                      ####
#           NDVI Database                                                   ####

# List of NDVI tiles
ndvi_list <- list.files("1.Data/data_clean/NDVI_MODIS",
                        full.names = T)

# Extracting Dates 
ndvi_dates <- list.files("1.Data/data_clean/NDVI_MODIS",
                         full.names = F) %>% 
  str_remove("ndvi_") %>% 
  str_remove(".tif") %>% 
  ymd()

# Creating Database
ndvi_database <- data.frame(filepath = ndvi_list,
                            dates = ndvi_dates) %>% 
  mutate(year = year(dates)) %>% 
  filter(year != 2007) %>% 
  mutate(season = getSeason(dates)) 





###############################################################################
