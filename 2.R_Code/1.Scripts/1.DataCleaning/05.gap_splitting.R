#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-03-30 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(tidyverse)
library(zip)

#      Functions                                                            ####

#      Data                                                                 ####
fps <- list.files("1.Data/data_clean/gap_clean",
                  full.names = T)

filenames <- list.files("1.Data/data_clean/gap_clean",
                        full.names = F) %>% 
  str_remove(".dbf")%>% 
  str_remove(".prj")%>% 
  str_remove(".shp")%>% 
  str_remove(".shx") %>% 
  str_subset("GAP_clean_all", negate = T) %>% 
  unique()
  
###############################################################################


for (i in 1:length(filenames)) {
  
  files_fps <- fps %>% 
    str_subset(pattern = filenames[[i]])
  
  zip(zipfile = paste0("1.Data/data_clean/gap_clean_NEAR_zip/",filenames[[i]],".zip"),
      files = files_fps,
      include_directories = F,
      mode = "cherry-pick")
  
  print(paste(i, "out of", length(filenames)))
}

###############################################################################



