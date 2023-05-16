library(terra)
library(tidyverse)
library(sf)
library(mapview)
library(zip)

layer_lake <- st_read("D:\\Drive\\Work\\CPHR\\CPHR_Workspace\\1.Data\\data_clean\\gap_clean\\gapclean_Lake.shp")

mapview(layer_lake)

split1 <- layer_lake %>% 
  slice_sample(n = 200)

mapview(split1)

layer_lake %>% 
  slice((1:300)) %>% 
  st_write("3.Outputs/test/GAP_clean_lake_sp1.shp",
           append = F)

layer_lake %>% 
  slice((301:600)) %>% 
  st_write("3.Outputs/test/GAP_clean_lake_sp2.shp",
           append = F)

layer_lake %>% 
  slice((601:900)) %>% 
  st_write("3.Outputs/test/GAP_clean_lake_sp3.shp",
           append = F)

layer_lake %>% 
  slice((901:1200)) %>% 
  st_write("3.Outputs/test/GAP_clean_lake_sp4.shp",
           append = F)

layer_lake %>% 
  slice((1201:1546)) %>% 
  st_write("3.Outputs/test/GAP_clean_lake_sp5.shp",
           append = F)

###############################################################################

#      Data                                                                 ####
fps <- list.files("3.Outputs/test",
                  full.names = T)

filenames <- list.files("3.Outputs/test",
                        full.names = F) %>% 
  str_remove(".dbf")%>% 
  str_remove(".prj")%>% 
  str_remove(".shp")%>% 
  str_remove(".shx") %>% 
  str_subset("GAP_clean_all", negate = T) %>% 
  unique()



for (i in 1:length(filenames)) {
  
  files_fps <- fps %>% 
    str_subset(pattern = filenames[[i]])
  
  zip(zipfile = paste0("3.Outputs/test_zip/",filenames[[i]],".zip"),
      files = files_fps,
      include_directories = F,
      mode = "cherry-pick")
  
  print(paste(i, "out of", length(filenames)))
}

###############################################################################