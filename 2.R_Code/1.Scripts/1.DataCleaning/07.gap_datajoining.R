#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-05-16 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(readr)

#      Functions                                                            ####

#      Data                                                                 ####

#        File lists                                                         ####

# Creating ziplist
zip_list <- list.files("1.Data\\data_raw\\NEAR_VISTA",pattern = "zip",
                       full.names = T)

# Creating ziplist
tsv_list <- list.files("1.Data\\data_raw\\NEAR_VISTA",pattern = "tsv",
                       full.names = T)

#        Spatial Data                                                       ####

gap_data <- list.files("1.Data/data_clean/gap_clean",
                       full.names = T,
                       pattern = ".shp") %>% 
  str_subset("GAP_clean_all",negate = T) %>% 
  lapply(st_read) %>% 
  bind_rows() 

###############################################################################
#   Unzipping files                                                         ####
#      CDL & CEL                                                            ####

# Loop for extraction
for (i in 1:length(zip_list)) {
  
  # Unzipping Filed
  zip_file <- unzip(zip_list[[i]],
                    list = F,
                    junkpaths = F,
                    exdir = "1.Data/data_raw/NEAR_VISTA_unzipped")
  
  # Filetypes
  filetypes <- c("cdl.tsv","cel.tsv")
  
  for (j in 1:length(filetypes)) {
    
    # Subsetting the filetypes
    sub <- zip_file %>% 
      str_subset(filetypes[[j]])
    
    # Importing Dataframe
    df <- read_tsv(gzfile(sub),
                   show_col_types = FALSE) 
    
    # Indicating Directories
    import_dir <- "1.Data/data_raw/NEAR_VISTA_unzipped/"
    export_dir <- "1.Data/data_clean/NEAR_VISTA_data_unzipped/"
    
    # Filename for Export
    filename <- paste0(export_dir,
                       "poly_",
                       i,
                       "_",
                       str_remove(filetypes[[j]],".tsv"),
                       ".csv")
    
    # Exporting csv
    write_csv(df,
              filename,
              append = F)
  }
  
  print(paste0(i, " out of ", length(zip_list), " completed"))
}

#      TSV Files - Visitor Count                                            ####

# Loop for extraction
for (i in 1:length(tsv_list)) {
  
  # Importing Dataframe
  df <- read_tsv(gzfile(tsv_list[[i]]),
                 show_col_types = FALSE) 
  
  # Indicating Directories
  export_dir <- "1.Data/data_clean/NEAR_VISTA_data_unzipped/"
  
  # Filename for Export
  filename <- paste0(export_dir,
                     "poly_",
                     i,
                     "_",
                     "visitorcount",
                     ".csv")
  
  # Exporting csv
  write_csv(df,
            filename,
            append = F)
  
  print(paste0(i, " out of ", length(tsv_list), " completed"))
}

###############################################################################
#   Creating Spatial Layers with NEAR VISTA data (dev)                      ####
#      NEAR VISTA Data                                                      ####

# Variables 
var <- c("cdl","cel","visitorcount")

# NEAR VISTA Files

cdl_df <- list.files("1.Data/data_clean/NEAR_VISTA_data_unzipped",
                    full.names = T) %>% 
  str_subset(var[[1]]) %>% 
  lapply(read_csv)%>% 
  lapply(rename,plygnID = "Polygon Id") %>% 
  lapply(select,!c("Common Daytime Postal1",
                   "Common Daytime Postal2",
                   "Common Daytime Custom1",
                   "Common Daytime Custom2")) %>% 
  bind_rows()

cel_df <- list.files("1.Data/data_clean/NEAR_VISTA_data_unzipped",
                     full.names = T) %>% 
  str_subset(var[[2]]) %>% 
  lapply(read_csv)%>% 
  lapply(rename,plygnID = "Polygon Id") %>% 
  lapply(select,!c("Common Evening Postal1",
                   "Common Evening Postal2",
                   "Common Evening Custom1",
                   "Common Evening Custom2")) %>% 
  bind_rows()

vc_df <- list.files("1.Data/data_clean/NEAR_VISTA_data_unzipped",
                     full.names = T) %>% 
  str_subset(var[[3]]) %>% 
  lapply(read_csv) %>% 
  lapply(rename,plygnID = "Polygon ID") %>% 
  bind_rows()


#      Creating Spatial Layer                                               ####
#           Median Distance to Polygons                                     ####

# Calculating Median Distance
dist_traveled <- cel_df %>% 
  group_by(plygnID) %>% 
  summarize(median_dist = stats::median(.$`Common Evening Distance Mi`,
                                        na.rm = T))

###############################################################################
