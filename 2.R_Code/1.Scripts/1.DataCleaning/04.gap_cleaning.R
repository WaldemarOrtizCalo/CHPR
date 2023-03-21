#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2023-02-07 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(terra)
library(sf)
library(tidyverse)
library(mapview)
library(ggpubr)
library(foreach)
library(doParallel)

#      Functions                                                            ####

#      Data                                                                 ####
#        Shapefiles                                                         ####

# Montana Country Polygons
county_boundaries <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp")

# Montana Protected Areas
protected_areas <- st_read("1.Data/data_clean/Montana_ProtectedAreas/Montana_ProtectedAreas.shp") %>% 
  st_make_valid()

# Roads
montana_roads <- read_sf("1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp") 

# Montana Cadastral Data
montana_cadastral <- read_sf("1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership_exception.shp") 

#        List of counties                                                   ####

# Making a list of county names 
county_list <- unique(county_boundaries$NAME)

# Checking if the list of county names are the same across layers
county_list_cadastral <- unique(montana_cadastral$CountyName)

county_list %in% county_list_cadastral

# Fixing The one county that is a problem. Lewis and Clark is the problem
# because one list has the word and and the other has an ampersand (&).

county_list <- str_replace(county_list,
                           pattern = "Lewis & Clark",
                           replacement = "Lewis and Clark")

###############################################################################
#   Gap Cleaning Protocol                                                   ####

#      Protocol Settings                                                    ####

# Error Buffer (in meters)
buffer_dist <- 20

# Working Projection EPSG code 
working_proj <- 5070

# Cluster Settings 
cl <- makeCluster(4)
registerDoParallel(cl)

# Exporting Packages to Cluster
clusterEvalQ(cl,
             {
               library(tidyverse)
               library(sf)
               library(foreach)
             })

#      Cleaning Protocol                                                    ####

foreach(i = 1:length(county_list),
        .errorhandling = "pass",
        .combine = rbind) %do% {
          
          #   County-specific sub-setting                                             ####
          
          # County Name
          county_name <- county_list[[i]]
          
          # County 
          county_boundary <- county_boundaries %>% 
            filter(NAME == county_name) 
          
          # Protected Areas
          county_protectedareas <- protected_areas %>% 
            st_make_valid() %>% 
            st_intersection(county_boundary) 
          
          # Roads
          county_roads <- montana_roads %>% 
            st_intersection(county_boundary)
          
          # Cadastral 
          county_cadastral <- montana_cadastral %>% 
            filter(CountyName == county_name) 
          
          #   GAP Cleaning: Overlapping Statuses                                      ####
          #      Filtering by Status                                                  ####
          
          GAP1 <- county_protectedareas %>%  filter(GAP_Sts == "1") 
          GAP2 <- county_protectedareas %>%  filter(GAP_Sts == "2")
          GAP3 <- county_protectedareas %>%  filter(GAP_Sts == "3")
          GAP4 <- county_protectedareas %>%  filter(GAP_Sts == "4")
          
          #      Eliminating Between Status Overlap                                   ####
          
          GAP1_combined <- GAP1 %>% st_combine() %>% st_make_valid()
          GAP2_combined <- GAP2 %>% st_combine() %>% st_make_valid()
          GAP3_combined <- GAP3 %>% st_combine() %>% st_make_valid()
          GAP4_combined <- GAP4 %>% st_combine() %>% st_make_valid()
          
          sf_use_s2(F)
          GAP2_clean <- st_difference(GAP2,GAP1_combined)
          GAP3_clean <- st_difference(GAP3,GAP2_combined)%>% 
            st_difference(GAP1_combined)
          GAP4_clean <- st_difference(GAP4,GAP3_combined) %>% 
            st_difference(GAP2_combined) %>% 
            st_difference(GAP1_combined)
          sf_use_s2(T)
          
          #      Packaging for Further Cleaning                                       ####
          
          GAP_layer_base <- bind_rows(GAP1,GAP2_clean,GAP3_clean,GAP4_clean) 
          
          #   GAP Cleaning: Roads                                                     ####
          #      Preparing Base Gap Layer                                             ####
          
          layer_gap_cleanroads <- GAP_layer_base %>% st_transform(5070)
          county_roads <- county_roads %>% st_transform(5070)
          
          #      Road Relevancy                                                       ####
          
          # Exporting data to clusters
          clusterExport(cl=cl, varlist=c("county_roads","layer_gap_cleanroads"), envir=environment())
          
          # Foreach Loop 
          sf_use_s2(F)
          
          road_relevancy <- foreach(a = 1:nrow(county_roads),
                                    .combine = rbind,
                                    .errorhandling = "pass") %dopar% {
                                      
                                      rd_sub <- county_roads[a,]
                                      intersect <- st_intersects(rd_sub,layer_gap_cleanroads,sparse = F)
                                      
                                      result <- rd_sub %>% mutate(relevant = (any(intersect) == T),
                                                                  .before = 1)
                                      
                                      return(result)
                                      
                                    }
          
          #      Eliminating Roads                                                    ####
          
          # Keeping only relevant roads
          county_roads_relevant <-  filter(road_relevancy,relevant == T)
          
          # Eliminating Roads from the GAP data
          
          log_roads <- foreach(b = 1:nrow(county_roads_relevant),
                               .combine=rbind,
                               .errorhandling = "pass") %do% {
                                 
                                 road_sub <- county_roads_relevant[b,] %>% 
                                   st_buffer(dist = buffer_dist) %>% 
                                   st_combine() %>% 
                                   st_make_valid()
                                 
                                 layer_gap_cleanroads <- st_difference(x = layer_gap_cleanroads,
                                                                       y = road_sub)
                                 
                                 return(data.frame(iteration = b,
                                                   time = Sys.time()))
                               }
          
          # Exporting Foreach Log
          write_csv(log_roads, 
                    file = paste0("3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_",county_name,".csv"))
          
          # Exporting Layer Temp 
          sf_use_s2(T)
          
          layer_gap_cleanroads <- st_collection_extract(layer_gap_cleanroads, "POLYGON")
          
          #   GAP Cleaning: Cadastral                                                 ####
          #      Preparing Base Gap Layer that has been cleaned                       ####
          
          layer_gap_cleanroads_cleancadastral <- layer_gap_cleanroads
          cadastral_exemption_shp <- county_cadastral %>% st_transform(5070)
          
          #      Cadastral Relevancy                                                  ####
          
          # Exporting data to clusters
          clusterExport(cl=cl, varlist=c("cadastral_exemption_shp","layer_gap_cleanroads_cleancadastral"), envir=environment())
          
          # Foreach Loop 
          sf_use_s2(F)
          
          cadastral_relevancy <- foreach(c = 1:nrow(cadastral_exemption_shp),
                                         .combine = rbind,
                                         .errorhandling = "pass") %dopar% {
                                           
                                           cad_sub <- cadastral_exemption_shp[c,]
                                           intersect <- st_intersects(cad_sub,layer_gap_cleanroads_cleancadastral,sparse = F)
                                           
                                           result <- cad_sub %>% mutate(relevant = (any(intersect) == T),
                                                                        .before = 1)
                                           
                                           return(result)
                                           
                                         }
          
          #      Eliminating Cadastral                                                ####
          
          # Keeping only relevant cadastral datapoints
          cadastral_relevant <- filter(cadastral_relevancy,relevant == T)
          
          # Eliminating Cadastral from the GAP data
          sf_use_s2(F)
          
          log_cadastral <- foreach(d = 1:nrow(cadastral_relevant),
                                   .combine=rbind,
                                   .errorhandling = "pass") %do% {
                                     
                                     cad_sub <- cadastral_relevant[d,] %>% 
                                       st_buffer(buffer_dist) %>% 
                                       st_combine() %>% 
                                       st_make_valid()
                                     
                                     layer_gap_cleanroads_cleancadastral <- st_difference(x = layer_gap_cleanroads_cleancadastral,
                                                                                          y = cad_sub)
                                     
                                     return(data.frame(iteration = d,
                                                       time = Sys.time()))
                                   }
          
          # Exporting Foreach Log
          write_csv(log_cadastral, 
                    file = paste0("3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_cadastral_",county_name,".csv"))
          
          #   Prepping final layer and export                                         ####
          
          # Reprojeccting Layer to WGS 84 
          gap_final <- st_transform(layer_gap_cleanroads_cleancadastral, 4326) %>%  
            st_collection_extract("POLYGON") %>% 
            mutate(polygonID = paste0(county_name,"_",1:nrow(.)),
                   .before = 1) %>% 
            mutate("area_hectares" = (as.numeric(st_area(.))/10000))
          
          # Exporting final layer 
          sf_use_s2(T)
          
          st_write(gap_final,
                   dsn = paste0("1.Data/data_clean/gap_clean/gapclean_",county_name,".shp"),
                   append = F)
          
          
          
          return(data.frame(iteration = i,
                            county = county_list[[i]],
                            time = Sys.time()))
        }

###############################################################################
#   Troubleshooting incomplete cases                                        ####
#      Identifying incomplete cases                                         ####

gap_completed <- list.files("1.Data/data_clean/gap_clean",
                        full.names = F,
                        pattern = ".shp") %>% 
  str_remove("gapclean_") %>% 
  str_remove(".shp")

gap_incompleted <- setdiff(county_list,gap_completed)

#      Troubleshooting X                                                    ####

#        Subsetting Data                                                    ####

# County Name
county_name <- gap_incompleted[[1]]

# County 
county_boundary <- county_boundaries %>% 
  filter(NAME == county_name) 

# Protected Areas
county_protectedareas <- protected_areas %>% 
  st_make_valid() %>% 
  st_intersection(county_boundary) 

# Roads
county_roads <- montana_roads %>% 
  st_intersection(county_boundary)

# Cadastral 
county_cadastral <- montana_cadastral %>% 
  filter(CountyName == county_name)

#        Cleaning Gap Statuses                                              ####

GAP1 <- county_protectedareas %>%  filter(GAP_Sts == "1") 
GAP2 <- county_protectedareas %>%  filter(GAP_Sts == "2")
GAP3 <- county_protectedareas %>%  filter(GAP_Sts == "3")
GAP4 <- county_protectedareas %>%  filter(GAP_Sts == "4")

GAP1_combined <- GAP1 %>% st_combine() %>% st_make_valid()
GAP2_combined <- GAP2 %>% st_combine() %>% st_make_valid()
GAP3_combined <- GAP3 %>% st_combine() %>% st_make_valid()
GAP4_combined <- GAP4 %>% st_combine() %>% st_make_valid()

sf_use_s2(F)
GAP2_clean <- st_difference(GAP2,GAP1_combined)
GAP3_clean <- st_difference(GAP3,GAP2_combined)%>% 
  st_difference(GAP1_combined)
GAP4_clean <- st_difference(GAP4,GAP3_combined) %>% 
  st_difference(GAP2_combined) %>% 
  st_difference(GAP1_combined)
sf_use_s2(T)

GAP_layer_base <- bind_rows(GAP1,GAP2_clean,GAP3_clean,GAP4_clean) 
#        Cleaning Roads                                                     ####

layer_gap_cleanroads <- GAP_layer_base %>% st_transform(5070)
county_roads <- county_roads %>% st_transform(5070)

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("county_roads","layer_gap_cleanroads"), envir=environment())

sf_use_s2(F)

road_relevancy <- foreach(a = 1:nrow(county_roads),
                          .combine = rbind,
                          .errorhandling = "pass") %dopar% {
                            
                            rd_sub <- county_roads[a,]
                            intersect <- st_intersects(rd_sub,layer_gap_cleanroads,sparse = F)
                            
                            result <- rd_sub %>% mutate(relevant = (any(intersect) == T),
                                                        .before = 1)
                            
                            return(result)
                            
                          }

# Keeping only relevant roads
county_roads_relevant <-  filter(road_relevancy,relevant == T)

# Eliminating Roads from the GAP data

log_roads <- foreach(b = 1:nrow(county_roads_relevant),
                     .combine=rbind,
                     .errorhandling = "pass") %do% {
                       
                       road_sub <- county_roads_relevant[b,] %>% 
                         st_buffer(dist = buffer_dist) %>% 
                         st_combine() %>% 
                         st_make_valid()
                       
                       layer_gap_cleanroads <- st_difference(x = layer_gap_cleanroads,
                                                             y = road_sub)
                       
                       return(data.frame(iteration = b,
                                         time = Sys.time()))
                     }

# Exporting Foreach Log
write_csv(log_roads, 
          file = paste0("3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_",county_name,".csv"))

# Exporting Layer Temp 
sf_use_s2(T)

layer_gap_cleanroads <- st_collection_extract(layer_gap_cleanroads, "POLYGON")


#        Cleaning Cadastral                                                 ####

layer_gap_cleanroads_cleancadastral <- layer_gap_cleanroads
cadastral_exemption_shp <- county_cadastral %>% st_transform(5070)

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("cadastral_exemption_shp","layer_gap_cleanroads_cleancadastral"), envir=environment())

# Foreach Loop 
sf_use_s2(F)

cadastral_relevancy <- foreach(c = 1:nrow(cadastral_exemption_shp),
                               .combine = rbind,
                               .errorhandling = "pass") %dopar% {
                                 
                                 cad_sub <- cadastral_exemption_shp[c,]
                                 intersect <- st_intersects(cad_sub,layer_gap_cleanroads_cleancadastral,sparse = F)
                                 
                                 result <- cad_sub %>% mutate(relevant = (any(intersect) == T),
                                                              .before = 1)
                                 
                                 return(result)
                                 
                               }


# Keeping only relevant cadastral datapoints
cadastral_relevant <- filter(cadastral_relevancy,relevant == T)

# Eliminating Cadastral from the GAP data
sf_use_s2(F)

log_cadastral <- foreach(d = 1:nrow(cadastral_relevant),
                         .combine=rbind,
                         .errorhandling = "pass") %do% {
                           
                           cad_sub <- cadastral_relevant[d,] %>% 
                             st_buffer(buffer_dist) %>% 
                             st_combine() %>% 
                             st_make_valid()
                           
                           layer_gap_cleanroads_cleancadastral <- st_difference(x = layer_gap_cleanroads_cleancadastral,
                                                                                y = cad_sub)
                           
                           return(data.frame(iteration = d,
                                             time = Sys.time()))
                         }

# Exporting Foreach Log
write_csv(log_cadastral, 
          file = paste0("3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_cadastral_",county_name,".csv"))
#        Export                                                             ####

# Reprojeccting Layer to WGS 84 
gap_final <- st_transform(layer_gap_cleanroads_cleancadastral, 4326) %>%  
  st_collection_extract("POLYGON") %>% 
  mutate(polygonID = paste0(county_name,"_",1:nrow(.)),
         .before = 1) %>% 
  mutate("area_hectares" = (as.numeric(st_area(.))/10000))

# Exporting final layer 
sf_use_s2(T)

st_write(gap_final,
         dsn = paste0("1.Data/data_clean/gap_clean/gapclean_",county_name,".shp"),
         append = F)


mapview(cadastral_relevant)

###############################################################################
#   Cleaning Final Layer                                                    ####

# Importing and Joining Layer
gap_clean <- list.files("1.Data/data_clean/gap_clean",
                        full.names = T,
                        pattern = ".shp") %>% 
  str_subset("GAP_clean",negate = T) %>% 
  lapply(st_read) %>% 
  bind_rows()

# Renaming Columns 
names(gap_clean)

# Renaming and Cleaning
gap_clean <- gap_clean %>% 
  rename(polygonID = plygnID,
         area_hectares = ar_hctr,
         county = NAME) %>% 
  relocate(county,polygonID, area_hectares,GAP_Sts) %>% 
  mutate(polygonID = paste0(str_extract(polygonID,pattern = "^[^_]+(?=_)"),"_",
                            str_extract(polygonID,pattern = "(?<=_)[0-9]+") %>% str_pad(4, pad = "0")))

# Export
st_write(gap_clean,
         "1.Data/data_clean/gap_clean/GAP_clean_all.shp",
         append = F)

###############################################################################