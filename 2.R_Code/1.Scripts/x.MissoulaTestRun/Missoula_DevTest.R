#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2022-09-01 

# Purpose: This script is designed as a test run of the cell phone data subset
# 

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

# Montana Country Polygons
missoula_boundary <- st_read("1.Data/data_clean/MontanaBoundaries/MontanaCountyBoundaries.shp") %>% 
  filter(NAME == "Missoula")

# Montana Protected Areas
protected_areas <- st_read("1.Data/data_clean/Montana_ProtectedAreas/Montana_ProtectedAreas.shp") %>% 
  st_make_valid() %>% 
  st_intersection(missoula_boundary)

# Roads
missoula_roads <- read_sf("1.Data\\data_clean\\RoadLayer\\MontanaRoads.shp") %>% 
  st_intersection(missoula_boundary)

# Montana Cadastral Data
missoula_cadastral <- read_sf("1.Data\\data_clean\\MontanaCadastral\\MontanaCadastral_ParcelOwnership.shp") %>% 
  filter(CountyName == "Missoula")

###############################################################################
#   Protected Area Cleaning                                                 ####
#      Making Maps of Protected Areas                                       ####

map_GAPstatus <- ggplot(protected_areas)+
  geom_sf(data = missoula_boundary) +
  geom_sf(fill = "#458B00") +
  facet_wrap(~ GAP_Sts)+
  theme_bw()+
  ggtitle("Protected Areas by GAP status")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

ggsave(filename = "3.Outputs/MissoulaDevTest/maps/protected_areas.png",
       plot = map_GAPstatus,
       device = "png",
       width = 12,
       height = 6, 
       units = "in")

#      Eliminating Overlapping Polygons                                     ####
#        [Filtering by Status]                                              ####

GAP1 <- protected_areas %>%  filter(GAP_Sts == "1") 
GAP2 <- protected_areas %>%  filter(GAP_Sts == "2")
GAP3 <- protected_areas %>%  filter(GAP_Sts == "3")
GAP4 <- protected_areas %>%  filter(GAP_Sts == "4")

#        [Eliminating WITHIN Statuses]                                      ####

overlaps1 <- st_intersection(GAP1) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps2 <- st_intersection(GAP2) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps3 <- st_intersection(GAP3) %>% mutate(n.overlaps = as.character(n.overlaps))
overlaps4 <- st_intersection(GAP4) %>% mutate(n.overlaps = as.character(n.overlaps))

p1 <- ggplot(overlaps1)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 1")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p2 <- ggplot(overlaps2)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 2")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p3 <- ggplot(overlaps3)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 3")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

p4 <- ggplot(overlaps4)+
  geom_sf(data = missoula_boundary) +
  geom_sf(aes(fill = n.overlaps)) +
  theme_bw()+
  ggtitle("Overlapping Polygons GAP Status 4")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = round(seq(st_bbox(protected_areas)[[1]],st_bbox(protected_areas)[[3]],by = 0.5),digits = 2))

ggarrange(p1,p2,p3,p4,
          ncol = 2,nrow = 2)

#        [Eliminating BETWEEN Statuses]                                     ####

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

mapview(GAP1, col.region = "red")+ 
  mapview(GAP2, col.region = "blue")+ 
  mapview(GAP3, col.region = "yellow")+ 
  mapview(GAP4, col.region = "green")+ 
  mapview(GAP2_clean, col.region = "orange") +
  mapview(GAP3_clean, col.region = "pink") +
  mapview(GAP4_clean, col.region = "#EE30A7")

GAP_layer_final <- bind_rows(GAP1,GAP2_clean,GAP3_clean,GAP4_clean)

mapview(GAP_layer_final,zcol = "GAP_Sts")
#      Exporting Final Layer                                                ####

export_dir <- "1.Data/data_clean/Montana_ProtectedAreas/county_based"
county_name <- "Missoula"

st_write(GAP_layer_final,
         paste0(export_dir,"/",county_name,"_GAP_areas.shp"),
         append = F)

t <- st_read(paste0(export_dir,"/",county_name,"_GAP_areas.shp"))

mapview(t) + mapview(GAP_layer_final)
###############################################################################
#   GAP Status Cleaning     ]                                               ####
#      Filepaths to shapefiles                                              ####

missoula_gap_fp <- "1.Data/data_clean/Montana_ProtectedAreas/county_based/Missoula_GAP_areas.shp"
cadastral_exemption_fp <- "1.Data\\data_clean\\MontanaCadastral\\CountyBasedSubset\\MontanaCadastral_Missoula_exception.shp"

#      Eliminating Roads                                                    ####
#        Data                                                               ####

# Reprojecting Layers to EPSG
roads <- missoula_roads %>% st_transform(5070)

gap_sf <- st_read(missoula_gap_fp) %>% st_transform(5070)

layer_gap <- gap_sf

#        Eliminating Roads                                                  ####

# Eliminating Roads from the GAP data
sf_use_s2(T)

print(paste0("Start Time: ",Sys.time()))

log_roads <- foreach(i = 1:nrow(roads),
               .combine=rbind,
               .errorhandling = "pass") %do% {
                 
                 road_sub <- roads[i,] %>% 
                   st_buffer(dist = 20) %>% 
                   st_combine() %>% 
                   st_make_valid()
                 
                 layer_gap <- st_difference(x = layer_gap,
                                        y = road_sub)
                 
                 return(data.frame(iteration = i,
                                   time = Sys.time()))
               }

print(paste0("End Time: ",Sys.time()))

# Exporting Foreach Log
write_csv(log_roads, 
          "3.Outputs/MissoulaDevTest/logs_countyscripts/log_roads_Missoula.csv")

# Exporting Layer Temp 
sf_use_s2(T)

data_export_road_clean <- st_collection_extract(layer_gap, "POLYGON")

st_write(data_export_road_clean,
         "1.Data/temp_folder/layer_gap_roads.shp",
         append = F)

# Quality Check
sub_samp <- roads %>% sample_n(1000)

mapview(layer_gap) + mapview(sub_samp, color = "red") + mapview(data_export_road_clean, col.regions = "yellow")

#      Eliminating Cadastral Data                                           ####
#        Data                                                               ####

# Importing gap layer without roads 
layer_gap <- st_read("1.Data/temp_folder/layer_gap_roads.shp")

# Importing Cadastral Data 
cadastral_exemption_shp <- st_read(cadastral_exemption_fp) %>% 
  st_transform(5070)

#        Cadastral Relevancy                                                ####

# Cluster Number
cl <- makeCluster(4)
registerDoParallel(cl)

# Exporting Packages
clusterEvalQ(cl,
             {
               library(tidyverse)
               library(sf)
               library(foreach)
             })

# Exporting data to clusters
clusterExport(cl=cl, varlist=c("cadastral_exemption_shp","layer_gap"), envir=environment())


# Foreach Loop 
sf_use_s2(F)

print(paste0("Start Time: ",Sys.time()))

cadastral_relevancy <- foreach(i = 1:nrow(cadastral_exemption_shp),
                               .combine = rbind,
                               .errorhandling = "pass") %dopar% {
                                 
                                 cad_sub <- cadastral_exemption_shp[i,]
                                 intersect <- st_intersects(cad_sub,layer_gap,sparse = F)
                                 
                                 result <- cad_sub %>% mutate(relevant = (any(intersect) == T),
                                                              .before = 1)
                                 
                                 return(result)
                                 
                               }

print(paste0("End Time: ",Sys.time()))

# Exporting Layer Temp 
sf_use_s2(T)
st_write(cadastral_relevancy,
         "1.Data/temp_folder/cadastral_relevancy.shp",
         append = F)

# Quality Check
mapview(cadastral_relevancy)

#        Eliminating Cadastral                                              ####

# Keeping only relevant cadastral datapoints
cadastral_relevancy <- st_read("1.Data/temp_folder/cadastral_relevancy.shp")

cadastry_relevant <- filter(cadastral_relevancy,relevant == T)

layer_gap_road_cadastral_clean <- layer_gap

# Eliminating Cadastral from the GAP data
sf_use_s2(F)

print(paste0("Start Time: ",Sys.time()))

log_cadastral <- foreach(i = 1:nrow(cadastry_relevant),
                         .combine=rbind,
                         .errorhandling = "pass") %do% {
                           
                           cad_sub <- cadastry_relevant[i,] %>% 
                            st_buffer(dist = 20) %>% 
                             st_combine() %>% 
                             st_make_valid()
                           
                           layer_gap_road_cadastral_clean <- st_difference(x = layer_gap_road_cadastral_clean,
                                                                           y = cad_sub)
                           
                           return(data.frame(iteration = i,
                                             time = Sys.time()))
                         }

print(paste0("End Time: ",Sys.time()))

# Exporting Foreach Log
write_csv(log_cadastral, 
          "3.Outputs/MissoulaDevTest/logs_countyscripts/log_cadastral_Missoula.csv")

# Quality Check 
mapview(layer_gap_road_cadastral_clean) + 
  mapview(cadastry_relevant, col.regions = "orange") +
  mapview(layer_gap, col.regions = "blue")

# Reprojeccting Layer to WGS 84 
gap_final <- st_transform(layer_gap_road_cadastral_clean, 4326) %>% 
  st_write()

# Exporting final layer 
sf_use_s2(T)
st_write(gap_final,
         "1.Data/data_clean/gap_clean/missoula_gap_clean.shp",
         append = F)

###############################################################################
#  Exporting Maps                                                           ####
###############################################################################