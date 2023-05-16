
gap <- st_read(file.choose())


gap_poly <- gap %>% 
  slice_head(n = 1) %>% 
  st_transform(5070)

grid <- st_make_grid(gap_poly,
                     what = "centers",
                     cellsize = 300) %>% 
  st_sf()



mapview(grid)+mapview(gap_poly)

plot(grid)
