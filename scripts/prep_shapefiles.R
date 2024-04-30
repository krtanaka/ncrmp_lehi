####################################################
### subsample shp files and turn them into RData ###
####################################################
rm(list = ls())

library(maptools)
library(rgdal)
library(maps)
library(sf)

isl <- readOGR("data/5km_buffer/ALLPacific_Sectors_Islands_5km_buffer.shp")

adjust_lon_polygons_new <- function(sp_obj) {
  for (i in 1:length(sp_obj@polygons)) {
    for (j in 1:length(sp_obj@polygons[[i]]@Polygons)) {
      # Extract original coordinates
      coords <- sp_obj@polygons[[i]]@Polygons[[j]]@coords
      
      # Shift longitudes
      coords[, 1] <- ifelse(coords[, 1] < 0, coords[, 1] + 360, coords[, 1])
      
      # Assign the modified coordinates back to the polygon
      sp_obj@polygons[[i]]@Polygons[[j]]@coords <- coords
    }
  }
  
  # Recalculate and correctly assign the bounding box
  all_coords <- do.call(rbind, lapply(sp_obj@polygons, function(x) do.call(rbind, lapply(x@Polygons, function(y) y@coords))))
  sp_obj@bbox["x",] <- c(min(all_coords[, 1]), max(all_coords[, 1]))
  sp_obj@bbox["y",] <- c(min(all_coords[, 2]), max(all_coords[, 2]))
  
  return(sp_obj)
}

# Apply the function to your SpatialPolygonsDataFrame
isl <- adjust_lon_polygons_new(isl)

# isl = recenter(isl); 
plot(isl); map(add = T)
isl <- rmapshaper::ms_simplify(isl, keep = 0.001, keep_shapes = F); plot(isl); map(add = T)
isl <- isl %>% st_as_sf()
save(isl, file = 'data/isl_sf_dataframe_0.001.RData') 

eez <- readOGR(dsn = "G:/GIS/eez/World_EEZ_v11_20191118_HR_0_360", layer = "eez_v11_0_360")
eez <- rmapshaper::ms_simplify(eez, keep = 0.001, keep_shapes = F); plot(eez)
eez <- eez %>% st_as_sf()
save(eez, file = 'data/eez_sf_dataframe_0.001.RData') 
