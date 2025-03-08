library(terra)
library(sf)
library(tidyverse)

hu <- sf::st_read("/Users/kevinli/Box Sync/InVEST/GIS_data/NEMWregion/selected_hu12_v2_candidates.gpkg")

global.eros <- rast("/Users/kevinli/Box Sync/InVEST/GIS_data/Erosivity/GlobalR/GlobalR_NoPol.tif")

outfolder <- "/Users/kevinli/Box Sync/InVEST/GIS_data/Pareto_proj/hu_437/erosivity/"

if(!dir.exists(outfolder)){dir.create(outfolder)}

hu.buf <- st_buffer(hu, 1000)

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,] %>% sf::st_transform(crs(global.eros))
  hu.name <- hu.i$huc12

  ext.hu.i <- terra::ext(hu.i)*1.01

  hu.i.filepath <- paste0(outfolder,"eros_hu",hu.name,".tif")

  terra::crop(global.eros, ext.hu.i, snap="out", filename = hu.i.filepath)
}
