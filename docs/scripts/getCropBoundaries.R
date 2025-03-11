library(doParallel)
library(tidyverse)
library(sf)
library(terra)

datadir <- "~/Box Sync/InVEST/GIS_data/"

cropbdry <- paste0(datadir,  "Crop boundaries/2022_National_CSB_gdb/CSB1522.gdb")
hu <- st_read(paste0(datadir,"NEMWregion/selected_hu12_v2_candidates.gpkg"))

# project to match crop boundry data
hu.aea <- st_transform(hu, cropbdry.crs)

# setup parallel

parallel::detectCores()

cl <- parallel::makeCluster(
  8, type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cl)

#check if it is registered (optional)
foreach::getDoParRegistered()

# loop through
focalhu.cropbdry <- foreach(i = 1:nrow(hu.aea)) %dopar% {
  hu.i <- hu.aea[i,]

  query_cropbdry(hu.i, cropbdry.path = cropbdry)
}

names(focalhu.cropbdry) <- hu.aea %>% pull(huc12)

hucrp <- bind_rows(focalhu.cropbdry, .id = "huc12")

st_write(hucrp, paste0(datadir,"NEMWregion/hucrpbdry_437.gpkg"))

hucrp_ag22 <- hucrp %>%
  left_join(cdl.codes %>% select(lucode, CLASS_NAME, landuse_group, landcover_ag), by = c("R22"="lucode")) %>%
  filter(landuse_group == "agriculture")

st_write(hucrp_ag22, paste0(datadir,"NEMWregion/hucrpbdry_437_2022ag.gpkg"), append=FALSE)
