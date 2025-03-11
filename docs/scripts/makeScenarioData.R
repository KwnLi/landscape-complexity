# create crop boundary data
library(optimEcoServices)
library(terra)
library(sf)
library(tidyverse)

# crop boundaries
crpbdry <- sf::st_read("/Users/kevinli/Box Sync/InVEST/GIS_data/NEMWregion/hucrpbdry_437_2022ag.gpkg")

cdldir <- "/Users/kevinli/Box Sync/InVEST/GIS_data/Pareto_proj/hu_437_out/cdl/"

hu <- sf::st_read("/Users/kevinli/Box Sync/InVEST/GIS_data/NEMWregion/selected_hu12_v2_candidates.gpkg") %>%
  sf::st_cast("POLYGON")


# make hu data directories and populate
# hudir <- "/Users/kevinli/Box Sync/InVEST/Optimization/Data/maxWFS/hu/"
hudir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/hu/"
if(!dir.exists(hudir)){dir.create(hudir, recursive = TRUE)}

field.reclass <- cdl.codes %>% select(lucode, landuse_group) %>%
  mutate(landuse_group = ifelse(landuse_group=="agriculture",1,NA))

hu.names <- hu$huc12

# "out" data filepath - where the cleaned data is stored (from the "makeCleanInputData.R" script)
out.filepath <- "/Users/kevinli/Box Sync/InVEST/GIS_data/Pareto_proj/hu_437_out/"

# Parallel processing
n.cores <- parallel::detectCores() - 1

#create a cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
#register it to be used by %dopar%
library(doParallel)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()

# library(doFuture)
# registerDoFuture()  ## %dopar% parallelizes via future
# plan(multicore)     ## forked parallel processing (via 'parallel')

# for(i in 1:nrow(hu))
foreach(i = 1:nrow(hu), .packages = c("terra","sf","dplyr")) %dopar%
  {
    devtools::load_all()
  hu.name <- hu.names[i]

  # make data folder for this hu
  hudir.i <- paste0(hudir, "hu", hu.name,"/data/")
  dir.create(hudir.i, recursive=TRUE)

  # make lu folder where cdl and field margin data are
  ludir.i <- paste0(hudir.i, "lu/")
  dir.create(ludir.i)

  crp.i <- crpbdry %>% filter(huc12==hu.name)

  cdl.i <- rast(paste0(cdldir, "cdl_hu", hu.name, ".tif"))

  # make fields and field IDs
  fields.i <- make_fields(fields.poly=crp.i,
                          field.col="R22",
                          cdl.rast=cdl.i,
                          field.rcl=field.reclass,
                          section.size = 100
                          )

  # make field margins
  margins.i <- make_fieldmargins(field.id.rast=fields.i$id,
                                 field.crop.rast = fields.i$crop,
                                 id.type = "section",
                                 section.rast = fields.i$section)

  # write out rasters
  writeRaster(fields.i, paste0(ludir.i, "fields.tif"))
  writeRaster(margins.i, paste0(ludir.i, "margins.tif"))

  # add elevation data
  system(paste0("cp '", out.filepath, "elev/elev_hu", hu.name, ".tif' '",
                hudir.i, "elev.tif'"))

  # add precipitation directory
  system(paste0("cp -R '", out.filepath, "precip/precip_hu", hu.name, "' '",
                hudir.i, "precip'"))

  # add evapotranspiration directory
  system(paste0("cp -R '", out.filepath, "et0/et0_hu", hu.name, "' '",
                hudir.i, "et0'"))

  # add hydrologic group data
  system(paste0("cp '", out.filepath, "hydrogroup/hdgrp_hu", hu.name, ".tif' '",
                hudir.i, "hydrogroup.tif'"))

  # add erosivity data
  system(paste0("cp '", out.filepath, "erosivity/eros_hu", hu.name, ".tif' '",
                hudir.i, "erosivity.tif'"))

  # add erodibility data
  system(paste0("cp '", out.filepath, "erodibility2/erod_hu", hu.name, ".tif' '",
                hudir.i, "erodibility.tif'"))

  # add rain event data (it's actually precip)
  system(paste0("cp '", out.filepath, "rainevents/events_hu", hu.name, ".csv' '",
                hudir.i, "rainevents.csv'"))

  # add watershed boundary
  hu %>% filter(huc12==hu.name) %>% select(huc12) %>%
    # sf::st_cast("POLYGON") %>%
    sf::st_write(paste0(hudir.i, "watershed.gpkg"), append=FALSE)
  }

parallel::stopCluster(cl = my.cluster)

# check no data value
grep(
  "NoData Value",
  terra::describe(paste0(paste0(hudir, "hu", hu.names[1],"/data/"), "hydrogroup.tif")),
  value = TRUE
)

# write out the 'clean' cdl in INT1U format
# for(i in 1:nrow(hu)){
#
#   hu.name <- hu.names[i]
#
#   # make data folder for this hu
#   hudir.i <- paste0(hudir, "hu", hu.name,"/data/")
#
#   # make lu folder where cdl and field margin data are
#   ludir.i <- paste0(hudir.i, "lu/")
#
#   # fields.i <- rast(paste0(ludir.i, "fields.tif"))
#   #
#   # writeRaster(fields.i$cdl.clean, paste0(ludir.i, "basecdl.tif"),
#   #             datatype="INT1U", NAflag=255)
#
#   # system(paste0("rm '", ludir.i, "basecdl.tif'"))
# }

# write out a file with all watersheds
st_write(hu, "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/data/hu12watersheds.gpkg")

# write out biophys 
biophysdir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/data/biophys/"
# make biophys directory
if(!dir.exists(biophysdir)){
  dir.create(biophysdir)
}

# make pollinator biophys tables
write.csv(biophys_pol, paste0(biophysdir,"biophys_pol.csv"), row.names = FALSE)
write.csv(guild_table, paste0(biophysdir,"pol_guilds.csv"), row.names = FALSE)

# make seasonal water yield biophys tables
write.csv(biophys_swy, paste0(biophysdir,"biophys_swy.csv"), row.names = FALSE)

# make sediment delivery ratio biophys tables
write.csv(biophys_sdr$Borrelli, paste0(biophysdir,"biophys_sdr.csv"), row.names = FALSE)

# make nutrient delivery ratio biophys tables
write.csv(biophys_ndr$BenezSecanho, paste0(biophysdir,"biophys_ndr.csv"), row.names = FALSE)
