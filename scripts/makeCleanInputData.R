# clean up file formats of input data
# reproject and make the same resolution
# mask all data by the watershed boundaries
# note about cdl: I am worried that there may be a boundary effect if the
# landcover layer is cut off right at the watershed boundary

library(terra)
library(sf)
library(tidyverse)

hu <- sf::st_read("/Users/kevinli/Box Sync/InVEST/GIS_data/NEMWregion/selected_hu12_v2_candidates.gpkg")
hu.buf <- st_buffer(hu, 1000)

raw.filepath <- "/Users/kevinli/Box Sync/InVEST/GIS_data/Pareto_proj/hu_437/"
out.filepath <- "/Users/kevinli/Box Sync/InVEST/GIS_data/Pareto_proj/hu_437_out/"

if(!dir.exists(out.filepath)){dir.create(out.filepath)}

# make cdl folder if it doesn't exist
cdl.out <- paste0(out.filepath, "cdl/")
if(!dir.exists(cdl.out)){dir.create(cdl.out)}

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  # make cdl mask
  cdl.i <- rast(paste0(raw.filepath,"cdl/cdl_hu",hu.name,".tif"))
  if(!same.crs(cdl.i, hu.i)){stop()}  # should be same crs. This is to stop the process if not, for double checking

  terra::mask(cdl.i, vect(hu.i),
              filename=paste0(cdl.out, "cdl_hu", hu.name, ".tif"),
              datatype="INT1U",
              NAflag = 255, overwrite=TRUE)
}

# test no data values
for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  test <- grep(
    "NoData Value",
    terra::describe(paste0(cdl.out, "cdl_hu", hu.name, ".tif")),
    value = TRUE
  )
  print(test)
}

##### rest of data #####

# huc names
hu.names <- hu.buf$huc12

# erodibility
erod.out <- paste0(out.filepath, "erodibility2/")
if(!dir.exists(erod.out)){dir.create(erod.out)}

for(i in 1:length(hu.names)){
  hu.name <- hu.names[i]

  # get the cdl mask
  cdl.mask <- rast(paste0(cdl.out, "cdl_hu", hu.name, ".tif"))

  # erodibility
  erod.i <- rast(paste0(raw.filepath,"erodibility2/erod_hu",hu.name,".tif"))

  # multiply by conversion per user manual and: https://community.naturalcapitalproject.org/t/soil-erodibility-in-gis/1923/2
  erod.i.conv <- erod.i * 0.1317

  # replace NA values with 0, per: http://www.soilinfo.psu.edu/index.cgi?soil_data&conus&data_cov&k&methods
  erod.i.noNA <- subst(erod.i.conv, NA, 0)

  project(erod.i.noNA, cdl.mask, method="near") %>%
    terra::mask(mask=cdl.mask,
                filename=paste0(erod.out, "erod_hu",hu.name,".tif"),
                overwrite=TRUE, datatype="FLT4S", NAflag=-9999)

  test <- grep(
    "NoData Value",
    terra::describe(paste0(erod.out, "erod_hu",hu.name,".tif")),
    value = TRUE
  )
  print(paste0("erodibility", test))
}

elev.out <- paste0(out.filepath, "elev/")
if(!dir.exists(elev.out)){dir.create(elev.out)}

eros.out <- paste0(out.filepath, "erosivity/")
if(!dir.exists(eros.out)){dir.create(eros.out)}

et0.out <- paste0(out.filepath, "et0/")
if(!dir.exists(et0.out)){dir.create(et0.out)}

prec.out <- paste0(out.filepath, "precip/")
if(!dir.exists(prec.out)){dir.create(prec.out)}

for(i in 1:length(hu.names)){
  hu.name <- hu.names[i]

  # get the cdl mask
  cdl.mask <- rast(paste0(cdl.out, "cdl_hu", hu.name, ".tif"))

  # elevation
  elev.i <- rast(paste0(raw.filepath,"elev/elev_hu",hu.name,".tif"))
  project(elev.i, cdl.mask, method="near") %>%
    terra::mask(mask=cdl.mask,
                filename=paste0(elev.out, "elev_hu",hu.name,".tif"),
                overwrite=TRUE, datatype="FLT4S", NAflag=-9999)

  # erosivity
  eros.i <- rast(paste0(raw.filepath,"erosivity/eros_hu",hu.name,".tif"))
  project(eros.i, cdl.mask, method="near") %>%
    terra::mask(mask=cdl.mask,
                filename=paste0(eros.out, "eros_hu",hu.name,".tif"),
                overwrite=TRUE, datatype="FLT4S", NAflag=-9999)

  # et0
  et0.files <- list.files(paste0(raw.filepath,"et0/et0_hu",hu.name))
  et0.out.i <- paste0(et0.out, "et0_hu",hu.name,"/")
  if(!dir.exists(et0.out.i)){dir.create(et0.out.i)}

  for(g in 1:length(et0.files)){
    et0.g <- rast(paste0(raw.filepath,"et0/et0_hu",hu.name,"/",et0.files[g]))
    project(et0.g, cdl.mask, method="near") %>%
      terra::mask(mask=cdl.mask,
                  filename=paste0(et0.out.i,et0.files[g]),
                  datatype="FLT4S", NAflag=-9999)
  }

  # precip
  prec.files <- list.files(paste0(raw.filepath,"precip/precip_hu",hu.name))
  prec.out.i <- paste0(prec.out, "precip_hu",hu.name,"/")
  if(!dir.exists(prec.out.i)){dir.create(prec.out.i)}

  for(g in 1:length(prec.files)){
    prec.g <- rast(paste0(raw.filepath,"precip/precip_hu",hu.name,"/",prec.files[g]))
    project(prec.g, cdl.mask, method="near") %>%
      terra::mask(mask=cdl.mask,
                  filename=paste0(prec.out.i,prec.files[g]),
                  datatype="FLT4S", NAflag=-9999)
  }

  # check no data values

  if(i==1){

    test <- grep(
      "NoData Value",
      terra::describe(paste0(elev.out, "elev_hu",hu.name,".tif")),
      value = TRUE
    )
    print(paste0("elevation", test))

    test <- grep(
      "NoData Value",
      terra::describe(paste0(eros.out, "eros_hu",hu.name,".tif")),
      value = TRUE
    )
    print(paste0("erosivity", test))

    test <- grep(
      "NoData Value",
      terra::describe(paste0(et0.out, "et0_hu",hu.name,"/",et0.files[g])),
      value = TRUE
    )
    print(paste0("et0", test))

    test <- grep(
      "NoData Value",
      terra::describe(paste0(prec.out, "precip_hu",hu.name,"/",prec.files[g])),
      value = TRUE
    )
    print(paste0("precipitation", test))
  }
}

# hydrogroup

hdgrp.out <- paste0(out.filepath, "hydrogroup/")
if(!dir.exists(hdgrp.out)){dir.create(hdgrp.out)}

hydgrp.rcltab <- data.frame(
  hydgrpdcd = c("A", "A/D", "B", "B/D", "C", "C/D", "D"),
  new.value = c(1, 1, 2, 2, 3, 3, 4)
)
for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  # get the cdl mask
  cdl.mask <- rast(paste0(cdl.out, "cdl_hu", hu.name, ".tif"))

  # hydrogroup
  hdgrp.i <- rast(paste0(raw.filepath,"hydrogroup/hdgrp_hu",hu.name,".tif"))

  hdgrp.rcltab.i <- cats(hdgrp.i)[[1]] %>% left_join(hydgrp.rcltab,
                                                     by="hydgrpdcd")

  hdgrp.reclass.i <- classify(hdgrp.i,
                              rcl=hdgrp.rcltab.i[,c("value","new.value")])

  project(hdgrp.reclass.i, cdl.mask, method="near") %>%
    terra::mask(mask=cdl.mask,
                filename=paste0(hdgrp.out, "hdgrp_hu",hu.name,".tif"),
                overwrite=TRUE,
                datatype="INT1U", NAflag=255)

  test <- grep(
    "NoData Value",
    terra::describe(paste0(hdgrp.out, "hdgrp_hu",hu.name,".tif")),
    value = TRUE
  )
  print(paste0("hydrogroup", test))
}

# precip (rain) events
hu.names <- hu.buf$huc12

events.out <- paste0(out.filepath, "rainevents/")
if(!dir.exists(events.out)){dir.create(events.out)}

for(i in 1:length(hu.names)){
  events.i <- read.csv(paste0(raw.filepath, "rainevents/events_hu", hu.names[i],".csv")) %>%
    rename(events="prdays")
  write.csv(events.i, paste0(out.filepath, "rainevents/events_hu",hu.names[i],".csv"), row.names = FALSE)
}
