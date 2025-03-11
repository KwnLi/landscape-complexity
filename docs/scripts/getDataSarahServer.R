# must be run from home directory /home/kevin on Sarah's server!

library(EnvToolbox, lib.loc="/home/kevin/Rlib")
library(sf)
# library(terra)

# hu <- sf::st_read("/home/kevin/selected_hu12_v2.gpkg")
hu <- st_read("/home/kevin/selected_hu12_v2_candidates.gpkg")
hu.buf <- st_buffer(hu, 1000)

# parent directory
parentdir <- "/home/kevin/gisdata/hu_437/"

dir.create(parentdir, recursive = TRUE)

# extract erodibility
dir.create(paste0(parentdir, "erodibility2"))

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  hu.erod <- querySoils(dat=hu.i,
                        return.grids = TRUE,
                        index = "gssurgo",
                        ssurgo.table = "chorizon",
                        soil.layers = "kffact", # k-factor (erodibility)
                        horizon.depth = 30,     # depth-weighted mean down to 10cm
                        verbose=FALSE)

  terra::writeRaster(hu.erod, paste0(parentdir, "erodibility2/erod_hu",
                              hu.name,".tif"))
}

# extract hydrogroup
dir.create(paste0(parentdir, "hydrogroup"))

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  hu.hdgrp <- querySoils(dat=hu.i,
                        return.grids = TRUE,
                        index = "gssurgo",
                        ssurgo.table = "muaggatt",
                        soil.layers = "hydgrpdcd",  # dominant hydrogroup
                        verbose=FALSE)

  terra::writeRaster(hu.hdgrp, paste0(parentdir, "hydrogroup/hdgrp_hu",
                                     hu.name,".tif"))
}

# extract  cdl landcover
dir.create(paste0(parentdir, "cdl"))

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  hu.cdl <- queryLandcover(dat=hu.i,
                            yr = 2022, index = "CDL",
                            return.grids = TRUE,
                            verbose=FALSE)

  terra::writeRaster(hu.cdl, paste0(parentdir, "cdl/cdl_hu",
                                     hu.name,".tif"))
}

# extract topo
dir.create(paste0(parentdir, "elev"))

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  hu.topo <- queryTopo(dat=hu.i,
                       return.grids = TRUE,
                       verbose=FALSE)

  terra::writeRaster(hu.topo$elev3dep, paste0(parentdir, "elev/elev_hu",
                                    hu.name,".tif"))
}

# extract climate data
date.range <- readRDS(file = "/home/kevin/ClimateDateRangeTable.rds")

# monthly precipitation events
dir.create(paste0(parentdir, "rainevents"))

for(i in 1:nrow(hu)){
  hu.i <- hu[i,]
  hu.name <- hu.i$huc12

  grid.data <- getGrid_climate(vct=hu.i,
                               from.when = "1992-01-01", to.when = "2021-12-31",
                               index = "gridMET", period = "daily",
                               by=by, verbose=FALSE, showPB = TRUE)

  grid.data.sum <- grid.data[[2]] %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           day = format(date, "%d")) %>%
    filter(!(month == "02" & day == "29")) %>%  # filter out leap year days
    dplyr::select(grid.ids, year, month, day, pr, tmin) %>%
    mutate(rain = pr>0 & tmin>0) %>%  # precipitation is greater than 0 AND min temp is greater than 0
    group_by(grid.ids, year, month) %>%
    summarize(prdays = sum(rain), .groups="drop") %>%
    group_by(year, month) %>%
    summarize(prdays = mean(prdays), .groups="drop") %>%
    group_by(month) %>%
    summarize(prdays = mean(prdays), .groups="drop") %>%
    mutate(month = as.numeric(month))

  write.csv(grid.data.sum,
            paste0(parentdir, "rainevents/events_hu",hu.name,".csv"),
            row.names = FALSE)
}

# monthly ET0 and precipitation
dir.create(paste0(parentdir, "et0"))
dir.create(paste0(parentdir, "precip"))

for(i in 1:nrow(hu)){
  hu.i <- hu.buf[i,]
  hu.name <- hu.i$huc12

  grid.data <- getGrid_climate(vct=hu.i,
                               from.when = "1992-01-01", to.when = "2021-12-31",
                               index = "gridMET", period = "daily",
                               by=by, verbose=FALSE, showPB = TRUE)

  # et0

  et0.sum <- grid.data[[2]] %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           day = format(date, "%d")) %>%
    filter(!(month == "02" & day == "29")) %>%  # filter out leap year days
    dplyr::select(grid.ids, year, month, day, etr) %>%
    group_by(grid.ids, year, month) %>%
    summarize(etr = sum(etr), .groups="drop") %>%
    group_by(grid.ids, month) %>%
    summarize(etr = mean(etr), .groups="drop") # take means across years

  et0.wide <- et0.sum %>%
    pivot_wider(id_cols = grid.ids,
                names_from = "month",
                names_sep="_",
                values_from = "etr") %>% as.data.frame()

  et0.return <- grid2data(grid.data[[1]], et0.wide, "grid.ids",
                          data.cols = names(et0.wide %>% dplyr::select(-grid.ids)))

  et0.names <- names(et0.return)

  # precip
  precip.sum <- grid.data[[2]] %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           day = format(date, "%d")) %>%
    filter(!(month == "02" & day == "29")) %>%  # filter out leap year days
    dplyr::select(grid.ids, year, month, day, pr) %>%
    group_by(grid.ids, year, month) %>%
    summarize(pr = sum(pr), .groups="drop") %>%
    group_by(grid.ids, month) %>%
    summarize(pr = mean(pr), .groups="drop") # take mean across years

  precip.wide <- precip.sum %>%
    pivot_wider(id_cols = grid.ids,
                names_from = "month",
                names_sep="_",
                values_from = "pr") %>% as.data.frame()

  precip.return <- grid2data(grid.data[[1]], precip.wide, "grid.ids",
                          data.cols = names(precip.wide %>% dplyr::select(-grid.ids)))

  precip.names <- names(precip.return)

  et0dir.i <- paste0(parentdir, "et0/et0_hu", hu.name)
  if(!dir.exists(et0dir.i)){dir.create(et0dir.i)}

  precipdir.i <- paste0(parentdir, "precip/precip_hu", hu.name)
  if(!dir.exists(precipdir.i)){dir.create(precipdir.i)}

  for(g in 1:length(et0.names)){
    et0.g <- et0.return[[et0.names[g]]]
    et0.filename.g <- paste0("et0_", as.numeric(et0.names[g])) # make output filename

    precip.g <- precip.return[[precip.names[g]]]
    precip.filename.g <- paste0("precip_", as.numeric(precip.names[g])) # make output filename

    terra::writeRaster(et0.g, paste0(et0dir.i,"/",et0.filename.g,".tif"))
    terra::writeRaster(precip.g, paste0(precipdir.i,"/",precip.filename.g,".tif"))
  }
}

