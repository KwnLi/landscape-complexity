library(devtools)
library(tidyverse)
library(sf)
library(terra)
library(optimEcoServices)

projdir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/"
datadir <- paste0(projdir,"data/")

hu <- st_read(paste0(datadir, "hu12watersheds.gpkg"))

hu.names <- hu$huc12
table(hu$huc2)


baseline <- list()
maxwfs <- list()

# make biophys directory
if(!dir.exists("biophys")){
  dir.create("biophys")
}

# make polliantor biophys tables
write.csv(biophys_pol, "biophys/biophys_pol.csv", row.names = FALSE)
write.csv(guild_table, "biophys/pol_guilds.csv", row.names = FALSE)

# make seasonal water yield biophys tables
write.csv(biophys_swy, "biophys/biophys_swy.csv", row.names = FALSE)

# make sediment delivery ratio biophys tables
write.csv(biophys_sdr$Borrelli, "biophys/biophys_sdr.csv", row.names = FALSE)

# make nutrient delivery ratio biophys tables
write.csv(biophys_ndr$BenezSecanho, "biophys/biophys_ndr.csv", row.names = FALSE)

for(i in seq_along(hu.names)){
  hu.name <- hu.names[i]

  hudir <- paste0(projdir, "hu/hu",hu.name,"/")
  hu.fields <- rast(paste0(hudir, "data/lu/fields", ".tif"))
  hu.margins <- rast(paste0(hudir, "data/lu/margins", ".tif"))

  hu.marginids <- unique(terra::values(hu.margins$id, na.rm=TRUE)) # added unique() because ids are section-based, not cell based
  basevec <- rep(0, length(hu.marginids))
  maxvec <- rep(1, length(hu.marginids))

  # baseline
  baseline[[i]] <- wfs_fitness(
    binary.vector = basevec,
    margins.rast = hu.margins,
    fields.rast = hu.fields,
    margin.lc = 171,   # land cover for 'pollinator habitat'
    biophys.dir = "biophys",
    data.dir =  paste0(hudir, "data"),
    workspace.dir = paste0(hudir,"baseline"),
    suffix="base"
  )

  # max wfs
  maxwfs[[i]] <- wfs_fitness(
    binary.vector = maxvec,
    margins.rast = hu.margins,
    fields.rast = hu.fields,
    margin.lc = 171,   # land cover for 'pollinator habitat'
    biophys.dir = "biophys",
    data.dir =  paste0(hudir, "data"),
    workspace.dir = paste0(hudir,"max"),
    suffix="max"
  )
}

# saveRDS(baseline, "/Users/kevinli/Documents/GitHub/landscape-complexity/data-raw/baseline_20241217_fix.rds")
# saveRDS(maxwfs, "/Users/kevinli/Documents/GitHub/landscape-complexity/data-raw/maxwfs_20241217_fix.rds")
