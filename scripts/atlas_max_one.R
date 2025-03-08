arryindx <- as.numeric(commandArgs(trailingOnly = TRUE))

suppressMessages(library(devtools))
suppressMessages(library(tidyverse))
suppressMessages(library(sf))
suppressMessages(library(terra))
suppressMessages(library(optimEcoServices))

# projdir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/"
projdir <- "/90daydata/geoecoservices/optimization/maxWFS/"

datadir <- paste0(projdir,"data/")

# biophys directory
biophysdir <- paste0(datadir, "biophys")

# onefields
fields.select <- readRDS(paste0(datadir,"selectfields.rds"))

hu <- st_read(paste0(datadir, "hu12watersheds.gpkg"))

hu.names <- hu$huc12

batchnum <- (arryindx-1)*23+seq(1,23)

baseline <- list()
maxwfs <- list()
onefield <- list()

for(i in seq_along(batchnum)){
  i.start <- Sys.time()
  
  hu.name <- hu.names[i]
  
  cat(paste(hu.name, "started on", lubridate::as_datetime(i.start), "\n"))

  hudir <- paste0(projdir, "hu/hu",hu.name,"/")
  hu.fields <- rast(paste0(hudir, "data/lu/fields", ".tif"))
  hu.margins <- rast(paste0(hudir, "data/lu/margins", ".tif"))

  hu.marginids <- unique(terra::values(hu.margins$id, na.rm=TRUE)) # added unique() because ids are section-based, not cell based
  basemargin <- terra::ifel(test = !is.na(hu.margins$id), 0, 0)
  maxmargin <- terra::ifel(test = !is.na(hu.margins$id), 1, 0)

  # baseline
  base.start <- Sys.time()
  baseline[[hu.name]] <- wfs_fitness2(
    margin.rast = basemargin,
    fields.rast = hu.fields,
    margin.lc = 171,   # land cover for 'pollinator habitat'
    biophys.dir = biophysdir,
    data.dir =  paste0(hudir, "data"),
    workspace.dir = paste0(hudir,"baseline"),
    suffix="base"
  )
  cat(
    paste(
      "baseline took",
      round(as.numeric(difftime(Sys.time(), base.start, units="secs")), 0),
      "seconds\n"
    )
    )

  # max wfs
  maxwfs.start <- Sys.time()
  maxwfs[[hu.name]] <- wfs_fitness2(
    margin.rast = maxmargin,
    fields.rast = hu.fields,
    margin.lc = 171,   # land cover for 'pollinator habitat'
    biophys.dir = biophysdir,
    data.dir =  paste0(hudir, "data"),
    workspace.dir = paste0(hudir,"max"),
    suffix="max"
  )
  cat(
    paste(
      "maxwfs took",
      round(as.numeric(difftime(Sys.time(), maxwfs.start, units="secs")), 0),
      "seconds\n"
    )
  )
  
  # one field loop
  onefield.start <- Sys.time()
  hu.fields.select.ids <- fields.select %>%
    filter(hu == hu.name) %>% pull(id)
  
  # randomly draw 10 without replacement
  hu.fields.select.10 <- hu.fields.select.ids[
    sample(seq_along(hu.fields.select.ids), size = 10, replace = FALSE)
  ]
  
  onefields.i <- list()
  
  for(j in seq_along(hu.fields.select.10)){
    field.ij <- terra::ifel(
      test = hu.fields$id == hu.fields.select.10[j],
      yes = 1, no = NA
    )
    margin.ij <- terra::boundaries(field.ij)
    
    # add wfs to field j
    onefields.i[[j]] <- wfs_fitness2(
      margin.rast = margin.ij,
      fields.rast = hu.fields,
      margin.lc = 171,   # land cover for 'pollinator habitat'
      biophys.dir = biophysdir,
      data.dir =  paste0(hudir, "data"),
      workspace.dir = paste0(hudir,"onefield"),
      suffix="onefield"
    )
  }
  onefield[[hu.name]] <- bind_rows(onefields.i)
  
  cat(
    paste(
      "onefields took",
      round(as.numeric(difftime(Sys.time(), onefield.start, units="secs")), 0),
      "seconds\n"
    )
  )
}

saveRDS(baseline, paste0(projdir,"base-", arryindx, ".rds"))
saveRDS(maxwfs, paste0(projdir,"maxwfs-", arryindx, ".rds"))
saveRDS(onefield, paste0(projdir,"onefield-", arryindx, ".rds"))
