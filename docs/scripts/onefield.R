library(tidyverse)
library(sf)
library(terra)
# library(future)
# library(doFuture)
# devtools::install()
library(optimEcoServices)

projdir <- "/Users/kevinli/Box Sync/InVEST/Optimization/Data/maxWFS/"
datadir <- paste0(projdir,"data/")

hu <- st_read(paste0(datadir, "hu12watersheds.gpkg"))
fields.select <- readRDS(paste0(datadir,"selectfields.rds"))

hu.names <- hu$huc12

hu.names.109 <- hu.names[1:109]
hu.names.218 <- hu.names[110:218]
hu.names.327 <- hu.names[219:327]
hu.names.437 <- hu.names[328:437]

hu.onefields <- list()

for(i in seq_along(hu.names.109)){
  hu.name <- hu.names.109[i]

  hudir <- paste0(projdir, "hu/hu",hu.name,"/")
  hu.fields <- rast(paste0(hudir, "data/lu/fields", ".tif"))
  hu.fields.select.ids <- fields.select %>%
    filter(hu == hu.name) %>% pull(id)

  # randomly draw 10 without replacement
  hu.fields.select.10 <- hu.fields.select.ids[
    sample(seq_along(hu.fields.select.ids), size = 10, replace = FALSE)
  ]

  onefields <- list()

  for(j in seq_along(hu.fields.select.10)){
    field.ij <- terra::ifel(
      test = hu.fields$id == hu.fields.select.10[j],
      yes = 1, no = NA
    )
    margin.ij <- terra::boundaries(field.ij)

    # add wfs to field j
    onefields[[j]] <- wfs_fitness2(
      margin.rast = margin.ij,
      fields.rast = hu.fields,
      margin.lc = 171,   # land cover for 'pollinator habitat'
      biophys.dir = "biophys",
      data.dir =  paste0(hudir, "data"),
      workspace.dir = paste0(hudir,"onefield"),
      suffix="onefield"
    )
  }
  hu.onefields[[i]] <- bind_rows(onefields) %>% mutate(hu = hu.name)
}

hu.109 <- bind_rows(hu.onefields)
