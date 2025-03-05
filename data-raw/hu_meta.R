## code to prepare `hu_meta` dataset goes here

datadir <- "~/Box Sync/InVEST/GIS_data/"

hu_meta <- sf::st_read(paste0(datadir,"NEMWregion/selected_hu12_v3_candidates.gpkg"))

saveRDS(hu_meta, "data/hu_meta.rds")
