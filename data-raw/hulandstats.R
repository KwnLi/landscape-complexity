library(exactextractr)
library(tidyverse)
library(terra)
library(sf)

datadir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/hu/"
hu.files <- list.files(datadir)

hustats <- list()
for(i in seq_along(hu.files)){
  hu.i <- gsub("hu","",hu.files[i])
  hu.files.i <- paste0(datadir, hu.files[i],"/data/")
  ws.i <- sf::st_read(paste0(hu.files.i,"watershed.gpkg"))
  
  stats.i <- list()
  
  # elevation
  elev.i <-terra::rast(paste0(hu.files.i,"elev.tif"))
  stats.i$elev <- exactextractr::exact_extract(elev.i, ws.i, c("mean","stdev","median"))
  
  # erodibility
  erod.i <- terra::rast(paste0(hu.files.i,"erodibility.tif"))
  stats.i$erodibility <- exactextractr::exact_extract(erod.i, ws.i, c("mean","stdev","median"))
  
  # erosivity
  eros.i <- terra::rast(paste0(hu.files.i,"erosivity.tif"))
  stats.i$erosivity <- exactextractr::exact_extract(eros.i, ws.i, c("mean","stdev","median"))
  
  # et0
  et0.i <- terra::rast(list.files(paste0(hu.files.i,"et0/"), full.names = TRUE))
  stats.i$et0 <- data.frame(mean=apply(exactextractr::exact_extract(et0.i, ws.i, c("mean")),1,mean))
  
  # precip
  precip.i <- terra::rast(list.files(paste0(hu.files.i,"precip/"), full.names = TRUE))
  stats.i$precip <- data.frame(mean=apply(exactextractr::exact_extract(precip.i, ws.i, c("mean")),1,mean))
  
  # rainevents
  rain.i <- read.csv(paste0(hu.files.i,"rainevents.csv"))
  stats.i$rainevents <- data.frame(mean=mean(rain.i$events))
  
  # combine them all
  hustats[[hu.i]] <- bind_rows(stats.i, .id = "variable") %>%
    pivot_wider(names_from = "variable", values_from = c("mean", "stdev", "median"),
                names_vary = "slowest", names_glue = "{variable}_{.value}") %>%
    dplyr::select(-et0_stdev, -et0_median, -precip_stdev, -precip_median, -rainevents_stdev, -rainevents_median)
}

hustatsdf <- bind_rows(hustats, .id = "hu")

saveRDS(hustatsdf,"data/hulandstats.rds")
