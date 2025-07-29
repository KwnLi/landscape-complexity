library(exactextractr)
library(tidyverse)
library(terra)
library(sf)

datadir <- "/Users/kevinl/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/hu/"
hu.files <- list.files(datadir)

hustats <- list()
for(i in seq_along(hu.files)){
  hu.i <- gsub("hu","",hu.files[i])
  hu.files.i <- paste0(datadir, hu.files[i],"/data/")
  ws.i <- sf::st_read(paste0(hu.files.i,"watershed.gpkg"))
  
  stats.i <- list()
  
  # elevation
  elev.i <-terra::rast(paste0(hu.files.i,"elev.tif"))
  stats.i$elev <- exactextractr::exact_extract(elev.i, ws.i, c("mean","stdev","median", "min", "max"))
  
  # erodibility
  # erod.i <- terra::rast(paste0(hu.files.i,"erodibility.tif"))
  # stats.i$erodibility <- exactextractr::exact_extract(erod.i, ws.i, c("mean","stdev","median"))
  
  # erosivity
  # eros.i <- terra::rast(paste0(hu.files.i,"erosivity.tif"))
  # stats.i$erosivity <- exactextractr::exact_extract(eros.i, ws.i, c("mean","stdev","median"))
  
  # et0
  # et0.i <- terra::rast(list.files(paste0(hu.files.i,"et0/"), full.names = TRUE))
  # stats.i$et0 <- data.frame(mean=apply(exactextractr::exact_extract(et0.i, ws.i, c("mean")),1,mean))
  
  # precip
  precip.i <- terra::rast(list.files(paste0(hu.files.i,"precip/"), full.names = TRUE))
  stats.i$precip <- data.frame(mean=apply(exactextractr::exact_extract(precip.i, ws.i, c("mean")),1,mean))
  
  # rainevents
  # rain.i <- read.csv(paste0(hu.files.i,"rainevents.csv"))
  # stats.i$rainevents <- data.frame(mean=mean(rain.i$events))
  
  # combine them all
  hustats[[hu.i]] <- bind_rows(stats.i, .id = "variable") %>%
    pivot_wider(names_from = "variable", values_from = c("mean", "stdev", "median", "min", "max"),
                names_vary = "slowest", names_glue = "{variable}_{.value}") # %>%
    # dplyr::select(-et0_stdev, -et0_median, -precip_stdev, -precip_median, -rainevents_stdev, -rainevents_median)
}

hustatsdf <- bind_rows(hustats, .id = "hu")

# saveRDS(hustatsdf,"data/hulandstats2.rds")

marginstats <- list()
for(g in seq_along(hu.files)){
  hu.g <- gsub("hu","",hu.files[g])
  field.g <- terra::rast(paste0(datadir, hu.files[g],"/data/lu/fields.tif"))
  margin.g <- terra::rast(paste0(datadir, hu.files[g],"/data/lu/margins.tif"))
  
  fieldsize <- as.data.frame(table(terra::values(field.g$id))) %>% dplyr::rename(field="Var1",size="Freq")
  marginsize <- as.data.frame(table(terra::values(margin.g$field_id))) %>% dplyr::rename(field="Var1",margin="Freq")
  margin.pc <- fieldsize %>% left_join(marginsize, by = "field") %>% mutate(margin.pc = 100*margin/size) %>% pull(margin.pc)
  
  marginstats[[g]] <- data.frame(hu=hu.g, totalfield = sum(fieldsize$size), totalmargin = sum(marginsize$margin), marginpc.mn = mean(margin.pc, na.rm = TRUE), marginpc.med = median(margin.pc, na.rm = TRUE), marginpc.sd = sd(margin.pc, na.rm = TRUE), marginpc.max = max(margin.pc, na.rm = TRUE), marginpc.min = min(margin.pc, na.rm = TRUE))
}

marginstatsdf <- bind_rows(marginstats)

mean(marginstatsdf$marginpc.mn)
median(marginstatsdf$marginpc.mn)
sd(marginstatsdf$marginpc.mn)

mean(marginstatsdf$totalfield)
median(marginstatsdf$totalfield)
sd(marginstatsdf$totalfield)

mean(marginstatsdf$totalmargin)
median(marginstatsdf$totalmargin)
sd(marginstatsdf$totalmargin)

mean(marginstatsdf$totalmargin)/mean(marginstatsdf$totalfield)

# source("FigureMap/Fig1_lcplot.R")
hu_aglu2 <- hu_aglu %>%
  pivot_wider(id_cols = huc12, names_from = landuse_group, values_from = percent.coverage) %>%
  left_join(hustatsdf, by = c(huc12 = "hu"))

ggplot(hu_aglu2, aes(ntr, elev_mean)) + geom_point()
