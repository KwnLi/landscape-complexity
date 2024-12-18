library(tidyverse)
library(sf)
library(terra)

projdir <- "/Users/kevinli/Box Sync/InVEST/Optimization/Data/maxWFS/"
hu <- st_read(paste0(projdir, "data/hu12watersheds.gpkg"))
regions <- st_read("/Users/kevinli/Box Sync/InVEST/GIS_data/NEMWregion/focalhuc2.gpkg")

hu.names <- hu$huc12

# previously calculted data
baseline <- readRDS("data-raw/baseline_20241217_fix.rds")
maxwfs <- readRDS("data-raw/maxwfs_20241217_fix.rds")

# combine results 
baselinedf <- bind_rows(baseline) %>% mutate(hu = hu.names)
maxwfsdf <- bind_rows(maxwfs) %>% mutate(hu=hu.names)

allresults <- baselinedf %>% left_join(maxwfsdf, by = "hu", suffix = c("_base", "_max")) %>%
  relocate(hu)

# get the total field area and margin area
total.fields <- c()
total.margin <- c()
for(i in 1:length(hu.names)){
  hu.name <- hu.names[i]
  
  hudir <- paste0(projdir, "hu/hu",hu.name,"/")
  hu.fields <- rast(paste0(hudir, "data/lu/fields", ".tif"))
  hu.margins <- rast(paste0(hudir, "data/lu/margins", ".tif"))
  
  total.fields[i] <- length(values(hu.fields$id, mat=FALSE, na.rm=TRUE))
  total.margin[i] <- length(values(hu.margins$id, mat=FALSE, na.rm=TRUE))
}

# combine all the results

huresults <- allresults %>% 
  bind_cols(field.ha = total.fields*(9/100), margin.ha=total.margin*(9/100)) %>%
  left_join(hu %>% st_drop_geometry(), by = c("hu" = "huc12")) %>% 
  mutate(delta_pollinators = pollinators_max-pollinators_base,  # change per ha
         delta_n_export = n_export_base - n_export_max,  # change per ha
         delta_qb = qb_max - qb_base,   # is an average per ha
         delta_sed = sed_export_base - sed_export_max   # change per ha
         ) %>%
  mutate(pc_chng_pollinators = 100*delta_pollinators/pollinators_base,
         pc_chng_n_export = 100*delta_n_export/n_export_base,
         pc_chng_qb = 100*delta_qb/qb_base,
         pc_chng_sed = 100*delta_sed/sed_export_base) %>% 
  left_join(regions %>% select(huc2,name) %>% st_drop_geometry(), by="huc2", 
            suffix = c("",".huc2"))

# saveRDS(huresults, "data/maxwfsresults_20241217_fix.rds")
