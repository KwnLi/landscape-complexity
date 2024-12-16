library(landscapemetrics)
library(terra)
library(tidyverse)

datadir <- "/Users/kevinli/Box Sync/InVEST/Optimization/Data/maxWFS/"
lcgroup <- read.csv("lc_grouping.csv")

# view landscape metrics
viewlsm <- list_lsm()

hulist <- list.files(paste0(datadir,"hu"))

# aggregated classes reclass table
lc.agg <- lcgroup %>% 
  mutate(landcover_ag = abbreviate(landcover_ag,3),
         landuse_group = abbreviate(landuse_group,3)) %>%
  unite(col = "landuse_group", landcover_ag,landuse_group, na.rm = TRUE,sep = "_") %>%
  select(Value, CDL_name, landuse_group) %>%
  mutate(groupcode = as.numeric(as.factor(landuse_group)))

reclass.agg <- lc.agg %>% select(Value, groupcode)

# landscape metrics
lsm.list <- c("lsm_l_ta", "lsm_l_prd", "lsm_l_pr", "lsm_l_shdi")
lsm.agg.list <- c("lsm_l_iji", "lsm_l_frac_mn", "lsm_l_frac_cv", "lsm_l_para_mn", "lsm_l_para_cv", "lsm_l_contag", "lsm_l_ed", "lsm_l_split", "lsm_l_lsi")

# class metrics
list.agg.cm <- c("lsm_c_frac_mn", "lsm_c_frac_cv", "lsm_c_para_mn", "lsm_c_para_cv", "lsm_c_area_mn", "lsm_c_area_cv", "lsm_c_pland", "lsm_c_pd", "lsm_c_cohesion", "lsm_c_ed", "lsm_c_split", "lsm_c_lsi")

ls.result <- list()
ls.result.agg <- list()
ls.result.class <- list()
ls.check <- list()

for(i in seq_along(hulist)){
  
  hu.i <- terra::rast(paste0(datadir,"hu/",hulist[i],"/data/lu/fields.tif"))
  mask.i <- terra::vect(paste0(datadir,"hu/",hulist[i],"/data/watershed.gpkg"))
  hucrop <- terra::mask(hu.i$cdl.clean, mask.i)
  
  hucrop.agg <- hucrop %>% classify(rcl = reclass.agg)
  
  # ls.check[[hulist[i]]] <- check_landscape(hucrop)
  ls.result[[hulist[i]]] <- calculate_lsm(hucrop, what=lsm.list)
  ls.result.agg[[hulist[i]]] <- calculate_lsm(hucrop.agg, what=lsm.agg.list)
  ls.result.class[[hulist[i]]] <- calculate_lsm(hucrop.agg, what=list.agg.cm)
}

# ls.check.df <- bind_rows(ls.check, .id = "hu")

ls.result.df <- bind_rows(ls.result, .id = "hu") %>%
  pivot_wider(id_cols = "hu", names_from = "metric", values_from = "value", names_prefix = "lu_") %>% 
  left_join(bind_rows(ls.result.agg, .id = "hu") %>%
              pivot_wider(id_cols = "hu", names_from = "metric", values_from = "value", names_prefix = "aglu_")
            )

# aggregated class results
class.result.df <- bind_rows(ls.result.class, .id = "hu") %>%
  left_join(lc.agg %>% select(groupcode, landuse_group) %>% distinct(),
            by = c("class" = "groupcode")) %>%
  pivot_wider(id_cols="hu", names_from = c(landuse_group, metric), values_from = value,
              names_sep = "_")

# field metrics
field.metrics <- c("lsm_l_frac_mn", "lsm_l_frac_cv", "lsm_l_para_mn", "lsm_l_para_cv", "lsm_l_area_mn", "lsm_l_area_cv", "lsm_l_pd", "lsm_l_ed")

field.result <- list()
field.check <- list()

for(i in seq_along(hulist)){
  
  field.i <- terra::rast(paste0(datadir,"hu/",hulist[i],"/data/lu/fields.tif"))[["id"]]

  field.check[[hulist[i]]] <- check_landscape(field.i)
  field.result[[hulist[i]]] <- calculate_lsm(field.i, what=field.metrics)
}

field.check.df <- bind_rows(field.check, .id = "hu")

field.result.df <- bind_rows(field.result, .id = "hu") %>% 
  pivot_wider(id_cols = "hu", names_from = "metric", values_from = "value", names_prefix = "field_")

# crop metrics
crop.metrics <- c("lsm_l_prd", "lsm_l_pr", "lsm_l_shdi")

crop.result <- list()
crop.check <- list()

for(i in seq_along(hulist)){
  
  crop.i <- terra::rast(paste0(datadir,"hu/",hulist[i],"/data/lu/fields.tif"))[["crop"]]
  
  crop.check[[hulist[i]]] <- check_landscape(crop.i)
  crop.result[[hulist[i]]] <- calculate_lsm(crop.i, what=crop.metrics)
}

crop.check.df <- bind_rows(crop.check, .id = "hu")

crop.result.df <- bind_rows(crop.result, .id = "hu") %>% 
  pivot_wider(id_cols = "hu", names_from = "metric", values_from = "value", names_prefix = "crop_")

# combine all results
allresults <- ls.result.df %>%
  left_join(class.result.df) %>% left_join(field.result.df) %>% left_join(crop.result.df)

# write.csv(allresults, "data/lsm.csv", row.names = FALSE)
