library(tidyverse)

datadir <- "data-raw/out_20250308/"

outfiles <- list.files(datadir)

base_out <- unlist(
  lapply(paste0(datadir,outfiles[grepl("base*", outfiles)]), readRDS),
  recursive = FALSE) %>%
  bind_rows(.id = "hu") %>%
  mutate(scenario="baseline")

max_out <- unlist(
  lapply(paste0(datadir,outfiles[grepl("max*", outfiles)]), readRDS),
  recursive = FALSE) %>%
  bind_rows(.id = "hu") %>%
  mutate(scenario="maxwfs")

one_out <- unlist(
  lapply(paste0(datadir,outfiles[grepl("onefield*", outfiles)]), readRDS),
  recursive = FALSE) %>%
  bind_rows(.id = "hu") %>%
  mutate(scenario="onefield")

# add other data
lsm <- read.csv("data/lsm_reduced.csv") %>%
  mutate(hu = gsub("hu", "", hu))
lsm2 <- read.csv("data/lsm.csv") %>%
  mutate(hu = gsub("hu", "", hu))
hu_meta <- readRDS("data/hu_meta.rds") %>% sf::st_drop_geometry()
hulandstats <- readRDS("data/hulandstats.rds")

## maximum vs baseline

maxdelta <- base_out %>% select(hu:sed_export,rawsdr_avoid_exp, field_cells) %>%
  left_join(max_out %>% select(hu:margin_cells,rawsdr_avoid_exp, margin_cells),
            by = "hu", suffix = c("_base","_max")) %>%
  mutate(
    delta_pollinators = pollinators_max - pollinators_base,
    # delta_n_ret = n_ret_max - n_ret_base,  # N retention - not used because N loading changes between scenarios so N retention is working from different assumptions
    delta_qb = qb_max - qb_base,   # change per ha
    # delta_avoidsedexp = rawsdr_avoid_exp_max - rawsdr_avoid_exp_base,  # avoided sediment export - not used because, similar to N retention, different scenarios work with different assumptions (pretty sure upslope sources of sedoiment changes)
    delta_n_export = n_export_base - n_export_max,  # change per ha
    delta_sed = sed_export_base - sed_export_max   # change per ha
  ) %>%
  select(hu, 
         pollinators_base, qb_base, n_export_base, sed_export_base, 
         pollinators_max, qb_max, n_export_max, sed_export_max,
         delta_pollinators, delta_qb, delta_n_export, delta_sed,
         field_cells, margin_cells
         ) %>%
  left_join(lsm %>% select(hu,lu_ta)) %>%
  mutate(
    field.ha = (field_cells*900)/10000,
    margin.ha = (margin_cells*900)/10000,
    margin_den = margin.ha/lu_ta) %>%
  mutate(across(delta_pollinators:delta_sed, \(x) x/margin_den, .names = "{.col}_max.margin"))  # normalize by margin density results in change in es per margin area (ws area cancels)


# one field vs baseline
#' Calculates change in ES from baseline normalized by margin area
#' Result has 4370 rows

ofdelta <- base_out %>% select(hu:sed_export,rawsdr_avoid_exp) %>%
  left_join(one_out %>% select(hu:margin_cells,rawsdr_avoid_exp),
            by = "hu", suffix = c("_base","_of")) %>%
  mutate(delta_pollinators_of = pollinators_of - pollinators_base, # calculate change from baseline (ES values are normalized by total ws area)
         delta_n_export_of = -(n_export_of - n_export_base),
         delta_qb_of = qb_of - qb_base,
         delta_sed_of = -(sed_export_of - sed_export_base)) %>%
  left_join(lsm %>% select(hu,lu_ta)) %>%
  mutate(margin.ha = (margin_cells*900)/10000,
         margin_den = margin.ha/lu_ta) %>%
  mutate(across(delta_pollinators_of:delta_sed_of, \(x) x/margin_den, .names = "{.col}.margin"))  # normalize by margin density results in change in es per margin area (ws area cancels)

# saveRDS(ofdelta, "data/of_delta.rds")

## median dataset
#' take the median ES change value of each watershed
#' use the margin-normalized values

ofdelta.md <- ofdelta %>% 
  group_by(hu) %>%
  summarize(
    across(
      c(delta_pollinators_of:delta_sed_of,
        delta_pollinators_of.margin:delta_sed_of.margin), 
           list(mean = mean, sd = sd, median = median),
           .names = "{.col}.{.fn}")
  )

# saveRDS(ofdelta.md, "data/of_deltamd.rds")

## combine all results
allresults <- maxdelta %>%
  left_join(ofdelta.md) %>%
  left_join(hu_meta %>% rename(hu=huc12)) %>%
  left_join(lsm) %>%
  # left_join(lsm2 %>% select(hu, aglu_contag, ann_agr_cohesion, ntr_cohesion, prn_agr_cohesion)) %>%
  left_join(hulandstats) %>%
  mutate(prn_ann_ratio = log(prn_agr_pland/ann_agr_pland),
         prn_pagr = 100*(prn_agr_pland/(ann_agr_pland+prn_agr_pland)),
         prn_ann_lograt = log(prn_agr_pland/ann_agr_pland),
         field_pland = field.ha/lu_ta) %>%
  mutate(habcat = cut(ntr_pland, breaks = c(-Inf,20,50,Inf),
                      labels = c("simple","moderate","natural")))

saveRDS(
  allresults, "data/allresults_20250308.rds"
)
write.csv(data.frame(colname = names(allresults)), "allresults_colnames.csv", row.names = FALSE)
