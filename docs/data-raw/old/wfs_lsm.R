## code to prepare `wfs_lsm` dataset goes here

wfs <- readRDS("data/maxwfsresults_20241217_fix.rds")
lsm <- read.csv("data/lsm_reduced.csv") %>% 
  mutate(hu = gsub("hu", "", hu))

wfs_lsm <- wfs %>% left_join(lsm, by = "hu") %>%
  select(c(hu, huc2, name, margin.ha, lu_ta,
           pollinators_base:sed_export_base, 
           pollinators_max:sed_export_max, 
           delta_pollinators:delta_sed, 
           fields_max, field.ha, margin.ha, 
           lu_shdi:ntr_frac_mn)) %>%
  mutate(prn_ann_ratio = log(prn_agr_pland/ann_agr_pland),
         prn_pagr = 100*(prn_agr_pland/(ann_agr_pland+prn_agr_pland)),
         prn_ann_lograt = log(prn_agr_pland/ann_agr_pland),
         field_pland = field.ha/lu_ta,
         margin_dens = margin.ha/lu_ta,
         delta_pollinators.margin = delta_pollinators/margin_dens,
         delta_qb.margin = delta_qb/margin_dens,
         delta_n_export.margin = delta_n_export/margin_dens,
         delta_sed.margin = delta_sed/margin_dens) %>%
  mutate(habcat = cut(ntr_pland, breaks = c(-Inf,20,50,Inf),
                      labels = c("simple","moderate","natural")))

saveRDS(wfs_lsm, "data/wfs_lsm.rds")
