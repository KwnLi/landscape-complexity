## code to prepare `of_lsm` dataset
#' Joins one field results (10 per watershed) with landscape metrics dataset
#' Calculates change in ES from baseline normalized by margin area
#' Result has 4370 rows

onefield <- readRDS("data/hu_onefield.rds")

of_lsm <- onefield %>%
  left_join(wfs_lsm %>% 
              select(hu, lu_ta, pollinators_base:sed_export_base)) %>%
  mutate(margin.ha = (margin_cells*900)/10000,
         margin_den = margin.ha/lu_ta) %>%
  mutate(delta_pollinators = pollinators - pollinators_base, # calculate change from baseline (ES values are normalized by total ws area)
         delta_n_export = -(n_export - n_export_base),
         delta_qb = qb - qb_base,
         delta_sed = -(sed_export - sed_export_base)) %>%
  mutate(across(delta_pollinators:delta_sed, \(x) x/margin_den, .names = "{.col}_of.margin"))  # normalize by margin density results in change in es per margin area (ws area cancels)

saveRDS(of_lsm, "data/of_lsm.rds")

## median dataset
#' take the median ES change value of each watershed
#' use the margin-normalized values

of_lsm_margin.med <- of_lsm %>% 
  group_by(hu) %>%
  summarize(
    across(delta_pollinators_of.margin:delta_sed_of.margin, 
           list(mean = mean, sd = sd, median = median),
           .names = "{.col}.{.fn}")
  )

saveRDS(of_lsm_margin.med, "data/of_lsm_marginmed.rds")
