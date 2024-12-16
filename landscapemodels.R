library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(performance)
library(piecewiseSEM)
library(psych)
source("R/utils-cor.R")

wfs <- readRDS("data/maxwfsresults_20241209.rds")
lsm <- read.csv("data/lsm_reduced.csv") %>% 
  mutate(hu = gsub("hu", "", hu))

wfs_lsm <- wfs %>% left_join(lsm, by = "hu") %>%
  select(c(hu, huc2, name, margin.ha, lu_ta,
           pollinators_base:sed_export_base, 
           delta_pollinators:delta_sed, 
           fields_max, field.ha, margin.ha, 
           lu_shdi:ntr_frac_mn)) %>%
  mutate(prn_ann_ratio = log(prn_agr_pland/ann_agr_pland),
         prn_pagr = 100*(prn_agr_pland/(ann_agr_pland+prn_agr_pland)),
         field_pland = field.ha/lu_ta,
         margin_dens = margin.ha/lu_ta)

scaled.data <- wfs_lsm %>% mutate(across(lu_shdi:field_pland, ~scale(.x)[,1]))

# % land area relationships
lsm.area <- wfs_lsm %>% 
  select(ntr_ed,
         ann_agr_ed,
         prn_agr_ed,
         margin_dens,
         ntr_pland,
         ann_agr_pland,
         prn_agr_pland,
         field_pland)

pairs.panels(lsm.area, cex.cor = 1.2, cex.labels=0.7, cex=0.5)

pairs.panels( wfs_lsm %>% 
                select(pollinators_base, delta_pollinators, field_frac_mn, field.ha, ntr_pland, field_ed), cex.cor = 1.2, cex.labels=0.7, cex=0.5)

# shape relationships
lsm.shape <- wfs_lsm %>% 
  select(ntr_pland, ann_agr_pland, field.ha, prn_agr_pland, 
         aglu_frac_mn,
         ntr_frac_mn,
         ann_agr_frac_mn,
         prn_agr_frac_mn, 
         field_frac_mn)
pairs.panels(lsm.shape, cex.cor = 1, cex.labels=1)

speardist.heatmap(lsm.shape)

# aggregation relationships
lsm.agg <- wfs_lsm %>% 
  select(aglu_iji,ntr_pd, ann_agr_pd, prn_agr_pd, field_pd)

pairs.panels(lsm.agg, cex.cor = 1, cex.labels=1)

speardist.heatmap(lsm.agg)

# field relationships
pairs.panels(wfs_lsm %>% 
               select(ntr_pland, ann_agr_pland, prn_agr_pland,
                      field_para_mn,
                      field_frac_mn,
                      field_area_mn,
                      margin_dens), cex.cor = 1, cex.labels=1)

# independent variable relationships
pairs.panels(wfs_lsm %>% 
               select(ntr_pland, prn_agr_pland,
                      aglu_iji, lu_shdi, crop_shdi,
                      field_frac_mn, field_ed), cex.cor = 1, cex.labels=1)

# test models
mod0 <- lm(pollinators_base ~ ntr_pland + aglu_iji + prn_agr_pland, data = scaled.data)
mod1 <- lm(pollinators_base ~ ntr_pland + aglu_iji + prn_agr_pland + field_frac_mn + field_ed + lu_shdi, data = scaled.data)
mod2 <- lm(pollinators_base ~ ntr_pland * aglu_iji + prn_agr_pland + field_frac_mn + field_ed + lu_shdi, data = scaled.data)
mod3 <- lm(pollinators_base ~ ntr_pland * field_ed + aglu_iji + prn_agr_pland + field_frac_mn + lu_shdi, data = scaled.data)
mod4 <- lm(pollinators_base ~ ntr_pland * lu_shdi + aglu_iji + prn_agr_pland + field_frac_mn + field_ed, data = scaled.data)
mod5 <- lm(pollinators_base ~ ntr_pland * prn_agr_pland + lu_shdi + aglu_iji + field_frac_mn + field_ed, data = scaled.data)
mod6 <- lm(pollinators_base ~ ntr_pland * field_frac_mn + prn_agr_pland + lu_shdi + aglu_iji + field_ed, data = scaled.data)
mod7 <- lm(pollinators_base ~ ntr_pland * crop_shdi + aglu_iji + prn_agr_pland + field_frac_mn + field_ed, data = scaled.data)

performance::compare_performance(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7)
performance::check_model(mod4)
summary(mod4)

dmod0 <- lm(delta_pollinators ~ ntr_pland + aglu_iji + prn_agr_pland, data = scaled.data)
dmod1 <- lm(delta_pollinators ~ ntr_pland + aglu_iji + prn_agr_pland + field_frac_mn + field_ed + lu_shdi, data = scaled.data)
dmod2 <- lm(delta_pollinators ~ ntr_pland * aglu_iji + prn_agr_pland + field_frac_mn + field_ed + lu_shdi, data = scaled.data)
dmod3 <- lm(delta_pollinators ~ ntr_pland * field_ed + aglu_iji + prn_agr_pland + field_frac_mn + lu_shdi, data = scaled.data)
dmod4 <- lm(delta_pollinators ~ ntr_pland * lu_shdi + aglu_iji + prn_agr_pland + field_frac_mn + field_ed, data = scaled.data)
dmod5 <- lm(delta_pollinators ~ ntr_pland * prn_agr_pland + aglu_iji + field_frac_mn + field_ed + lu_shdi, data = scaled.data)
dmod6 <- lm(delta_pollinators ~ ntr_pland * field_frac_mn + prn_agr_pland + aglu_iji + field_ed + lu_shdi, data = scaled.data)

performance::compare_performance(dmod0, dmod1, dmod2, dmod3, dmod4, dmod5, dmod6)

performance::check_model(dmod2)
summary(dmod2)
plot(simulateResiduals(mmod2))

# models
pollmod <- glmmTMB(pollinators_base ~ ntr_pland*aglu_iji + ntr_pland*prn_ann_agr + (1|huc2),
                   data = scaled.data)
pollmod.delta <- glmmTMB(delta_pollinators ~ ntr_pland*aglu_iji + ntr_pland*prn_ann_agr + (1|huc2),
                   data = scaled.data)
# model system
esmod <- glmmTMB(pollinators_base ~ntr_pland + ann_agr_pland + prn_agr_pland + (1|huc2),
                 data = scaled.data)
esdltmod <- glmmTMB(delta_pollinators ~ pollinators_base + field_para_mn + aglu_iji + (1|huc2),
                 data = scaled.data)

# piecewiseSEM models
library(piecewiseSEM)
pollinator_sem <- psem(
  lm(pollinators_base ~ ntr_pland + prn_agr_pland, 
     data = scaled.data),
  lm(delta_pollinators ~ pollinators_base + field_area_mn + field_frac_mn, 
     data = scaled.data),
  scaled.data
)
summary(pollinator_sem)
