library(tidyverse)
library(psych)

wfs <- readRDS("data/maxwfsresults_20241209.rds")
# test <- readRDS("maxwfs_fullresults_20240816_v2.rds")
lsm <- read.csv("data/lsm_reduced.csv") %>% 
  mutate(hu = gsub("hu", "", hu))
lsm_all <- read.csv("data/lsm.csv") %>% 
  mutate(hu = gsub("hu", "", hu))

wfs_lsm <- wfs %>% left_join(lsm, by = "hu") %>%
  select(c(hu, delta_pollinators:delta_sed, lu_shdi:field_para_mn))

pairs.panels(wfs %>% left_join(lsm, by = "hu") %>%
               select(c(delta_pollinators:delta_sed, lu_shdi:field_para_mn)),
             histogram = TRUE, stars = TRUE, 
             method = "spearman", pch=".", cex.labels = 1.1, cex.cor = 1)

# description
library(egg)
lsm_about <- read.csv("data/lsm_description.csv")

wfs_lsm2 <- wfs_lsm %>% pivot_longer(lu_shdi:field_para_mn, names_to = "landscape.metric", values_to = "value")

ggplot(wfs_lsm2, aes(value, delta_pollinators)) + geom_point() + 
  facet_wrap(~landscape.metric, scales = "free_x") + theme_article()
ggplot(wfs_lsm2, aes(value, delta_n_export)) + geom_point() + 
  facet_wrap(~landscape.metric, scales = "free_x") + theme_article()
ggplot(wfs_lsm2, aes(value, delta_qb)) + geom_point() + 
  facet_wrap(~landscape.metric, scales = "free_x") + theme_article()
ggplot(wfs_lsm2, aes(value, delta_sed)) + geom_point() + 
  facet_wrap(~landscape.metric, scales = "free_x") + theme_article()

