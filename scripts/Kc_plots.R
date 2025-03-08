library(tidyverse)
library(ggforce)
devtools::load_all()

curverefs <- read.csv("data-raw/Crop_evap_reference_FAO.csv")

swy_curves <- espareto::biophys_swy %>%
  left_join(curverefs %>% select(VALUE,Kc.reference, Kc.ref.crop, Winter.Kc.ref.crop), by = c("lucode"="VALUE")) %>%
  mutate(cropkctitle = paste0(CLASS_NAME, " (", Kc.reference, ")")) %>% rowwise() %>%
  mutate(cropkctitle2 = ifelse(Kc.reference == "Double",
                               paste0(CLASS_NAME, " [", Kc.reference, ", ", Kc.ref.crop, "/", Winter.Kc.ref.crop,"]"),
                               ifelse(Kc.ref.crop == "",
                                      paste0(CLASS_NAME, " [", Kc.reference, "]"),
                                      paste0(CLASS_NAME, " [", Kc.reference, ", ", Kc.ref.crop, "]")))) %>%
  ungroup() %>%
  pivot_longer(cols=Kc_1:Kc_12, values_to = "Kc", names_to = "Month",
               names_transform = list(Month=\(x){as.numeric(gsub("Kc_","",x))}))

i <- ceiling(length(unique(swy_curves$CLASS_NAME)) / 8) # set the number of pages

pdf("crop_Kc_curves.pdf", width = 8.5, height = 11)
lapply(seq(i), function(page) {
  SdesGG <- swy_curves %>% #launch each time or does not work
    group_by(cropkctitle2) %>% #mandatory or need to fortify
    ggplot(aes(x = Month, y = Kc)) +
    geom_line() + geom_point() +
    scale_x_continuous(breaks = 1:12) +
    coord_cartesian(ylim=c(0,1.4)) +
    facet_wrap_paginate(~cropkctitle2, ncol = 2, nrow = 4, page = page,
                        labeller = labeller(cropkctitle2 = label_wrap_gen(60))) #ggforce
})
dev.off()
graphics.off()
