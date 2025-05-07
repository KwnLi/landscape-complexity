library(tidyverse)

hu_lc <- read.csv("data-raw/hu_rawlc.csv") %>%
  mutate(huc12 = as.character(paste0("0",huc12)))

lc.agg <- readRDS("data/aglu_codes.rds")

hu_lclong <- hu_lc %>% select(huc12, areasqkm, X1:X52) %>% 
  pivot_longer(cols=X1:X52, names_to = "Value", values_to = "coverage") %>%
  mutate(Value = as.numeric(gsub("X","",Value))) %>%
  mutate(coverage = replace_na(coverage,0)) %>% 
  left_join(lc.agg)

hu_pas <- hu_lclong %>% 
  group_by(huc12, CDL_name, areasqkm) %>%
  summarize(pixel.coverage = sum(coverage), .groups = "drop") %>%
  mutate(percent.coverage = 100*(pixel.coverage*900/(1000^2))/areasqkm) %>%
  filter(CDL_name == "Grass/Pasture")

write.csv(hu_pas, "pasture/pasturesummary.csv", row.names = FALSE)
