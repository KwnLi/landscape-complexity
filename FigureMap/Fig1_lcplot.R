library(tidyverse)

hu_lc <- read.csv("data-raw/hu_rawlc.csv") %>%
  mutate(huc12 = as.character(paste0("0",huc12)))
lc.agg <- readRDS("data/aglu_codes.rds")

hu_lclong <- hu_lc %>% select(huc12, areasqkm, X1:X52) %>% 
  pivot_longer(cols=X1:X52, names_to = "Value", values_to = "coverage") %>%
  mutate(Value = as.numeric(gsub("X","",Value))) %>%
  mutate(coverage = replace_na(coverage,0)) %>% 
  left_join(lc.agg)

hu_aglu <- hu_lclong %>% 
  group_by(huc12, landuse_group, areasqkm) %>%
  summarize(pixel.coverage = sum(coverage), .groups = "drop") %>%
  mutate(percent.coverage = 100*(pixel.coverage*900/(1000^2))/areasqkm) %>%
  group_by(huc12) %>% 
  group_modify(~ .x %>% cbind(natural = .x[.x$landuse_group=="ntr",]$percent.coverage)) %>%
  ungroup() %>%
  mutate(huc12 = fct_reorder(as.factor(huc12), natural)) %>% 
  mutate(landuse_group = fct_relevel(landuse_group, 
                                     c("ann_agr", "prn_agr","wtr","dvl","non","ntr")))

# check they add up to 100. Pretty much, maybe some rounding error from areasqkm
hu_aglu_check <- hu_aglu %>% group_by(huc12) %>%
  summarize(total.percent = sum(percent.coverage), .groups = "drop")

# land cover colors
lccols <- c("ann_agr" = "#fdbf6f", "dvl" = "#bfa4a4", "ntr" = "#33a02c", 
            "wtr" = "#a6cee3", "prn_agr" = "#b2df8a", "non" = "black")

ggplot(hu_aglu, aes(huc12, percent.coverage, fill = landuse_group)) +
  geom_col(width = 1) + scale_fill_manual(values = lccols) +
  egg::theme_presentation() + ylab("Cumulative \n% coverage") + 
  xlab("Watersheds ordered by increasing % natural land cover") +
  geom_vline(xintercept = 
               which(levels(hu_aglu$huc12) %in% c("071000010602", "071401061005", "020802070704"))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    axis.line.y = element_line(colour = "gray15"),
    axis.line.x = element_line(colour = "gray15"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
    )

ggsave("FigureMap/FigureMap_lcplot.png", height = 3.5, width = 16, units = "in", bg = "white")
