library(tidyverse)

aglu_colortable <- read.table("FigureMap/cdlhabitat_colortable.txt", sep = ",")
lc.agg <- readRDS("data/aglu_codes.rds")

new_colors <- data.frame(
  landuse_group = c("ann_agr", "dvl", "non","ntr", "prn_agr", "wtr"),
  r = c(253,191,0,51,178,166),
  g = c(191,164,0,160,223,206),
  b = c(111,164,0,44,138,227),
  a = c(255,255,255,255,255,255)
)

aglu_colortable_new <- aglu_colortable %>% 
  left_join(lc.agg, by = c("V1" = "Value")) %>% 
  left_join(new_colors, by = "landuse_group") %>%
  mutate(across(r:a, ~replace_na(.x,0))) %>%
  select(V1, r,g,b,a, landuse_group)

write.table(aglu_colortable_new, file = "FigureMap/aglu_colortable.txt",
            row.names = FALSE, col.names = FALSE, sep = ",")

# add the following manually to the head of the colormap for QGIS:
`
# QGIS Generated Color Map Export File
INTERPOLATION:INTERPOLATED
`

