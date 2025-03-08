library(tidyverse)
library(sf)
library(terra)
# remotes::install_github("ClimateEcology/optimEcoServices")
library(optimEcoServices)

projdir <- "/Users/kevinli/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/maxWFS/"
datadir <- paste0(projdir,"data/")

hu <- st_read(paste0(datadir, "hu12watersheds.gpkg"))

hu.names <- hu$huc12

# identify medians

hu.fields <- list()
for(i in seq_along(hu.names)){
  hu.name <- hu.names[i]

  hudir <- paste0(projdir, "hu/hu",hu.name,"/")
  hu.field <- rast(paste0(hudir, "data/lu/fields", ".tif"))
  hu.fields[[hu.name]] <- as.data.frame(table(terra::values(hu.field$id, na.rm=TRUE)))
}

hu.fields.range <- bind_rows(hu.fields, .id = "hu") %>% group_by(hu) %>%
  summarize(n = n(),
            mean = mean(Freq),
            sd = sd(Freq),
            min = min(Freq),
            quant25 = quantile(Freq,0.25),
            median = median(Freq),
            quant75 = quantile(Freq,0.75),
            max = max(Freq))

median(hu.fields.range$median)

# select field IDs within a range
hu.fields.size <- bind_rows(hu.fields, .id = "hu") %>%
  filter(Freq >= 30 & Freq <= 50) %>%
  group_by(hu) %>%
  summarise(
    n.40 = n(),
    mean.40 = mean(Freq),
    median.40 = median(Freq)
  ) %>%
  left_join(hu.fields.range)

hu.fields.select <- bind_rows(hu.fields, .id = "hu") %>%
  filter(Freq >= 30 & Freq <= 50) %>% rename(id = Var1, size = Freq) %>%
  mutate(id = as.numeric(as.character(id)))

saveRDS(hu.fields.select, paste0(datadir,"selectfields.rds"))
