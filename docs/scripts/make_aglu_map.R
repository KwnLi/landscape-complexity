library(terra)

lc.agg <- readRDS("data/aglu_codes.rds")
lc.agg[which(lc.agg$Value==0),"groupcode"] <- NA

nemw <- rast("/Users/kevinl/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/NEMWregion/focalarea_cdl.tif")

# active category to first column (value)
activeCat(nemw) <- 0

nemw_aglu <- classify(nemw, lc.agg[,c(1,4)])

# create a new aggregated land use table
aglu <- lc.agg %>% select(groupcode, landuse_group) %>% distinct() %>% filter(!is.na(groupcode))

levels(nemw_aglu) <- aglu

writeRaster(nemw_aglu, 
            "/Users/kevinl/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/GIS/NEMWregion/focalarea_rcl.tif")
