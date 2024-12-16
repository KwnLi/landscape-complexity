library(tidyverse)
library(landscapemetrics)
library(fuzzyjoin)
library(PerformanceAnalytics)
library(psych)
library(Hmisc)

# view lHmisc# view landscape metrics
viewlsm <- list_lsm()

lsm <- read.csv("data/lsm.csv")

lsm2 <- lsm %>% select(-dvl_area_mn, -non_area_mn, -wtr_area_mn,
                       -dvl_area_cv, -non_area_cv, -wtr_area_cv,
                       -dvl_frac_mn, -non_frac_mn, -wtr_frac_mn,
                       -dvl_frac_cv, -non_frac_cv, -wtr_frac_cv,
                       -dvl_para_mn, -non_para_mn, -wtr_para_mn,
                       -dvl_para_cv, -non_para_cv, -wtr_para_cv,
                       -dvl_cohesion, -non_cohesion, -wtr_cohesion,
                       -dvl_ed, -non_ed, -wtr_ed,
                       -dvl_pd, -non_pd, -wtr_pd,
                       -dvl_lsi, -non_lsi, -wtr_lsi,
                       -dvl_pland, -non_pland, -wtr_pland,
                       -dvl_split, -non_split, -wtr_split)

names(lsm2)

# backwards engineer metric descriptors
# not pretty but I did this
lsm2.class <- data.frame(lsm = names(lsm2)[2:ncol(lsm2)]) %>% 
  separate(lsm, into = c("left", "right"), extra = "merge", sep = "_", remove = FALSE) %>%
  left_join(viewlsm, by = c("right" = "metric")) %>%
  select(lsm, left, type, right) %>% rename("lsm.metric" = "right", "landvar"="left") %>% distinct() %>% 
  separate(lsm, into = c("left", "right", "right2"), extra = "merge", sep = "_", remove = FALSE) %>%
  left_join(viewlsm, by = c("right2" = "metric"), suffix=c("",".2")) %>% 
  unite("landvar.2", c(left,right), remove = FALSE) %>%
  select(lsm, landvar, landvar.2, type, lsm.metric, left, right2, type.2) %>%
  rename("lsm.metric2" = "right2") %>% distinct() %>%
  mutate(metric = ifelse(is.na(type),lsm.metric2,lsm.metric),
         landvar = ifelse(is.na(type),landvar.2,landvar),
         type = ifelse(is.na(type),type.2,type)) %>%
  select(lsm, landvar, metric, type)

# Here are the different types of metrics
table(lsm2.class$type)
# aggregation metric area and edge metric     diversity metric         shape metric 
# 17                   16                    6                   18 

# aggregation metrics
lsm.agg <- lsm2 %>% select(lsm2.class %>% filter(type == "aggregation metric") %>% pull(lsm))

pairs.panels(lsm.agg, histogram = TRUE, stars = TRUE, method = "spearman", pch=".", cex.cor = 2)

# diversity metrics
lsm.div <- lsm2 %>% select(lsm2.class %>% filter(type == "diversity metric") %>% pull(lsm))

pairs.panels(lsm.div, histogram = TRUE, stars = TRUE, method = "spearman", pch=".", cex.cor = .8)

# area and edge metrics
lsm.area <- lsm2 %>% select(lsm2.class %>% filter(type == "area and edge metric") %>% pull(lsm))

pairs.panels(lsm.area, histogram = TRUE, stars = TRUE, method = "spearman", pch=".", cex.cor = 2)

# shape metric
lsm.shape <- lsm2 %>% select(lsm2.class %>% filter(type == "shape metric") %>% pull(lsm))

pairs.panels(lsm.shape, histogram = TRUE, stars = TRUE, method = "spearman", pch=".", cex.cor = 2)

# Define a distance function https://davetang.org/muse/2010/11/26/hierarchical-clustering-with-p-values-using-spearman/
abs.spearman.dist <- function(x, ...) {
  x <- as.matrix(x)
  res <- as.dist(1 - abs(cor(x, method = "spearman", use = "everything")))
  res <- as.dist(res)
  attr(res, "method") <- "abs. spearman"
  return(res)
}

# Make heatmaps
make.sig.mat <- function(pmat, x.90 = TRUE){
  pval <- pmat$P
  sig <- ifelse(pval<=0.05, "", "O")
  sig2 <- ifelse(pval<=0.01, "", "o")
  sig3 <- ifelse(pval<=0.001, "", ".")
  sig123 <- ifelse(sig=="", ifelse(sig2=="",sig3, sig2), sig)
  
  if(x.90){
    sig123 <- ifelse(pmat$r>.9,"X",sig123)
  }
  
  sig123
}

# heatmaps for metric types

# Aggregation metrics
lsmcor.agg.spear <- rcorr(as.matrix(lsm.agg), type = "spearman")

agg.pval <- make.sig.mat(lsmcor.agg.spear) # add significance

gplots::heatmap.2(as.matrix(abs.spearman.dist(lsm.agg)), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = agg.pval,
          notecol = "black"
)

# Diversity metrics
lsmcor.div.spear <- rcorr(as.matrix(lsm.div), type = "spearman")

div.pval <- make.sig.mat(lsmcor.div.spear) # add significance

heatmap.2(as.matrix(abs.spearman.dist(lsm.div)), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = div.pval,
          notecol = "black"
)

# area and edge metrics
lsmcor.area.spear <- rcorr(as.matrix(lsm.area), type = "spearman")

area.pval <- make.sig.mat(lsmcor.area.spear) # add significance

gplots::heatmap.2(as.matrix(abs.spearman.dist(lsm.area)), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = area.pval,
          notecol = "black"
)

# shape metrics
lsmcor.shape.spear <- rcorr(as.matrix(lsm.shape), type = "spearman")

shape.pval <- make.sig.mat(lsmcor.shape.spear) # add significance

heatmap.2(as.matrix(abs.spearman.dist(lsm.shape)), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = shape.pval,
          notecol = "black"
)

##### reduced set
lsm.red <- lsm2 %>% select(hu, lu_shdi, crop_shdi,
                           aglu_iji, lu_ta, aglu_frac_mn,
                           field_pd, field_ed, field_para_mn, field_para_cv, field_frac_mn, field_frac_cv, field_area_mn, field_area_cv,
                           prn_agr_pd, prn_agr_pland, prn_agr_ed, prn_agr_para_mn, prn_agr_frac_mn,
                           ann_agr_pd, ann_agr_pland, ann_agr_ed, ann_agr_para_mn, ann_agr_frac_mn,
                           ntr_pd, ntr_pland, ntr_ed, ntr_para_mn, ntr_frac_mn)

# write out reduced dataset
# write.csv(lsm.red, "data/lsm_reduced.csv", row.names = FALSE)

# write out variable explanation table
# write.csv(lsm2.class, "data/lsm_description.csv", row.names = FALSE)
