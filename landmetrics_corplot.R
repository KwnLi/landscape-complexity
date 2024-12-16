library(tidyverse)
library(corrplot)
library(gplots)
library(Hmisc)
library(pvclust)

lsm <- read.csv("lsm.csv")
names(lsm)

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

# Shapiro Wilks test for normality
lsm.shap <- lapply(lsm2[,2:ncol(lsm2)], \(x) data.frame(statistic=shapiro.test(x)$statistic, p.value = shapiro.test(x)$p.value)) %>%
  bind_rows(.id = "metric")

lsmcor <- cor(lsm2[,2:ncol(lsm2)], method = "pearson")

# spearman correlation with p values
lsmcor.spear <- rcorr(as.matrix(lsm2[,2:ncol(lsm2)]), type = "spearman")

# plot(hclust(dist(lsmcor)))

# corrplot(cor(lsm2[,2:ncol(lsm2)]), type = "upper", order = "hclust", method = "ellipse")

# combine cluster and corrplot
# heatmap.2(lsmcor, trace = "none",
#           distfun = dist,
#           hclustfun = hclust,
#           col=cm.colors(64), margin=c(5, 10),
#           offsetCol = 0, offsetRow = 0,
#           srtCol=45,  adjCol = c(0.9,0)
#           )
# 
# heatmap.2(lsmcor.spear$r, trace = "none",
#           distfun = dist,
#           hclustfun = hclust,
#           col=cm.colors(64), margin=c(5, 10),
#           offsetCol = 0, offsetRow = 0,
#           srtCol=45,  adjCol = c(0.9,0)
# )

# using 1-abs(cor) to be distance for clustering
# https://online.stat.psu.edu/stat555/node/85/

# pvclust

# Define a distance function https://davetang.org/muse/2010/11/26/hierarchical-clustering-with-p-values-using-spearman/
abs.spearman.dist <- function(x, ...) {
  x <- as.matrix(x)
  res <- as.dist(1 - abs(cor(x, method = "spearman", use = "everything")))
  res <- as.dist(res)
  attr(res, "method") <- "abs. spearman"
  return(res)
}

lsm.clust <- hclust(abs.spearman.dist(lsm2[,2:ncol(lsm2)]), method = "complete")

lsm.bootclust <- pvclust(lsm2[,2:ncol(lsm2)], method.dist=abs.spearman.dist, method.hclust="complete", nboot=10000, parallel=TRUE)

# plot the clusters
plot(lsm.bootclust)
pvrect(lsm.bootclust, alpha = 0.95)

seplot(lsm.bootclust, identify = TRUE)

lsmplot <- heatmap.2(as.matrix(abs.spearman.dist(lsm2[,2:ncol(lsm2)])), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0)
)

# add significance
lsm.pval <- lsmcor.spear$P
lsm.sig <- ifelse(lsm.pval<=0.05, "", "O")
lsm.sig2 <- ifelse(lsm.pval<=0.01, "", "o")
lsm.sig3 <- ifelse(lsm.pval<=0.001, "", ".")

lsm.sig12 <- ifelse(lsm.sig=="", ifelse(lsm.sig2=="",lsm.sig3, lsm.sig2), lsm.sig)

heatmap.2(as.matrix(abs.spearman.dist(lsm2[,2:ncol(lsm2)])), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = lsm.sig12,
          notecol = "black"
)


# reduce by clusters
lsm3 <- lsm2 %>% select(
  -field_pd, -field_para_cv, -ann_agr_area_mn, -ann_agr_split, -ntr_cohesion, -ntr_split,
  -aglu_lsi, -prn_agr_lsi, -aglu_split, -prn_agr_cohesion, -prn_agr_split,
  -prn_agr_para_cv, -ann_agr_para_cv, -ann_agr_frac_mn, -aglu_para_cv, -aglu_para_mn,
  -ann_agr_frac_cv, -ntr_para_cv, -ntr_para_mn, -ntr_area_mn,
  -lu_pr, -crop_pr, -aglu_frac_cv, -ntr_lsi
)

lsm3.bootclust <- pvclust(lsm3[,2:ncol(lsm3)], method.dist=abs.spearman.dist, method.hclust="complete", nboot=10000, parallel=TRUE)
plot(lsm3.bootclust)
pvrect(lsm3.bootclust, alpha = 0.95)

# plot again, with significance

# add significance
lsm3cor.spear <- rcorr(as.matrix(lsm3[,2:ncol(lsm3)]), type = "spearman")

lsm3.pval <- lsm3cor.spear$P
lsm3.sig <- ifelse(lsm3.pval<=0.05, "", "O")

heatmap.2(as.matrix(abs.spearman.dist(lsm3[,2:ncol(lsm3)])), trace = "none",
          distfun = as.dist,
          hclustfun = function(d) hclust(d, method = "complete"),
          col=cm.colors(64), margin=c(5, 10),
          offsetCol = 0, offsetRow = 0,
          srtCol=45,  adjCol = c(0.9,0),
          cellnote = lsm3.sig,
          notecol = "black"
)
