# make random forest from unique datasets

library(ranger)
options(ranger.num.threads = 8)

wfs <- readRDS("data/allresults_20250401.rds")

names(wfs)

selectedvar <- c("ntr_pland", "ann_agr_pland", "ntr_ed", 
                 "field_frac_mn", "prn_pagr", "prn_agr_para_mn",
                 "ann_agr_ed", "ann_agr_cohesion"
)

dat.predictors <-  wfs[, selectedvar]

dat.response <- wfs[, c(2:5, 10:13, 20:23)]

sur.dat <- cbind(dat.response, dat.predictors)

# make rf formulas
sur.pol.base.form <- reformulate(names(dat.predictors), response = "pollinators_base")
sur.pol.margin.form <- reformulate(names(dat.predictors), response = "delta_pollinators_max.margin")

sur.n_exp.base.form <- reformulate(names(dat.predictors), response = "n_export_base")
sur.n_exp.margin.form <- reformulate(names(dat.predictors), response = "delta_n_export_max.margin")

sur.qb.base.form <- reformulate(names(dat.predictors), response = "qb_base")
sur.qb.margin.form <- reformulate(names(dat.predictors), response = "delta_qb_max.margin")

sur.sed.base.form <- reformulate(names(dat.predictors), response = "sed_export_base")
sur.sed.margin.form <- reformulate(names(dat.predictors), response = "delta_sed_max.margin")

# run random forest
sur.pollinators_base.rf <- ranger(sur.pol.base.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.pollinators_base.p <- data.frame(importance_pvalues(sur.pollinators_base.rf, method = "altmann", 
                                                    formula = sur.pol.base.form, data = sur.dat, num.permutations = 1000))
sur.pollinators_margin.rf <- ranger(sur.pol.margin.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.pollinators_margin.p <- data.frame(importance_pvalues(sur.pollinators_margin.rf, method = "altmann", 
                                                      formula = sur.pol.margin.form, data = sur.dat, num.permutations = 1000))

sur.n_export_base.rf <- ranger(sur.n_exp.base.form, data = vdat, importance = "permutation", num.trees = 1000)
sur.n_export_base.p <- data.frame(importance_pvalues(sur.n_export_base.rf, method = "altmann", 
                                                 formula = sur.n_exp.base.form, sur.data = dat, num.permutations = 1000))
sur.n_export_margin.rf <- ranger(sur.n_exp.margin.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.n_export_margin.p <- data.frame(importance_pvalues(sur.n_export_margin.rf, method = "altmann", 
                                                   formula = sur.n_exp.margin.form, data = sur.dat, num.permutations = 1000))

sur.sed_export_base.rf <- ranger(sur.sed.base.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.sed_export_base.p <- data.frame(importance_pvalues(sur.sed_export_base.rf, method = "altmann", 
                                                   formula = sur.sed.base.form, data = sur.dat, num.permutations = 1000))
sur.sed_export_margin.rf <- ranger(sur.sed.margin.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.sed_export_margin.p <- data.frame(importance_pvalues(sur.sed_export_margin.rf, method = "altmann", 
                                                     formula = sur.sed.margin.form, data = sur.dat, num.permutations = 1000))

sur.qb_base.rf <- ranger(sur.qb.base.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.qb_base.p <- data.frame(importance_pvalues(sur.qb_base.rf, method = "altmann", 
                                           formula = sur.qb.base.form, data = sur.dat, num.permutations = 1000))
sur.qb_margin.rf <- ranger(sur.qb.margin.form, data = sur.dat, importance = "permutation", num.trees = 1000)
sur.qb_margin.p <- data.frame(importance_pvalues(sur.qb_margin.rf, method = "altmann",
                                             formula = sur.qb.margin.form, data = sur.dat, num.permutations = 1000))

# run random forest using different importance
pollinators_base.imp.rf <- ranger(pol.base.form, data = dat, importance = "impurity", num.trees = 1000)
pollinators_base.imp.p <- data.frame(importance_pvalues(pollinators_base.imp.rf, method = "altmann", 
                                                    formula = pol.base.form, data = dat, num.permutations = 1000))
pollinators_margin.imp.rf <- ranger(pol.margin.form, data = dat, importance = "impurity", num.trees = 1000)
pollinators_margin.imp.p <- data.frame(importance_pvalues(pollinators_margin.imp.rf, method = "altmann", 
                                                      formula = pol.margin.form, data = dat, num.permutations = 1000))

n_export_base.imp.rf <- ranger(n_exp.base.form, data = dat, importance = "impurity", num.trees = 1000)
n_export_base.imp.p <- data.frame(importance_pvalues(n_export_base.imp.rf, method = "altmann", 
                                                 formula = n_exp.base.form, data = dat, num.permutations = 1000))
n_export_margin.imp.rf <- ranger(n_exp.margin.form, data = dat, importance = "impurity", num.trees = 1000)
n_export_margin.imp.p <- data.frame(importance_pvalues(n_export_margin.imp.rf, method = "altmann", 
                                                   formula = n_exp.margin.form, data = dat, num.permutations = 1000))

sed_export_base.imp.rf <- ranger(sed.base.form, data = dat, importance = "impurity", num.trees = 1000)
sed_export_base.imp.p <- data.frame(importance_pvalues(sed_export_base.imp.rf, method = "altmann", 
                                                   formula = sed.base.form, data = dat, num.permutations = 1000))
sed_export_margin.imp.rf <- ranger(sed.margin.form, data = dat, importance = "impurity", num.trees = 1000)
sed_export_margin.imp.p <- data.frame(importance_pvalues(sed_export_margin.imp.rf, method = "altmann", 
                                                     formula = sed.margin.form, data = dat, num.permutations = 1000))

qb_base.imp.rf <- ranger(qb.base.form, data = dat, importance = "impurity", num.trees = 1000)
qb_base.imp.p <- data.frame(importance_pvalues(qb_base.imp.rf, method = "altmann", 
                                           formula = qb.base.form, data = dat, num.permutations = 1000))
qb_margin.imp.rf <- ranger(qb.margin.form, data = dat, importance = "impurity", num.trees = 1000)
qb_margin.imp.p <- data.frame(importance_pvalues(qb_margin.imp.rf, method = "altmann",
                                             formula = qb.margin.form, data = dat, num.permutations = 1000))


# save the permutation outputs
save(list = c("dat",ls(pattern = "^[npqs]")), file = "data/ES-RF3.RDA")

# save just the impurity-based outputs
# save(list = ls(pattern = "imp"), file = "data/ES-RF2-impurity.RDA")
