# make random forest from unique datasets

library(ranger)

wfs <- readRDS("data/allresults_20250308.rds")

names(wfs)

dat.predictors <-  wfs[, c(81:107,120,123)]

dat.response <- wfs[, c(2:5, 10:13, 20:23)]

dat <- cbind(dat.response, dat.predictors)

# make rf formulas
pol.base.form <- reformulate(names(dat.predictors), response = "pollinators_base")
pol.margin.form <- reformulate(names(dat.predictors), response = "delta_pollinators_max.margin")

n_exp.base.form <- reformulate(names(dat.predictors), response = "n_export_base")
n_exp.margin.form <- reformulate(names(dat.predictors), response = "delta_n_export_max.margin")

qb.base.form <- reformulate(names(dat.predictors), response = "qb_base")
qb.margin.form <- reformulate(names(dat.predictors), response = "delta_qb_max.margin")

sed.base.form <- reformulate(names(dat.predictors), response = "sed_export_base")
sed.margin.form <- reformulate(names(dat.predictors), response = "delta_sed_max.margin")

# run random forest
pollinators_base.rf <- ranger(pol.base.form, data = dat, importance = "permutation", num.trees = 1000)
pollinators_base.p <- data.frame(importance_pvalues(pollinators_base.rf, method = "altmann", 
                                                    formula = pol.base.form, data = dat, num.permutations = 1000))
pollinators_margin.rf <- ranger(pol.margin.form, data = dat, importance = "permutation", num.trees = 1000)
pollinators_margin.p <- data.frame(importance_pvalues(pollinators_margin.rf, method = "altmann", 
                                                      formula = pol.margin.form, data = dat, num.permutations = 1000))

n_export_base.rf <- ranger(n_exp.base.form, data = dat, importance = "permutation", num.trees = 1000)
n_export_base.p <- data.frame(importance_pvalues(n_export_base.rf, method = "altmann", 
                                                 formula = n_exp.base.form, data = dat, num.permutations = 1000))
n_export_margin.rf <- ranger(n_exp.margin.form, data = dat, importance = "permutation", num.trees = 1000)
n_export_margin.p <- data.frame(importance_pvalues(n_export_margin.rf, method = "altmann", 
                                                   formula = n_exp.margin.form, data = dat, num.permutations = 1000))

sed_export_base.rf <- ranger(sed.base.form, data = dat, importance = "permutation", num.trees = 1000)
sed_export_base.p <- data.frame(importance_pvalues(sed_export_base.rf, method = "altmann", 
                                                   formula = sed.base.form, data = dat, num.permutations = 1000))
sed_export_margin.rf <- ranger(sed.margin.form, data = dat, importance = "permutation", num.trees = 1000)
sed_export_margin.p <- data.frame(importance_pvalues(sed_export_margin.rf, method = "altmann", 
                                                     formula = sed.margin.form, data = dat, num.permutations = 1000))

qb_base.rf <- ranger(qb.base.form, data = dat, importance = "permutation", num.trees = 1000)
qb_base.p <- data.frame(importance_pvalues(qb_base.rf, method = "altmann", 
                                           formula = qb.base.form, data = dat, num.permutations = 1000))
qb_margin.rf <- ranger(qb.margin.form, data = dat, importance = "permutation", num.trees = 1000)
qb_margin.p <- data.frame(importance_pvalues(qb_margin.rf, method = "altmann",
                                             formula = qb.margin.form, data = dat, num.permutations = 1000))

# save the outputs
save(list = c("dat",ls(pattern = "^[npqs]")), file = "data/ES-RF2.RDA")
