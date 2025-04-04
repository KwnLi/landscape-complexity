# make random forest from unique datasets

library(ranger)
options(ranger.num.threads = 8)

# wfs <- readRDS("data/allresults_20250401.rds")
wfs <- readRDS("data/allresults_20250403.rds")

names(wfs)

dat.predictors <-  wfs[, c(85:112,126,127)]

dat.response <- wfs[, c(2:5, 10:13, 20:27)]

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

# Percent change models
# formulae
pol.pc.form <- reformulate(names(dat.predictors), response = "pc_pol_incr.margin")
n_exp.pc.form <- reformulate(names(dat.predictors), response = "pc_nexp_red.margin")
qb.pc.form <- reformulate(names(dat.predictors), response = "pc_qb_incr.margin")
sed.pc.form <- reformulate(names(dat.predictors), response = "pc_sed_red.margin")

# rf models
pollinators_pc.rf <- ranger(pol.pc.form, data = dat, importance = "permutation", num.trees = 1000)
pollinators_pc.p <- data.frame(importance_pvalues(pollinators_pc.rf, method = "altmann", 
                                                    formula = pol.pc.form, data = dat, num.permutations = 1000))

n_export_pc.rf <- ranger(n_exp.pc.form, data = dat, importance = "permutation", num.trees = 1000)
n_export_pc.p <- data.frame(importance_pvalues(n_export_pc.rf, method = "altmann", 
                                                 formula = n_exp.pc.form, data = dat, num.permutations = 1000))

qb_pc.rf <- ranger(qb.pc.form, data = dat, importance = "permutation", num.trees = 1000)
qb_pc.p <- data.frame(importance_pvalues(qb_pc.rf, method = "altmann", 
                                               formula = qb.pc.form, data = dat, num.permutations = 1000))

sed_pc.rf <- ranger(sed.pc.form, data = dat, importance = "permutation", num.trees = 1000)
sed_pc.p <- data.frame(importance_pvalues(sed_pc.rf, method = "altmann", 
                                         formula = sed.pc.form, data = dat, num.permutations = 1000))

# # run random forest using different importance
# pollinators_base.imp.rf <- ranger(pol.base.form, data = dat, importance = "impurity", num.trees = 1000)
# pollinators_base.imp.p <- data.frame(importance_pvalues(pollinators_base.imp.rf, method = "altmann", 
#                                                     formula = pol.base.form, data = dat, num.permutations = 1000))
# pollinators_margin.imp.rf <- ranger(pol.margin.form, data = dat, importance = "impurity", num.trees = 1000)
# pollinators_margin.imp.p <- data.frame(importance_pvalues(pollinators_margin.imp.rf, method = "altmann", 
#                                                       formula = pol.margin.form, data = dat, num.permutations = 1000))
# 
# n_export_base.imp.rf <- ranger(n_exp.base.form, data = dat, importance = "impurity", num.trees = 1000)
# n_export_base.imp.p <- data.frame(importance_pvalues(n_export_base.imp.rf, method = "altmann", 
#                                                  formula = n_exp.base.form, data = dat, num.permutations = 1000))
# n_export_margin.imp.rf <- ranger(n_exp.margin.form, data = dat, importance = "impurity", num.trees = 1000)
# n_export_margin.imp.p <- data.frame(importance_pvalues(n_export_margin.imp.rf, method = "altmann", 
#                                                    formula = n_exp.margin.form, data = dat, num.permutations = 1000))
# 
# sed_export_base.imp.rf <- ranger(sed.base.form, data = dat, importance = "impurity", num.trees = 1000)
# sed_export_base.imp.p <- data.frame(importance_pvalues(sed_export_base.imp.rf, method = "altmann", 
#                                                    formula = sed.base.form, data = dat, num.permutations = 1000))
# sed_export_margin.imp.rf <- ranger(sed.margin.form, data = dat, importance = "impurity", num.trees = 1000)
# sed_export_margin.imp.p <- data.frame(importance_pvalues(sed_export_margin.imp.rf, method = "altmann", 
#                                                      formula = sed.margin.form, data = dat, num.permutations = 1000))
# 
# qb_base.imp.rf <- ranger(qb.base.form, data = dat, importance = "impurity", num.trees = 1000)
# qb_base.imp.p <- data.frame(importance_pvalues(qb_base.imp.rf, method = "altmann", 
#                                            formula = qb.base.form, data = dat, num.permutations = 1000))
# qb_margin.imp.rf <- ranger(qb.margin.form, data = dat, importance = "impurity", num.trees = 1000)
# qb_margin.imp.p <- data.frame(importance_pvalues(qb_margin.imp.rf, method = "altmann",
#                                              formula = qb.margin.form, data = dat, num.permutations = 1000))


# save the permutation outputs
save(list = c("dat",ls(pattern = "^[npqs]")), file = "data/ES-RF3.RDA")

# save just the impurity-based outputs
# save(list = ls(pattern = "imp"), file = "data/ES-RF2-impurity.RDA")

# save the percentage outputs
save(list = c("dat",ls(pattern = "pc")), file = "data/ES-RF3pc.RDA")
