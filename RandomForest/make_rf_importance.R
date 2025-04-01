# make importance values
library(iml)
load("data/ES-RF3.RDA")
load("data/ES-RF3-preds.RDA")

# pollinators
imp.polbase <- FeatureImp$new(pred.pollinators_base, loss = "mae")
imp.polmarg <- FeatureImp$new(pred.pollinators_margin, loss = "mae")

# N export
imp.nexpbase <- FeatureImp$new(pred.n_export_base, loss = "mae")
imp.nexpmarg <- FeatureImp$new(pred.n_export_margin, loss = "mae")

# sed export
imp.sedbase <- FeatureImp$new(pred.sed_export_base, loss = "mae")
imp.sedmarg <- FeatureImp$new(pred.sed_export_margin, loss = "mae")

# baseflow
imp.qbbase <- FeatureImp$new(pred.qb_base, loss = "mae")
imp.qbmarg <- FeatureImp$new(pred.qb_margin, loss = "mae")
