# shapley importance
library(fastshap)
library(shapviz)

load("data/ES-RF2.RDA")

pfun <- function(object, newdata) predict(object, data = newdata)$predictions

dat.x <- dat[,13:41]

# pollinators
ex.polbase <- explain(pollinators_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.polbase.glb <- shapviz(ex.polbase)

ex.polmarg <- explain(pollinators_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.polmarg.glb <- shapviz(ex.polmarg)

# N export
ex.nexpbase <- explain(n_export_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.nexpbase.glb <- shapviz(ex.nexpbase)

ex.nexpmarg <- explain(n_export_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.nexpmarg.glb <- shapviz(ex.nexpmarg)

# sediment export
ex.sedbase <- explain(sed_export_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.sedbase.glb <- shapviz(ex.sedbase)

ex.sedmarg <- explain(sed_export_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.sedmarg.glb <- shapviz(ex.sedmarg)

# baseflow
ex.qbbase <- explain(qb_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.qbbase.glb <- shapviz(ex.qbbase)

ex.qbmarg <- explain(qb_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 100, adjust = TRUE, shap_only = FALSE)
ex.qbmarg.glb <- shapviz(ex.qbmarg)

save(list = ls(pattern = "^ex"), file = "data/ES-RF2-shapimportance.RDA")
