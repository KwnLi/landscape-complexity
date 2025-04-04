# shapley importance
library(fastshap)
library(shapviz)

load("data/ES-RF3.RDA")
load("data/ES-RF3pc.RDA")

pfun <- function(object, newdata) predict(object, data = newdata)$predictions

dat.x <- dat[,17:46]

# pollinators
ex.polbase <- explain(pollinators_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.polbase.glb <- shapviz(ex.polbase)

ex.polmarg <- explain(pollinators_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.polmarg.glb <- shapviz(ex.polmarg)

# N export
ex.nexpbase <- explain(n_export_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.nexpbase.glb <- shapviz(ex.nexpbase)

ex.nexpmarg <- explain(n_export_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.nexpmarg.glb <- shapviz(ex.nexpmarg)

# sediment export
ex.sedbase <- explain(sed_export_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.sedbase.glb <- shapviz(ex.sedbase)

ex.sedmarg <- explain(sed_export_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.sedmarg.glb <- shapviz(ex.sedmarg)

# baseflow
ex.qbbase <- explain(qb_base.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.qbbase.glb <- shapviz(ex.qbbase)

ex.qbmarg <- explain(qb_margin.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.qbmarg.glb <- shapviz(ex.qbmarg)

save(list = ls(pattern = "^ex"), file = "data/ES-RF3-shapimportance.RDA")

# percent models
ex.polpc <- explain(pollinators_pc.rf, X=dat.x, pred_wrapper = pfun,
                      nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.polpc.glb <- shapviz(ex.polpc)

ex.nexppc <- explain(n_export_pc.rf, X=dat.x, pred_wrapper = pfun,
                    nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.nexppc.glb <- shapviz(ex.nexppc)

ex.sedpc <- explain(sed_pc.rf, X=dat.x, pred_wrapper = pfun,
                     nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.sedpc.glb <- shapviz(ex.sedpc)

ex.qbpc <- explain(qb_pc.rf, X=dat.x, pred_wrapper = pfun,
                    nsim = 1000, adjust = TRUE, shap_only = FALSE)
ex.qbpc.glb <- shapviz(ex.qbpc)

save(list = ls(pattern = "pc"), file = "data/ES-RF3pc-shapimportance.RDA")
