library(ranger)
library(iml)
library(future)
load("data/ES-RF3.RDA")
pfun <- function(object, newdata) predict(object, data = newdata)$predictions

future::plan(multisession, workers = 8)

# pollinators base

pred.pollinators_base <- Predictor$new(
  model = pollinators_base.rf,
  data = dat,
  y = "pollinators_base",
  predict.function = pfun
)

int.polbase  <- Interaction$new(pred.pollinators_base)
int.polbase.ntr_pland  <- Interaction$new(pred.pollinators_base, feature = "ntr_pland")

# pollinators margin

pred.pollinators_margin <- Predictor$new(
  model = pollinators_margin.rf,
  data = dat,
  y = "delta_pollinators_max.margin",
  predict.function = pfun
)

int.polmarg  <- Interaction$new(pred.pollinators_margin)
int.polmarg.ntr_pland  <- Interaction$new(pred.pollinators_margin, feature = "ntr_pland")

# N export base

pred.n_export_base <- Predictor$new(
  model = n_export_base.rf,
  data = dat,
  y = "n_export_base",
  predict.function = pfun
)

int.nexpbase  <- Interaction$new(pred.n_export_base)
int.nexpbase.ntr_pland  <- Interaction$new(pred.n_export_base, feature = "ntr_pland")

# N export margin

pred.n_export_margin <- Predictor$new(
  model = n_export_margin.rf,
  data = dat,
  y = "delta_n_export_max.margin",
  predict.function = pfun
)

int.nexpmarg  <- Interaction$new(pred.n_export_margin)
int.nexpmarg.ann_agr_pland  <- Interaction$new(pred.n_export_margin, feature = "ann_agr_pland")

# Sediment export base

pred.sed_export_base <- Predictor$new(
  model = sed_export_base.rf,
  data = dat,
  y = "sed_export_base",
  predict.function = pfun
)

int.sedexpbase  <- Interaction$new(pred.sed_export_base)
int.sedexpbase.ann_agr_ed  <- Interaction$new(pred.sed_export_base, feature = "ann_agr_ed")

# Sediment export margin

pred.sed_export_margin <- Predictor$new(
  model = sed_export_margin.rf,
  data = dat,
  y = "sed_export_base",
  predict.function = pfun
)

int.sedexpmarg  <- Interaction$new(pred.sed_export_margin)
int.sedexpmarg.lu_shdi  <- Interaction$new(pred.sed_export_margin, feature = "lu_shdi")

# Water recharge base

pred.qb_base <- Predictor$new(
  model = qb_base.rf,
  data = dat,
  y = "qb_base",
  predict.function = pfun
)

int.qbbase  <- Interaction$new(pred.qb_base)
int.qbbase.field_frac_mn  <- Interaction$new(pred.sed_export_margin, feature = "field_frac_mn")

# Water recharge margin

pred.qb_margin <- Predictor$new(
  model = qb_margin.rf,
  data = dat,
  y = "delta_qb_max.margin",
  predict.function = pfun
)

int.qbmarg  <- Interaction$new(pred.qb_margin)
int.qbmarg.field_frac_cv <- Interaction$new(pred.qb_margin, feature = "field_frac_cv")

save(list = ls(pattern = "^int|^pred"), file = "data/ES-RF3-preds.RDA")

# ## Interactions
# ```{r int_load}
# library(iml)
# library(gridExtra)
# load("data/ES-RF2-preds.RDA")
# ```
#
# ## Pollinator base interactions
# 
# ```{r polbase_int}
# grid.arrange(
#   int.polbase %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.polbase.ntr_pland %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## Pollinators margins interactions
# 
# ```{r polmarg_int}
# grid.arrange(
#   int.polmarg %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.polmarg.ntr_pland %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## N export base interactions
# 
# ```{r nexpbase_int}
# grid.arrange(
#   int.nexpbase %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.nexpbase.ntr_pland %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## N export margin interactions
# 
# ```{r nexpmarg_int}
# grid.arrange(
#   int.nexpmarg %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.nexpmarg.ann_agr_pland %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## Sediment export base interactions
# 
# ```{r sedbase_int}
# grid.arrange(
#   int.sedexpbase %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.sedexpbase.ann_agr_ed %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## Sediment export margin interactions
# 
# ```{r sedmarg_int}
# grid.arrange(
#   int.sedexpmarg %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.sedexpmarg.lu_shdi %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# 
# ## Baseflow base interactions
# 
# ```{r qbbase_int}
# grid.arrange(
#   int.qbbase %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.qbbase.field_frac_mn %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
# 
# ## Baseflow margin interactions
# 
# ```{r qbmarg_int}
# grid.arrange(
#   int.qbmarg %>% plot() + ggtitle("Interaction strength") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   int.qbmarg.field_frac_cv %>% plot() + ggtitle("Pairwise interactions with strongest") + egg::theme_article() + theme(plot.title.position = "plot", axis.title.y = element_blank()),
#   ncol = 2
# )
# ```
