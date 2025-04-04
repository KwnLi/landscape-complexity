wfs <- readRDS("data/allresults_20250401.rds") 

lsm <- read.csv("data/lsm.csv") %>%
  mutate(hu = gsub("hu", "", hu))

wfs.long <- wfs %>% 
  pivot_longer(cols = c(pollinators_base:sed_export_base, delta_pollinators:delta_sed, delta_pollinators_max.margin:delta_sed_max.margin), names_to = "ES_response", values_to = "ES_value") %>%
  select(hu, ES_response, ES_value, precip_mean) %>%
  mutate(ES = gsub("delta_|_base|_max.margin","", ES_response)) %>%
  mutate(ES = gsub("sed$","sed_export", ES)) %>%
  mutate(response_type =
           gsub("sed|export|n_|pollinators|qb|_|max","",ES_response))

# fit some gams
wfs.gam <- wfs.long %>% 
  group_by(response_type, ES) %>%
  nest() %>%
  # Fit GAM
  mutate(Mod = map(data, ~ lm(ES_value ~ precip_mean, data = .x))) %>%
  # Get R2
  mutate(R2 = map_dbl(Mod, ~round(summary(.x)$r.sq, 2)))  

ggplot(wfs.long,
       aes(precip_mean, ES_value)) + 
  geom_point() + geom_smooth(method="lm") +
  ggh4x::facet_grid2(response_type ~ ES, 
                     scales = "free", independent = "y") + 
  egg::theme_article() + 
  # Add label
  geom_label(data = wfs.gam, 
             aes(x = Inf, y = Inf, 
                 label = paste("R2 = ", R2, sep = " ")),
             hjust = 1, vjust = 1)

wfs.wider <- wfs.long %>% select(-ES_response) %>% 
  pivot_wider(names_from = response_type, values_from = ES_value) %>%
  left_join(lsm %>% select(hu, lu_ta))

base.v.delta <- ggplot(wfs.wider, aes(base, delta)) + geom_point() + facet_wrap(~ES, scales = "free")
delta.v.margin <- ggplot(wfs.wider, aes(delta, delta.margin)) + geom_point() + facet_wrap(~ES, scales = "free")
base.v.margin <- ggplot(wfs.wider, aes(base, delta.margin)) + geom_point() + 
  xlab("Baseline ES per ha") + ylab("Increase in ES per ha margin (ES units)") +
  geom_smooth(method = "gam") + facet_wrap(~ES, scales = "free") + 
  ggtitle("ES baseline against ES improvement per margin (what we use now)") + egg::theme_article()
base.v.margin.base <- ggplot(wfs.wider, aes(base, 100*delta.margin/(base*lu_ta))) + 
  geom_point() + geom_smooth(method = "gam") + facet_wrap(~ES, scales = "free") +
  ggtitle("ES Baseline against % ES improvement per margin") + 
  xlab("Baseline ES per ha") +
  ylab("% improvement from baseline per ha margin") +
  egg::theme_article()

cowplot::plot_grid(base.v.margin, base.v.margin.base)
