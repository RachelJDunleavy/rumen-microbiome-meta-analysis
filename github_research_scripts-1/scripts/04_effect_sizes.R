# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# Effect sizes: relative mean difference (RMD %), treatment vs control WITHIN a study
# Comparison = Study x Study_Type x Strategy x Treatment x Dose.  Log response ratio -> % = 100*(exp(x)-1)
# ============================================================
d <- d %>% mutate(Dose_key = ifelse(is.na(Dose), "NA", as.character(Dose)))
make_es <- function(metric) {
  dd <- d; dd$v <- dd[[metric]]
  ctrl <- dd %>% filter(Label == "Control") %>% group_by(Study, Study_Type) %>%
    summarise(c_n = n(), c_m = mean(v), c_sd = sd(v), .groups = "drop")
  trt <- dd %>% filter(Label == "Treatment") %>%
    group_by(Study, Study_Type, Strategy_Group, Mitigation_Method, Type, Dose_key, Dose, Dose_Unit) %>%
    summarise(t_n = n(), t_m = mean(v), t_sd = sd(v), .groups = "drop")
  e <- inner_join(trt, ctrl, by = c("Study", "Study_Type")) %>% filter(t_n >= 3, c_n >= 3)
  e <- escalc(measure = "ROM", m1i = t_m, sd1i = t_sd, n1i = t_n, m2i = c_m, sd2i = c_sd, n2i = c_n, data = e)
  e$se <- sqrt(e$vi); e$metric <- metric
  e$rmd <- 100 * (exp(e$yi) - 1)
  e$lo  <- 100 * (exp(e$yi - 1.96 * e$se) - 1)
  e$hi  <- 100 * (exp(e$yi + 1.96 * e$se) - 1)
  e[is.finite(e$yi) & is.finite(e$vi), ]
}
es_l <- lapply(setNames(metric_lv, metric_lv), make_es)
keys <- c("Study", "Study_Type", "Strategy_Group", "Mitigation_Method", "Dose_key")
common <- Reduce(function(a, b) inner_join(a, b, by = keys), lapply(es_l, function(e) e[keys]))
es_l <- lapply(es_l, function(e) {
  e <- inner_join(common, e, by = keys)
  e %>% arrange(factor(Strategy_Group, levels = names(strat_cols)), Study, Mitigation_Method, Dose) %>%
    mutate(id = row_number())
})
stopifnot(identical(es_l$shannon$Study, es_l$observed$Study))
write.csv(bind_rows(es_l), "Tables/effect_sizes_RMD.csv", row.names = FALSE)
es <- es_l$shannon                                    # row order shared by all panels

