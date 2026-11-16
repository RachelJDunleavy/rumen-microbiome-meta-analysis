# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# FIG 5 / 6  Pooled effect and meta-regression PER treatment (Shannon, RMD %)
# ============================================================
res <- do.call(rbind, lapply(split(es, droplevels(es$Strategy_Group)), function(g) {
  one <- data.frame(Strategy = as.character(g$Strategy_Group[1]), k = nrow(g), rmd = NA_real_, lo = NA_real_, hi = NA_real_,
                    dose_k = sum(!is.na(g$Dose)), dose_slope = NA_real_, dose_p = NA_real_)
  if (nrow(g) == 1) { one$rmd <- g$rmd; one$lo <- g$lo; one$hi <- g$hi; return(one) }
  g$cid <- seq_len(nrow(g))
  m <- tryCatch(rma.mv(yi, vi, random = ~ 1 | Study/cid, data = g), error = function(e) rma(yi, vi, data = g))
  one$rmd <- 100 * (exp(as.numeric(m$b[1])) - 1); one$lo <- 100 * (exp(m$ci.lb) - 1); one$hi <- 100 * (exp(m$ci.ub) - 1)
  gd <- g %>% filter(!is.na(Dose), Dose > 0)
  if (nrow(gd) >= 3 && length(unique(gd$Dose_Unit)) == 1 && length(unique(gd$Dose)) >= 2) {
    mr <- tryCatch(rma(yi, vi, mods = ~ log(Dose), data = gd), error = function(e) NULL)
    if (!is.null(mr)) { one$dose_slope <- mr$b[2]; one$dose_p <- mr$pval[2] }
  }
  one
}))
res$dose_note <- ifelse(is.na(res$dose_slope), "dose model not run (too few doses / mixed units)", "log(dose) meta-regression")
write.csv(res, "Tables/meta_regression_per_treatment.csv", row.names = FALSE)

f5 <- ggplot(res, aes(rmd, reorder(Strategy, rmd), colour = Strategy)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_linerange(aes(xmin = lo, xmax = hi), orientation = "y", linewidth = .6, na.rm = TRUE) +
  geom_point(size = 3, shape = 15) +
  geom_text(aes(label = paste0("k = ", k)), nudge_y = .3, size = 2.8, colour = "black") +
  scale_colour_manual(values = strat_cols, guide = "none") +
  labs(x = "Pooled Relative Mean Difference in Shannon (%)", y = NULL, title = "Random-effects estimate per treatment type") +
  theme(panel.grid.minor = element_blank())
save_fig(f5, "Fig5_pooled_per_treatment", 8, 5)

dr <- es %>% filter(as.character(Strategy_Group) %in% res$Strategy[!is.na(res$dose_slope)], !is.na(Dose), Dose > 0)
if (nrow(dr) > 0) {
  f6 <- ggplot(dr, aes(Dose, rmd, colour = Strategy_Group)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_point(aes(size = 1 / se), shape = 15) + geom_smooth(aes(group = 1), method = "lm", colour = "black", se = TRUE, linewidth = .5) +
    scale_x_log10() + scale_colour_manual(values = strat_cols, drop = TRUE, name = "Treatment type") +
    facet_wrap(~ paste(Strategy_Group, Dose_Unit), scales = "free_x") +
    labs(x = "Dose (log scale; units in panel title)", y = "RMD in Shannon (%)", size = "precision")
  save_fig(f6, "Fig6_dose_response", 10, 6)
}

