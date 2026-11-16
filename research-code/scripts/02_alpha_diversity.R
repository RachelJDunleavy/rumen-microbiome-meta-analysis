# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# FIG 1  Alpha diversity, three panels, by strategy, control and treatment SEPARATE
# ============================================================
metric_lv  <- c("shannon","observed","evenness")
metric_lab <- c(shannon = "Shannon diversity", observed = "Observed features", evenness = "Pielou's evenness")
strats <- levels(droplevels(d$Strategy_Group))
al <- d %>% mutate(Strategy_Group = droplevels(Strategy_Group)) %>%
  pivot_longer(c(shannon, observed, evenness), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = metric_lv, labels = metric_lab[metric_lv]),
         xpos = as.numeric(Strategy_Group) + ifelse(Label == "Control", -.2, .2),
         grp = interaction(Strategy_Group, Label))
ncount <- d %>% mutate(Strategy_Group = droplevels(Strategy_Group)) %>%
  count(Strategy_Group, Label) %>% pivot_wider(names_from = Label, values_from = n, values_fill = 0) %>%
  mutate(lbl = paste0(Treatment, " / ", Control), x = as.numeric(Strategy_Group))
ncount <- merge(ncount, data.frame(metric = factor(metric_lab, levels = metric_lab)))
f1 <- ggplot(al, aes(xpos, value)) +
  geom_boxplot(aes(group = grp, fill = Strategy_Group, colour = Strategy_Group, alpha = Label),
               width = .34, outlier.shape = NA, linewidth = .4) +
  geom_point(aes(colour = Strategy_Group, shape = Label), alpha = .45, size = .7,
             position = position_jitter(width = .07, height = 0)) +
  geom_text(data = ncount, aes(x = x, y = Inf, label = lbl), inherit.aes = FALSE, vjust = 1.4, size = 2.3) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(breaks = seq_along(strats), labels = strats) +
  scale_fill_manual(values = strat_cols, drop = TRUE, name = "Mitigation strategy") +
  scale_colour_manual(values = strat_cols, drop = TRUE, name = "Mitigation strategy") +
  scale_alpha_manual(values = c(Control = .08, Treatment = .55), name = NULL) +
  scale_shape_manual(values = c(Control = 1, Treatment = 16), name = NULL) +
  labs(x = "Methane mitigation strategy", y = "Alpha diversity",
       caption = "Left box/open circles = control, right box/filled circles = treatment.  n = treatment / control.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom",
        strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
save_fig(f1, "Fig1_alpha_three_panel_control_vs_treatment", 14, 6.5)

