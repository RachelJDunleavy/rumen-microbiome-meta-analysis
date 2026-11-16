# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# FIG 3 / 4  Forest plots in your Kebreab layout
# ============================================================
fmt_ci <- function(x, lo, hi) sprintf("%.1f [%.1f, %.1f]", x, lo, hi)
forest_panel <- function(e, title, full_cols, show_legend = FALSE) {
  n <- nrow(e); e$y <- n:1
  fit <- tryCatch(rma(yi, vi, data = e, method = "REML"), error = function(err) NULL)
  pooled <- if (!is.null(fit)) c(est = 100 * (exp(as.numeric(fit$b)[1]) - 1), lo = 100 * (exp(as.numeric(fit$ci.lb)) - 1), hi = 100 * (exp(as.numeric(fit$ci.ub)) - 1)) else c(est = NA_real_, lo = NA_real_, hi = NA_real_)
  lim <- c(floor(min(c(-100, pmax(e$lo, -250)), na.rm = TRUE) / 50) * 50, ceiling(max(c(50, pmin(e$hi, 300)), na.rm = TRUE) / 50) * 50)
  W <- diff(lim)
  e$lo_c <- pmax(e$lo, lim[1]); e$hi_c <- pmin(e$hi, lim[2])
  e$w <- 1 / e$vi; e$sz <- 1.4 + 3 * (e$w - min(e$w)) / max(1e-9, diff(range(e$w)))
  e$dose_txt <- ifelse(is.na(e$Dose), "-", format(e$Dose, trim = TRUE, drop0trailing = TRUE))
  yb <- -1.7; yh <- n + 1
  xL <- if (full_cols) lim[1] - 2.9 * W else lim[1] - 0.75 * W
  xR <- lim[2] + 0.95 * W
  p <- ggplot() +
    annotate("text", x = (lim[1] + lim[2]) / 2, y = n + 3.2, label = title, fontface = "bold", size = 4.4) +
    annotate("segment", x = xL, xend = xR, y = yh - .45, yend = yh - .45, linewidth = .5) +
    annotate("segment", x = xL, xend = xR, y = .45, yend = .45, linewidth = .5) +
    annotate("segment", x = 0, xend = 0, y = -.4, yend = n + .5, linetype = "dotted", linewidth = .5) +
    annotate("segment", x = lim[1], xend = lim[2], y = yb, yend = yb, linewidth = .4) +
    annotate("text", x = (lim[1] + lim[2]) / 2, y = yb - 2.2, label = "Relative Mean Difference (%)", size = 2.8) +
    annotate("text", x = lim[2] + .12 * W, y = yh, label = "RMD [95% CI]", hjust = 0, fontface = "bold", size = 2.7) +
    geom_segment(data = e, aes(x = lo_c, xend = hi_c, y = y, yend = y), linewidth = .35) +
    geom_segment(data = e, aes(x = lo_c, xend = lo_c, y = y - .28, yend = y + .28), linewidth = .35) +
    geom_segment(data = e, aes(x = hi_c, xend = hi_c, y = y - .28, yend = y + .28), linewidth = .35) +
    geom_text(data = e, aes(x = lim[2] + .12 * W, y = y, label = fmt_ci(rmd, lo, hi)), hjust = 0, size = 2.4)
  if (forest_colour_by_strategy) {
    p <- p + geom_point(data = e, aes(x = rmd, y = y, size = sz, colour = Strategy_Group), shape = 15) +
      scale_colour_manual(values = strat_cols, drop = TRUE, name = "Treatment type") + guides(size = "none")
  } else {
    p <- p + geom_point(data = e, aes(x = rmd, y = y, size = sz), shape = 15, colour = "black") + guides(size = "none")
  }
  p <- p + scale_size_identity()
  tk <- seq(lim[1], lim[2], by = 50)
  p <- p +
    annotate("segment", x = tk, xend = tk, y = yb, yend = yb - .5, linewidth = .4) +
    annotate("text", x = tk, y = yb - 1.3, label = tk, size = 2.5)
  # pooled random-effects diamond
  if (all(is.finite(pooled))) {
    dia <- data.frame(x = unname(c(pooled["lo"], pooled["est"], pooled["hi"], pooled["est"])), y = c(-.2, .15, -.2, -.55))
    p <- p + geom_polygon(data = dia, aes(x, y), fill = "#0000CD", colour = "black", linewidth = .2) +
      annotate("text", x = lim[2] + .12 * W, y = -.2, hjust = 0, size = 2.4, label = fmt_ci(pooled["est"], pooled["lo"], pooled["hi"]))
  }
  p <- p + annotate("text", x = xL + .02 * W, y = -.2, hjust = 0, size = 2.6, label = "Random-Effects Model")
  # left text columns
  p <- p + annotate("text", x = xL + .02 * W, y = yh, label = "ID", hjust = 0, fontface = "bold", size = 2.7) +
    geom_text(data = e, aes(x = xL + .02 * W, y = y, label = id), hjust = 0, size = 2.4)
  cols <- if (full_cols) list(list("Authors and Year", "Study", .2), list("Treatment", "Mitigation_Method", 1.25),
                              list("Type", "Type", 2.25), list("Dose", "dose_txt", 2.52))
          else list(list("Type", "Type", .22), list("Dose", "dose_txt", .52))
  for (cl in cols) {
    xx <- xL + cl[[3]] * W
    p <- p + annotate("text", x = xx, y = yh, label = cl[[1]], hjust = 0, fontface = "bold", size = 2.7) +
      geom_text(data = e, aes(x = xx, y = y, label = .data[[cl[[2]]]]), hjust = 0, size = 2.4)
  }
  p + coord_cartesian(xlim = c(xL, xR), ylim = c(yb - 2.8, n + 3.8), expand = FALSE, clip = "off") +
    theme_void() + theme(legend.position = "bottom", plot.margin = margin(4, 6, 4, 6))
}
hh <- function(n) max(5, 0.19 * n + 2.6)
pS <- forest_panel(es_l$shannon,  "Shannon Diversity",   TRUE,  FALSE)
pO <- forest_panel(es_l$observed, "Observed Features",   FALSE, FALSE)
pE <- forest_panel(es_l$evenness, "Pielou's Evenness",   FALSE, TRUE)
f3 <- (pS | pO | pE) + plot_layout(widths = c(2.9, 1.15, 1.15), guides = "collect") & theme(legend.position = "bottom")
save_fig(f3, "Fig3_forest_alpha_three_panel", 17, hh(nrow(es)))

# Fig 4: Shannon forest + beta diversity R2 on the SAME rows
if (!is.null(beta)) {
  bb <- es %>% mutate(y = rev(seq_len(n()))) %>% left_join(beta[, c("Study", "R2", "q", "sig")], by = "Study")
  rmax <- max(bb$R2, .1, na.rm = TRUE) * 1.15
  tkb <- pretty(c(0, rmax), n = 4); tkb <- tkb[tkb <= rmax]
  n4 <- nrow(bb); yb4 <- -1.7
  pB <- ggplot() +
    annotate("text", x = rmax / 2, y = n4 + 3.2, label = "Beta Diversity", fontface = "bold", size = 4.4) +
    annotate("segment", x = 0, xend = rmax, y = n4 + .55, yend = n4 + .55, linewidth = .5) +
    annotate("segment", x = 0, xend = rmax, y = .45, yend = .45, linewidth = .5) +
    annotate("text", x = 0, y = n4 + 1, label = "R\u00b2 [FDR]", hjust = 0, fontface = "bold", size = 2.7) +
    annotate("segment", x = 0, xend = rmax, y = yb4, yend = yb4, linewidth = .4) +
    annotate("segment", x = tkb, xend = tkb, y = yb4, yend = yb4 - .5, linewidth = .4) +
    annotate("text", x = tkb, y = yb4 - 1.3, label = tkb, size = 2.5) +
    annotate("text", x = rmax / 2, y = yb4 - 2.2, label = "Treatment-associated R\u00b2 (PERMANOVA)", size = 2.8) +
    geom_segment(data = filter(bb, !is.na(R2)), aes(x = 0, xend = R2, y = y, yend = y), colour = "grey80", linewidth = .4) +
    geom_point(data = filter(bb, !is.na(R2)), aes(R2, y, colour = sig), size = 2.6, shape = 15) +
    scale_colour_manual(values = sig_cols, name = "Beta diversity", na.translate = FALSE) +
    coord_cartesian(xlim = c(0, rmax), ylim = c(yb4 - 2.8, n4 + 3.8), expand = FALSE, clip = "off") +
    theme_void() + theme(legend.position = "bottom", plot.margin = margin(4, 6, 4, 6))
  f4 <- (pS + pB) + plot_layout(widths = c(3.4, 1), guides = "collect") & theme(legend.position = "bottom")
  save_fig(f4, "Fig4_forest_shannon_plus_beta", 13, hh(nrow(es)))

  # Fig 4b: your original beta dot plot (orange = FDR < 0.05)
  bd <- beta %>% mutate(lab = paste0(Method, " - ", Study)) %>% arrange(R2) %>% mutate(lab = factor(lab, levels = lab))
  f4b <- ggplot(bd, aes(R2, lab, colour = sig)) + geom_point(size = 2.8) +
    scale_colour_manual(values = sig_cols, name = "Statistical significance") +
    labs(title = "Effects of Methane Mitigation on Rumen Beta Diversity", subtitle = "Within-study Bray-Curtis PERMANOVA",
         x = "Treatment-associated R²", y = NULL) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
  save_fig(f4b, "Fig4b_beta_R2_by_study", 8.5, 5.5)
}

