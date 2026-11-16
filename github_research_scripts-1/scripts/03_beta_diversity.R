# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# FIG 2  PCoA (Bray-Curtis)
# ============================================================
lines <- readLines(read_qza_file("^(bray_curtis_pcoa_10k|bray_curtis_pcoa_results|bray_pcoa)\\.qza$", "ordination.txt"))
i2 <- grep("^Proportion explained", lines); i3 <- grep("^Site\t", lines)
pve <- as.numeric(strsplit(lines[i2 + 1], "\t")[[1]])
site <- read.table(text = lines[(i3 + 1):(i3 + as.numeric(strsplit(lines[i3], "\t")[[1]][2]))], sep = "\t", row.names = 1)
pc <- data.frame(ID = rownames(site), PC1 = site[[1]], PC2 = site[[2]])
pcd <- d %>% inner_join(pc, by = "ID") %>% mutate(Study_Type = ifelse(is.na(Study_Type), "Not recorded", Study_Type))
axl <- list(sprintf("PCoA 1 (%.1f%%)", 100 * pve[1]), sprintf("PCoA 2 (%.1f%%)", 100 * pve[2]))

# 2a  all samples, control blue / treatment coral, ellipses (like your slide)
f2a <- ggplot(pcd, aes(PC1, PC2, colour = Label, shape = Label)) +
  geom_point(size = 1.4, alpha = .7) + stat_ellipse(type = "norm", linewidth = .6) +
  scale_colour_manual(values = lab_cols, name = "Group") + scale_shape_manual(values = c(Control = 16, Treatment = 17), name = "Group") +
  labs(title = "Bray-Curtis Beta Diversity", subtitle = "Rumen microbiome response to methane mitigation", x = axl[[1]], y = axl[[2]]) +
  theme(panel.grid.minor = element_blank())
save_fig(f2a, "Fig2a_PCoA_control_vs_treatment", 7.5, 6)

# 2b  controls removed, colour = treatment type, shape = study type (your meeting note)
f2b <- ggplot(filter(pcd, Label == "Treatment"), aes(PC1, PC2, colour = Strategy_Group, shape = Study_Type)) +
  geom_point(size = 1.8, alpha = .8) +
  scale_colour_manual(values = strat_cols, drop = TRUE, name = "Treatment type") + scale_shape_discrete(name = "Study type") +
  labs(title = "Bray-Curtis PCoA, treated samples only", x = axl[[1]], y = axl[[2]]) +
  theme(panel.grid.minor = element_blank())
save_fig(f2b, "Fig2b_PCoA_treatment_type_no_controls", 8.5, 6)

# 2c  independent PCoA per study: hulls + centroids (like your 'Individual study PCoA' slide)
study_pcoa <- bind_rows(lapply(split(d, d$Study), function(s) {
  s <- s[s$ID %in% rownames(bc), ]
  if (length(unique(s$Label)) < 2 || min(table(s$Label)) < 3 || nrow(s) < 6) return(NULL)
  xy <- cmdscale(as.dist(bc[s$ID, s$ID]), k = 2)
  data.frame(Study = s$Study, Strategy_Group = as.character(s$Strategy_Group), Label = s$Label, PC1 = xy[, 1], PC2 = xy[, 2])
}))
pcoa_panel <- function(df, ncol = 3) {
  hulls <- df %>% group_by(Study, Label) %>% filter(n() >= 3) %>% slice(chull(PC1, PC2)) %>% ungroup()
  cent  <- df %>% group_by(Study, Label) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
  ggplot(df, aes(PC1, PC2, colour = Label, fill = Label)) +
    geom_polygon(data = hulls, alpha = .08, linewidth = .4) +
    geom_point(aes(shape = Label), size = 1.3, alpha = .8) +
    geom_point(data = cent, shape = 21, size = 3.2, colour = "black") +
    facet_wrap(~ Study, scales = "free", ncol = ncol) +
    scale_colour_manual(values = lab_cols, name = "Group") + scale_fill_manual(values = lab_cols, name = "Group") +
    scale_shape_manual(values = c(Control = 16, Treatment = 17), name = "Group") +
    labs(x = "PCoA 1", y = "PCoA 2", caption = "Independent ordination per study; axes are not comparable across studies. Large circles = centroids.") +
    theme(legend.position = "bottom", strip.text = element_text(size = 7, face = "bold"), panel.grid.minor = element_blank())
}
if (nrow(study_pcoa) > 0) {
  ns <- length(unique(study_pcoa$Study))
  save_fig(pcoa_panel(study_pcoa), "Fig2c_PCoA_each_study", 11, max(4, 2.6 * ceiling(ns / 3) + 1))
  for (sg in unique(study_pcoa$Strategy_Group)) {                 # one file per treatment category
    sub <- filter(study_pcoa, Strategy_Group == sg)
    nss <- length(unique(sub$Study))
    save_fig(pcoa_panel(sub, ncol = min(3, nss)) + ggtitle(sg), paste0("PCoA_by_strategy/", gsub("[^A-Za-z0-9]+", "_", sg)),
             3.8 * min(3, nss) + .6, 3.2 * ceiling(nss / 3) + 1.3)
  }
}

