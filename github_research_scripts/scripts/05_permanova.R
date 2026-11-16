# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# Beta diversity: PERMANOVA R2 per study (9,999 permutations), BH-FDR
beta <- do.call(rbind, lapply(split(d, d$Study), function(s) {
  s <- s %>% filter(ID %in% rownames(bc))
  if (length(unique(s$Label)) < 2 || min(table(s$Label)) < 3) return(NULL)
  a <- adonis2(as.dist(bc[s$ID, s$ID]) ~ Label, data = s, permutations = 9999)
  data.frame(Study = s$Study[1], n = nrow(s), R2 = a$R2[1], p = a$`Pr(>F)`[1],
             Method = names(sort(table(s$Mitigation_Method[s$Label == "Treatment"]), decreasing = TRUE))[1])
}))
if (!is.null(beta)) {
  beta$q <- p.adjust(beta$p, "BH"); beta$sig <- ifelse(beta$q < .05, "FDR < 0.05", "Not significant")
  write.csv(beta, "Tables/beta_permanova_by_study.csv", row.names = FALSE)
}

