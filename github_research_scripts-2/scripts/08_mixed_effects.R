# Extracted from the archived USDA v2 workflow; see scripts/README.md.
cat("\nSamples used:", nrow(d), "from", length(unique(d$Study)), "studies;", nrow(es), "treatment-vs-control comparisons in the forest plot.\n")
mm <- tryCatch(lmer(shannon ~ Label * Strategy_Group + (1 | Study), data = droplevels(d)),
               error = function(e) { message("Mixed model skipped: ", conditionMessage(e)); NULL })
if (!is.null(mm)) capture.output(anova(mm), file = "Tables/alpha_mixed_model.txt")
cat("\nDone. Figures/ and Tables/ are ready. Check Tables/pending_labels.csv for samples still waiting on a key.\n")
