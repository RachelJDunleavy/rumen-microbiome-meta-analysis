# Run from repository root. Sources the archived workflow in its original order.
for (f in c("scripts/00_setup.R",
  "scripts/01_metadata_processing.R",
  "scripts/02_alpha_diversity.R",
  "scripts/03_beta_diversity.R",
  "scripts/04_effect_sizes.R",
  "scripts/05_permanova.R",
  "scripts/06_forest_plots.R",
  "scripts/07_pooled_effects.R",
  "scripts/08_mixed_effects.R")) {
  message("Running ", f)
  source(f)
}
