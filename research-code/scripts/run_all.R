
# Rumen Microbiome Meta-Analysis
# Run from the main GitHub repository directory.
# Executes the research scripts in their original order.

script_dir <- "research-code/scripts"

script_files <- c(
  "00_setup.R",
  "01_metadata_processing.R",
  "02_alpha_diversity.R",
  "03_beta_diversity.R",
  "04_effect_sizes.R",
  "05_permanova.R",
  "06_forest_plots.R",
  "07_pooled_effects.R",
  "08_mixed_effects.R"
)

for (f in script_files) {
  script_path <- file.path(script_dir, f)

  if (!file.exists(script_path)) {
    stop("Script not found: ", script_path)
  }

  message("Running ", script_path)
  source(script_path)
}

message("All scripts completed.")
