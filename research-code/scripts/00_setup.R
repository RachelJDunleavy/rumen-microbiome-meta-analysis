# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ============================================================
# Rachel Dunleavy - USDA meta-analysis figures, v2 (one script, run top to bottom)
# Style: your original figures (strategy colours, blue control / coral treatment,
#        orange/blue significance) + Kebreab-style forest plot (ID, Authors, Treatment,
#        Type, Dose, RMD [95% CI], black/coloured squares, dotted zero line, blue diamond).
# Inputs : Metadata_corrected_v11.xlsx (in your working directory) and the QIIME .qza files
# Outputs: Figures/ (PNG + PDF) and Tables/ (CSV)
# ============================================================

# ---- 0. EDIT THESE ONLY IF YOUR PATHS CHANGE ----
meta_file <- Sys.getenv("RUMEN_METADATA", unset = "inputs/Metadata_corrected_v11.xlsx")
qza_dir <- Sys.getenv("RUMEN_QZA_DIR", unset = "inputs/qiime2")
forest_colour_by_strategy <- TRUE   # FALSE = black squares exactly like the example
# --------------------------------------------------

pkgs <- c("readxl","dplyr","tidyr","stringr","ggplot2","patchwork",
          "vegan","metafor","lme4","lmerTest")
new <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
dir.create("Figures", showWarnings = FALSE); dir.create("Tables", showWarnings = FALSE)
dir.create("Figures/PCoA_by_strategy", showWarnings = FALSE)
set.seed(1)

save_fig <- function(p, name, w, h) {
  ggsave(file.path("Figures", paste0(name, ".png")), p, width = w, height = h, dpi = 300, limitsize = FALSE)
  ggsave(file.path("Figures", paste0(name, ".pdf")), p, width = w, height = h, limitsize = FALSE)
}

# ---- 1. Read the QIIME artifacts (.qza is a zip) ----
read_qza_file <- function(pattern, inner) {
  if (!dir.exists(qza_dir)) stop("Folder '", qza_dir, "' not found. Working directory: ", getwd())
  f <- list.files(qza_dir, pattern = pattern, full.names = TRUE, recursive = FALSE, ignore.case = TRUE)[1]
  if (is.na(f)) stop("No .qza matching '", pattern, "' in ", qza_dir,
                     "\nFiles found:\n", paste(list.files(qza_dir, recursive = TRUE), collapse = "\n"))
  td <- tempfile(); dir.create(td); unzip(f, exdir = td)
 matches <- list.files(
  td,
  pattern = paste0("^", inner, "$"),
  recursive = TRUE,
  full.names = TRUE
)

if (length(matches) == 0) {
  stop("Expected file '", inner, "' not found inside QIIME artifact: ", f)
}

matches[1]
}
shannon  <- read.delim(read_qza_file("^shannon_vector\\.qza$", "alpha-diversity.tsv"), row.names = 1)
evenness <- read.delim(read_qza_file("^evenness_vector\\.qza$", "alpha-diversity.tsv"), row.names = 1)
obsfeat  <- read.delim(read_qza_file("^observed_features_vector\\.qza$", "alpha-diversity.tsv"), row.names = 1)
alpha <- data.frame(ID = rownames(shannon), shannon = shannon[[1]],
                    evenness = evenness[rownames(shannon), 1],
                    observed = obsfeat[rownames(shannon), 1])
bc <- as.matrix(read.delim(read_qza_file("^bray_curtis_distance_matrix\\.qza$", "distance-matrix.tsv"),
                           row.names = 1, check.names = FALSE))

