# R analysis workflow — private review draft

These nine modules are extracted in original order from the appendix of `R_code_blocks.md` (USDA figure workflow v2). They share objects in one R session; they are not standalone scripts. The only substantive configuration edits are replacing local input paths with environment-variable settings. The runner and this documentation were added during portfolio preparation with AI assistance. Authorship of the original code has not been independently verified.

## Run from the repository root

1. Install R and the packages listed in `00_setup.R` (readxl, dplyr, tidyr, stringr, ggplot2, patchwork, vegan, metafor, lme4, lmerTest). The archived setup installs missing packages automatically; review before running.
2. Supply your approved metadata workbook at `inputs/Metadata_corrected_v11.xlsx` and QIIME artifacts in `inputs/qiime2/`, or set `RUMEN_METADATA` and `RUMEN_QZA_DIR` to the appropriate paths. The artifacts must be from the same analysis run. Expected files: `shannon_vector.qza`, `evenness_vector.qza`, `observed_features_vector.qza`, `bray_curtis_distance_matrix.qza`, and one of `bray_curtis_pcoa_10k.qza`, `bray_curtis_pcoa_results.qza`, `bray_pcoa.qza`.
3. Run `Rscript scripts/run_all.R` or source that file in RStudio from the repository root. Outputs go to `Figures/` and `Tables/`.

## Review before publication

- No input data or research outputs are included. This package has not been executed or validated in this environment; R and the required data are unavailable.
- The archive records failed runs and a run that selected only 16 samples from one study. Verify artifact selection, sample matching and study counts before interpreting outputs. The appendix is not proof of a successful final run.
- Review metadata row removal, missing values, matching of control groups, repeated measures/shared controls, permutation restrictions, dose units, effect-size variance and model assumptions with the research team. The archived code is preserved, not statistically corrected.
- The mixed model is exploratory and may fail with insufficient study levels; dose regression is conditional on available data.
- Keep private until advisor approval and authorship review. For Sage, submit only code meeting its personally authored sample requirement; do not claim the AI-added runner/documentation as independently authored.
