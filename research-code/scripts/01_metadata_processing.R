# Extracted from the archived USDA v2 workflow; see scripts/README.md.
# ---- 2. Metadata ----
md <- read_excel(meta_file, col_types = "text")
# Remove the QIIME 2 type row only when present.
if (nrow(md) > 0 && any(md[1, ] == "#q2:types", na.rm = TRUE)) {
  md <- md[-1, ]
}                                 # drop the #q2:types row
num <- function(x) suppressWarnings(as.numeric(x))
md <- md %>%
  mutate(Dose = num(Dose),
         Label = case_when(`CH4_Mitigation_trt-v-ctrl` == "Control"   ~ "Control",
                           `CH4_Mitigation_trt-v-ctrl` == "Treatment" ~ "Treatment",
                           TRUE ~ NA_character_),
         Study = Study_Label,
         Type  = ifelse(is.na(Ruminant_Type), "NA", Ruminant_Type)) %>%
  filter(is.na(Label_Flag) | !Label_Flag %in% c("NON_16S_LIBRARY","BASELINE_PRE_SEAWEED",
                                                "DIET_COMPARISON","CHECK_POSITIVE_CONTROL"))

pending <- md %>% filter(is.na(Label) | Label_Flag %in% c("ALL_LABELED_TREATMENT_NEEDS_KEY","UNK_LABEL","MISSING_LABEL"))
write.csv(pending %>% count(Study, Label_Flag, name = "n_samples"), "Tables/pending_labels.csv", row.names = FALSE)

d <- md %>% inner_join(alpha, by = "ID") %>%
  filter(!is.na(Label), !ID %in% pending$ID[pending$Label_Flag %in% "ALL_LABELED_TREATMENT_NEEDS_KEY"])
write.csv(d %>% count(Study, Strategy_Group, Label), "Tables/samples_used.csv", row.names = FALSE)

# ---- Colours: your original strategy colours; new groups use your Tol palette ----
strat_cols <- c("3-NOP"="#E76F51", "Seaweeds/Algae"="#2A9D8F", "Diet formulation"="#E9C46A",
                "Oils/Lipids"="#457B9D", "Plant secondary compounds"="#8D99AE", "Ionophores"="#9B5DE5",
                "Biochar"="#6A994E", "Halogenated analogs"="#F4A261",
                "3-NOP combination"="#332288", "Microbial additives"="#CC6677", "Nitrate"="#88CCEE")
lab_cols  <- c(Control = "#1F78B4", Treatment = "#F8766D")   # blue / coral as in your PCoAs
sig_cols  <- c("FDR < 0.05" = "#D55E00", "Not significant" = "#1F78B4")
d$Strategy_Group <- factor(d$Strategy_Group, levels = names(strat_cols))
d <- d %>% filter(!is.na(Strategy_Group))
theme_set(theme_bw(base_size = 11))

