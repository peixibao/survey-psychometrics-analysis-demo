project_files <- c(
  "scripts/00_check_packages.R",
  "scripts/01_generate_synthetic_data.R",
  "scripts/02_response_quality_checks.R",
  "scripts/03_item_descriptives.R",
  "scripts/04_exploratory_factor_analysis.R",
  "scripts/05_cooccurrence_clustering.R",
  "scripts/06_fdr_associations.R",
  "scripts/07_figures.R"
)

for (script in project_files) {
  message("\n--- Running ", script, " ---")
  source(script, echo = FALSE)
}

writeLines(
  capture.output(sessionInfo()),
  "output/session_info.txt"
)

message(
  "\nComplete workflow finished successfully.\n",
  "See data/ for generated datasets and output/ for analysis results and figures."
)
