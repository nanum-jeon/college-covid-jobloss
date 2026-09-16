# scripts/50_covid_jobloss_results.R
# ------------------------------------------------------------
# Runs main + mechanism analyses, formats outputs, and saves.
# ------------------------------------------------------------

# Ensure results directory exists
dir.create(DIR_RESULTS, showWarnings = FALSE, recursive = TRUE)

# ---- Load data --------------------------------------------------------------
rds_in <- file.path(DIR_RESULTS, "covid_jobloss_list.rds")
if (!file.exists(rds_in)) {
  stop("Input RDS not found: ", rds_in)
}
covid_jobloss_list <- readRDS(rds_in)

# ---- Main analysis ----------------------------------------------------------
covid_jobloss_main_results <- run_regression_analysis(
  covid_jobloss_list, 
  fit_main_models
)
covid_jobloss_main_results <- format_main_results(covid_jobloss_main_results)

 
# ---- Mechanism analysis -----------------------------------------------------
covid_jobloss_mechanism_results <- run_mechanism_analysis(
  covid_jobloss_list, 
  fit_mechanism_models
)
 
 
covid_jobloss_mechanism_results <- format_mechanism_results(
  covid_jobloss_mechanism_results
)
 
# ---- Combine results --------------------------------------------------------
covid_jobloss_results <- list(
  main_results      = covid_jobloss_main_results,
  mechanism_results = covid_jobloss_mechanism_results
)


# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_jobloss_results.rds")
saveRDS(covid_jobloss_results, rds_out)
message("Saved combined results to: ", rds_out)
