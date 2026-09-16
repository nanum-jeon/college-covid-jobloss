## ---------- Covid job loss non imputed---------- ## 


# ---- Load data --------------------------------------------------------------
rds_in <- file.path(DIR_RESULTS, "covid_jobloss_non_imputed.rds")
if (!file.exists(rds_in)) {
  stop("Input RDS not found: ", rds_in)
}
covid_jobloss_non_imputed <- readRDS(rds_in)

# ---- Main analysis ----------------------------------------------------------
 
covid_jobloss_non_imputed_results <- run_regression_analysis_non_imputed(
  covid_jobloss_non_imputed,
  fit_main_models_non_imputed
)

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_jobloss_non_imputed_results.rds")
saveRDS(covid_jobloss_non_imputed_results, rds_out)
message("Saved combined results to: ", rds_out)

