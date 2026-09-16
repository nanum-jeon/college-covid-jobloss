# scripts/45_analysis_non_imputed

non_imputed <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_non_imputed.rds"))

# 2. Run the analysis
non_imputed_results <- run_regression_analysis_non_imputed(
  df = non_imputed, 
  models_function = fit_main_models_non_imputed
)

covid_jobloss_non_imputed_results <- format_all_results(non_imputed_results) 

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_jobloss_non_imputed_results.rds")
saveRDS(covid_jobloss_non_imputed_results, rds_out)
message("Saved combined results to: ", rds_out)

