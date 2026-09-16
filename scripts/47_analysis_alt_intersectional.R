# scripts/52_covid_displacement_intersectional_results.R
# ------------------------------------------------------------
# Runs main + mechanism analyses, formats outputs, and saves.
# ------------------------------------------------------------

# ---- Load data --------------------------------------------------------------
rds_in <- file.path(DIR_RESULTS, "covid_displacement_list.rds")
if (!file.exists(rds_in)) {
  stop("Input RDS not found: ", rds_in)
}
covid_displacement_list <- readRDS(rds_in)


covid_displacement_intersectional_analysis_results <- run_regression_analysis(
  covid_displacement_list, 
  fit_intersectional_models
)


# ---- Female 1/0 formatting ---------------------

covid_displacement_female1_results <- format_female1_results(covid_displacement_intersectional_analysis_results)
covid_displacement_female0_results <- format_female0_results(covid_displacement_intersectional_analysis_results)

covid_displacement_female1_results
covid_displacement_female0_results



# ---- Combine results --------------------------------------------------------
covid_displacement_intersectional_results <- list(
  female1_results = covid_displacement_female1_results,
  female0_results = covid_displacement_female0_results
)

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_displacement_intersectional_results.rds")
saveRDS(covid_displacement_intersectional_results, rds_out)
message("Saved combined results to: ", rds_out)
