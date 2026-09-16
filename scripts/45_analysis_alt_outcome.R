# scripts/45_analysis_alt_outcome.R
# ------------------------------------------------------------
# Runs main + mechanism analyses, formats outputs, and saves.
# ------------------------------------------------------------

## ---------- Covid Displacement ---------- ## 

# ---- Load data --------------------------------------------------------------
rds_in <- file.path(DIR_RESULTS, "covid_displacement_list.rds")
if (!file.exists(rds_in)) {
  stop("Input RDS not found: ", rds_in)
}
covid_displacement_list <- readRDS(rds_in)

# ---- Main analysis ----------------------------------------------------------
covid_displacement_main_results <- run_regression_analysis(
  covid_displacement_list, 
  fit_main_models
)
covid_displacement_main_results <- format_main_results(covid_displacement_main_results)

# ---- Mechanism analysis -----------------------------------------------------
covid_displacement_mechanism_results <- run_mechanism_analysis(
  covid_displacement_list, 
  fit_mechanism_models
)
covid_displacement_mechanism_results <- format_mechanism_results(
  covid_displacement_mechanism_results
)

# ---- Combine results --------------------------------------------------------
covid_displacement_results <- list(
  main_results      = covid_displacement_main_results,
  mechanism_results = covid_displacement_mechanism_results
)

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_displacement_results.rds")
saveRDS(covid_displacement_results, rds_out)
message("Saved combined results to: ", rds_out)



## ----------Covid Disruption ---------- ## 


# ---- Load data --------------------------------------------------------------
rds_in <- file.path(DIR_RESULTS, "covid_disruption_list.rds")
if (!file.exists(rds_in)) {
  stop("Input RDS not found: ", rds_in)
}
covid_disruption_list <- readRDS(rds_in)

# ---- Main analysis ----------------------------------------------------------
covid_disruption_main_results <- run_regression_analysis(
  covid_disruption_list, 
  fit_main_models
)
covid_disruption_main_results <- format_main_results(covid_disruption_main_results)

# ---- Mechanism analysis -----------------------------------------------------
covid_disruption_mechanism_results <- run_mechanism_analysis(
  covid_disruption_list, 
  fit_mechanism_models
)
covid_disruption_mechanism_results <- format_mechanism_results(
  covid_disruption_mechanism_results
)

# ---- Combine results --------------------------------------------------------
covid_disruption_results <- list(
  main_results      = covid_disruption_main_results,
  mechanism_results = covid_disruption_mechanism_results
)

# ---- Save -------------------------------------------------------------------
rds_out <- file.path(DIR_RESULTS, "covid_disruption_results.rds")
saveRDS(covid_disruption_results, rds_out)
message("Saved combined results to: ", rds_out)
