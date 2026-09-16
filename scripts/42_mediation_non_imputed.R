# scripts/42_mediation_non_imputed.R


# Load imputed data
non_imputed <- readRDS(file.path(DIR_INTERIM, "non_imputed.rds"))


# Fit the models and produce results :
covid_jobloss_non_imputed <- fit_non_imputed_mediation_models(
  data = non_imputed,
  avar_name = "compcoll25",
  mvar_name = "hiskil19",
  yvar_name = "covid_jobloss"
)

# Save raw results  
rds_path <- file.path(DIR_RESULTS, "covid_jobloss_non_imputed.rds")

saveRDS(covid_jobloss_non_imputed, rds_path)

message("Saved: \n  - ", rds_path)

covid_jobloss_non_imputed 

 
