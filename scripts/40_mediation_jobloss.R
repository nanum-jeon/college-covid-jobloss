# scripts/40_mediation_jobloss.R


# Load imputed data
merged_list <- readRDS(file.path(DIR_INTERIM, "merged_list.rds"))
 
# Fit the models and produce results :
covid_jobloss_list <- fit_mediation_models(
  data_list = merged_list,
  avar_name = "compcoll25",
  mvar_name = "hiskil19",
  yvar_name = "covid_jobloss"
)

# Save raw results  
rds_path <- file.path(DIR_RESULTS, "covid_jobloss_list.rds")

saveRDS(covid_jobloss_list, rds_path)

message("Saved: \n  - ", rds_path)

 
 
 
  