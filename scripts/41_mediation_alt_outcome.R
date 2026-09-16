# scripts/41_mediation_alt_outcome.R


## Covid Displacement 
# Load imputed data
merged_list <- readRDS(file.path(DIR_INTERIM, "merged_list.rds"))


# Fit the models and produce results :
covid_displacement_list <- fit_mediation_models(
  data_list = merged_list,
  avar_name = "compcoll25",
  mvar_name = "hiskil19",
  yvar_name = "covid_displacement"
)

# Save raw results  
rds_path <- file.path(DIR_RESULTS, "covid_displacement_list.rds")

saveRDS(covid_displacement_list, rds_path)

message("Saved: \n  - ", rds_path)




