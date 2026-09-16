# scripts/40_mediation_jobloss.R


# Load imputed data
merged_list <- readRDS(file.path(DIR_INTERIM, "merged_list.rds"))
 
# Fit the models and produce results :
covid_jobloss_alt_moderator_list <- fit_mediation_models(
  data_list = merged_list,
  avar_name = "compcoll25",
  mvar_name = "hiskil19",
  yvar_name = "covid_jobloss",
  xvars = exprs(
    race_white, skin_tone_debiased, 
    female, maeduc, faeduc, parinc,
    rural_12, south_12, intact_12, sibsz,
    asvab_pst, hs_gpa, col_prep,
    delinq, subs_use, fight,
    cohabited_18, child_18,
    tchgd, schsafe,
    stolen, threatened,
    pct_peer_about75, pct_peer_more_than90
  )
)

# Save raw results  
rds_path <- file.path(DIR_RESULTS, "covid_jobloss_alt_moderator_list.rds")

saveRDS(covid_jobloss_alt_moderator_list, rds_path)

message("Saved: \n  - ", rds_path)

 
 
 
  