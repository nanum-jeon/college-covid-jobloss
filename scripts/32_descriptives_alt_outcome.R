# scripts/42_descriptive_statistics_covid_displacement.R 

# --- Load & preprocess designs ------------------------------------------
svy_design_list <- readRDS(file.path(DIR_INTERIM, "svy_list.rds"))

# subset inside each imputed design
covid_displacement_design_list <- lapply(svy_design_list, function(des) {
  des <- update(des, race = factor(race, levels = c("white","black")))
  subset(
    des,
    race %in% c("white","black") &
      hs_by20 == 1 &
      !is.na(skin_tone_scale) &
      !is.na(compcoll25) &
      !is.na(hiskil19) &
      !is.na(covid_displacement)
  )
})


# --- Compute all scenarios (9 tables) -----------------------------------

# 1) Outcome by Treatment (marginal in mediator)
covid_displacement_all         <- pool_mean_by(~covid_displacement, ~compcoll25, design_list = covid_displacement_design_list)
covid_displacement_race        <- pool_mean_by(~covid_displacement, ~compcoll25 + race, design_list = covid_displacement_design_list)
covid_displacement_race_skin   <- pool_mean_by(~covid_displacement, ~compcoll25 + race_skin,
                                    subset_expr = quote(race_skin != "white"), design_list = covid_displacement_design_list)

# 2) Outcome by Treatment and Mediator (conditional)
covid_displacement_cond_all       <- pool_mean_by(~covid_displacement, ~compcoll25 + hiskil19, design_list = covid_displacement_design_list)
covid_displacement_cond_race      <- pool_mean_by(~covid_displacement, ~compcoll25 + hiskil19 + race, design_list = covid_displacement_design_list)
covid_displacement_cond_race_skin <- pool_mean_by(~covid_displacement, ~compcoll25 + hiskil19 + race_skin,
                                       subset_expr = quote(race_skin != "white"), design_list = covid_displacement_design_list)

# --- Normalize/stack -----------------------------------------------------

 

covid_displacement_results_list <- list( 
  normalize_result(covid_displacement_all,               "outcome_marginal",    group_value = "all",        fix_highskill = NA),
  normalize_result(covid_displacement_cond_all,          "outcome_conditional", group_value = "all"),
   
  normalize_result(covid_displacement_race,              "outcome_marginal",    group_col = "race",         fix_highskill = NA),
  normalize_result(covid_displacement_cond_race,         "outcome_conditional", group_col = "race"),
   
  normalize_result(covid_displacement_race_skin,         "outcome_marginal",    group_col = "race_skin",    fix_highskill = NA),
  normalize_result(covid_displacement_cond_race_skin,    "outcome_conditional", group_col = "race_skin")
)
 

combined_covid_displacement_results <- bind_rows(covid_displacement_results_list)

combined_covid_displacement_results <- combined_covid_displacement_results %>% 
  mutate(analysis = factor(analysis, 
                           levels = c("outcome_marginal", "outcome_conditional")))

combined_covid_displacement_results %>%
  print(n = 30)


# --- Save & preview ------------------------------------------------------ 
rds_path <- file.path(DIR_RESULTS, "pooled_means_college_skill_covid_displacement.rds")
 
saveRDS(combined_covid_displacement_results, rds_path)

message("Saved: \n  - ", rds_path)
