# scripts/43_descriptive_statistics_covid_jobloss_intersectional.R 

# --- Load & preprocess designs ------------------------------------------
svy_design_list <- readRDS(file.path(DIR_INTERIM, "svy_list.rds"))

### ------- Women ----------

# subset inside each imputed design
female1_covid_jobloss_design_list <- lapply(svy_design_list, function(des) {
  des <- update(des, race = factor(race, levels = c("white","black")))
  subset(
    des,
    race %in% c("white","black") &
      hs_completed == 1 &
      female == 1 &
      !is.na(skin_tone_scale) &
      !is.na(compcoll25) &
      !is.na(hiskil19) &
      !is.na(covid_jobloss)
  )
})


# --- Compute all scenarios (9 tables) -----------------------------------

# 1) Outcome by Treatment (marginal in mediator)
female1_covid_jobloss_all         <- pool_mean_by(~covid_jobloss, ~compcoll25, design_list = female1_covid_jobloss_design_list)
female1_covid_jobloss_race        <- pool_mean_by(~covid_jobloss, ~compcoll25 + race, design_list = female1_covid_jobloss_design_list)
female1_covid_jobloss_race_skin   <- pool_mean_by(~covid_jobloss, ~compcoll25 + race_skin_debiased,
                                               subset_expr = quote(race_skin_debiased != "white"), design_list = female1_covid_jobloss_design_list)

# 2) Outcome by Treatment and Mediator (conditional)
female1_covid_jobloss_cond_all       <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19, design_list = female1_covid_jobloss_design_list)
female1_covid_jobloss_cond_race      <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race, design_list = female1_covid_jobloss_design_list)
female1_covid_jobloss_cond_race_skin <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race_skin_debiased,
                                                  subset_expr = quote(race_skin_debiased != "white"), design_list = female1_covid_jobloss_design_list)

# --- Normalize/stack -----------------------------------------------------
 
female1_covid_jobloss_results_list <- list( 
  normalize_result(female1_covid_jobloss_all,               "outcome_marginal",    group_value = "all",        fix_highskill = NA),
  normalize_result(female1_covid_jobloss_cond_all,          "outcome_conditional", group_value = "all"),
  
  normalize_result(female1_covid_jobloss_race,              "outcome_marginal",    group_col = "race",         fix_highskill = NA),
  normalize_result(female1_covid_jobloss_cond_race,         "outcome_conditional", group_col = "race"),
  
  normalize_result(female1_covid_jobloss_race_skin,         "outcome_marginal",    group_col = "race_skin_debiased",    fix_highskill = NA),
  normalize_result(female1_covid_jobloss_cond_race_skin,    "outcome_conditional", group_col = "race_skin_debiased")
)


combined_female1_covid_jobloss_results <- bind_rows(female1_covid_jobloss_results_list)

combined_female1_covid_jobloss_results <- combined_female1_covid_jobloss_results %>% 
  mutate(analysis = factor(analysis, 
                           levels = c("outcome_marginal", "outcome_conditional")))

combined_female1_covid_jobloss_results %>%
  print(n = 30)


# --- Save & preview ------------------------------------------------------ 
rds_path <- file.path(DIR_RESULTS, "pooled_means_college_skill_female1_covid_jobloss.rds")

saveRDS(combined_female1_covid_jobloss_results, rds_path)

message("Saved: \n  - ", rds_path)


### ------- Men ----------
 

# subset inside each imputed design
female0_covid_jobloss_design_list <- lapply(svy_design_list, function(des) {
  des <- update(des, race = factor(race, levels = c("white","black")))
  subset(
    des,
    race %in% c("white","black") &
      hs_completed == 1 &
      female == 0 &
      !is.na(skin_tone_scale) &
      !is.na(compcoll25) &
      !is.na(hiskil19) &
      !is.na(covid_jobloss)
  )
})


# --- Compute all scenarios (9 tables) -----------------------------------

# 1) Outcome by Treatment (marginal in mediator)
female0_covid_jobloss_all         <- pool_mean_by(~covid_jobloss, ~compcoll25, design_list = female0_covid_jobloss_design_list)
female0_covid_jobloss_race        <- pool_mean_by(~covid_jobloss, ~compcoll25 + race, design_list = female0_covid_jobloss_design_list)
female0_covid_jobloss_race_skin   <- pool_mean_by(~covid_jobloss, ~compcoll25 + race_skin_debiased,
                                                  subset_expr = quote(race_skin_debiased != "white"), design_list = female0_covid_jobloss_design_list)

# 2) Outcome by Treatment and Mediator (conditional)
female0_covid_jobloss_cond_all       <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19, design_list = female0_covid_jobloss_design_list)
female0_covid_jobloss_cond_race      <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race, design_list = female0_covid_jobloss_design_list)
female0_covid_jobloss_cond_race_skin <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race_skin_debiased,
                                                     subset_expr = quote(race_skin_debiased != "white"), design_list = female0_covid_jobloss_design_list)

# --- Normalize/stack -----------------------------------------------------

female0_covid_jobloss_results_list <- list( 
  normalize_result(female0_covid_jobloss_all,               "outcome_marginal",    group_value = "all",        fix_highskill = NA),
  normalize_result(female0_covid_jobloss_cond_all,          "outcome_conditional", group_value = "all"),
  
  normalize_result(female0_covid_jobloss_race,              "outcome_marginal",    group_col = "race",         fix_highskill = NA),
  normalize_result(female0_covid_jobloss_cond_race,         "outcome_conditional", group_col = "race"),
  
  normalize_result(female0_covid_jobloss_race_skin,         "outcome_marginal",    group_col = "race_skin_debiased",    fix_highskill = NA),
  normalize_result(female0_covid_jobloss_cond_race_skin,    "outcome_conditional", group_col = "race_skin_debiased")
)


combined_female0_covid_jobloss_results <- bind_rows(female0_covid_jobloss_results_list)

combined_female0_covid_jobloss_results <- combined_female0_covid_jobloss_results %>% 
  mutate(analysis = factor(analysis, 
                           levels = c("outcome_marginal", "outcome_conditional")))

combined_female0_covid_jobloss_results %>%
  print(n = 30)


# --- Save & preview ------------------------------------------------------ 
rds_path <- file.path(DIR_RESULTS, "pooled_means_college_skill_female0_covid_jobloss.rds")

saveRDS(combined_female0_covid_jobloss_results, rds_path)

message("Saved: \n  - ", rds_path)
