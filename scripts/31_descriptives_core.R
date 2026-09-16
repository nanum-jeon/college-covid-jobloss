# scripts/30_descriptives_core.R 


# --- Load & preprocess designs ------------------------------------------
svy_design_list <- readRDS(file.path(DIR_INTERIM, "svy_list.rds"))

# subset inside each imputed design
subset_design_list <- lapply(svy_design_list, function(des) {
  des <- update(des, race = factor(race, levels = c("white","black")))
  subset(
    des,
    race %in% c("white","black") &
      hs_completed == 1 &
      !is.na(skin_tone_scale) &
      !is.na(compcoll25) &
      !is.na(hiskil19) &
      !is.na(covid_jobloss)
  )
})
 

# --- Compute all scenarios (9 tables) -----------------------------------

# 1) Mediator by Treatment
mediator_all        <- pool_mean_by(~hiskil19, ~compcoll25, design_list = subset_design_list)
mediator_race       <- pool_mean_by(~hiskil19, ~compcoll25 + race, design_list = subset_design_list)
mediator_race_skin  <- pool_mean_by(~hiskil19, ~compcoll25 + race_skin,
                                    subset_expr = quote(race_skin != "white"), design_list = subset_design_list)

# 2) Outcome by Treatment (marginal in mediator)
outcome_all         <- pool_mean_by(~covid_jobloss, ~compcoll25, design_list = subset_design_list)
outcome_race        <- pool_mean_by(~covid_jobloss, ~compcoll25 + race, design_list = subset_design_list)
outcome_race_skin   <- pool_mean_by(~covid_jobloss, ~compcoll25 + race_skin,
                                    subset_expr = quote(race_skin != "white"), design_list = subset_design_list)

# 3) Outcome by Treatment and Mediator (conditional)
outcome_cond_all       <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19, design_list = subset_design_list)
outcome_cond_race      <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race, design_list = subset_design_list)
outcome_cond_race_skin <- pool_mean_by(~covid_jobloss, ~compcoll25 + hiskil19 + race_skin,
                                       subset_expr = quote(race_skin != "white"), design_list = subset_design_list)


# 4) Outcome by Moderator (by Race and Skion Color)
outcome_race_only      <- pool_mean_by(~covid_jobloss, ~race, design_list = subset_design_list)
outcome_race_skin_only <- pool_mean_by(~covid_jobloss, ~race_skin,
                                       subset_expr = quote(race_skin != "white"), design_list = subset_design_list)


# --- Normalize/stack -----------------------------------------------------

results_list <- list(
  normalize_result(mediator_all,              "mediator",            group_value = "all",        fix_highskill = 1),
  normalize_result(outcome_all,               "outcome_marginal",    group_value = "all",        fix_highskill = NA),
  normalize_result(outcome_cond_all,          "outcome_conditional", group_value = "all"),
  
  normalize_result(mediator_race,             "mediator",            group_col = "race",         fix_highskill = 1),
  normalize_result(outcome_race,              "outcome_marginal",    group_col = "race",         fix_highskill = NA),
  normalize_result(outcome_cond_race,         "outcome_conditional", group_col = "race"),
  
  normalize_result(mediator_race_skin,        "mediator",            group_col = "race_skin",    fix_highskill = 1),
  normalize_result(outcome_race_skin,         "outcome_marginal",    group_col = "race_skin",    fix_highskill = NA),
  normalize_result(outcome_cond_race_skin,    "outcome_conditional", group_col = "race_skin"),
  
  normalize_result(outcome_race_only,         "outcome_moderator",    group_col = "race",    fix_highskill = NA, fix_college = NA),
  normalize_result(outcome_race_skin_only,    "outcome_moderator", group_col = "race_skin", fix_highskill = NA, fix_college = NA)
)

combined_results <- bind_rows(results_list)

combined_results <- combined_results %>% 
  mutate(analysis = factor(analysis, 
                           levels = c("mediator", "outcome_marginal", "outcome_conditional", "outcome_moderator")))

combined_results %>%
  print(n = 100)


# --- Save & preview ------------------------------------------------------
rds_path <- file.path(DIR_RESULTS, "pooled_means_college_skill_covid.rds")
 
saveRDS(combined_results, rds_path)

message("Saved:\n  - ",  rds_path)