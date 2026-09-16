## scripts/40_descriptive_statistics.R
## MI-pooled weighted descriptives

# load designs (ensure this matches where you saved them)
svy_design_list <- readRDS(file.path(DIR_INTERIM, "svy_list.rds"))

 
# Analyze all subgroups using the utility functions
all_results_df <- analyze_subgroup(des_var_order, svy_design_list, var_order = des_var_order)
 

white_results_df <- analyze_subgroup(des_var_order, svy_design_list, 
                                     subset_condition = 'race %in% c("white")', 
                                     var_order = des_var_order)

black_results_df <- analyze_subgroup(des_var_order, svy_design_list, 
                                     subset_condition = 'race %in% c("black")', 
                                     var_order = des_var_order)

black_light_results_df <- analyze_subgroup(des_var_order, svy_design_list, 
                                           subset_condition = 'race_skin %in% c("light_black")', 
                                           var_order = des_var_order)

black_dark_results_df <- analyze_subgroup(des_var_order, svy_design_list, 
                                          subset_condition = 'race_skin %in% c("dark_black")', 
                                          var_order = des_var_order)


# Combine all results
combined_df <- combine_subgroup_results(
  des_var_labels,
  all = all_results_df,
  white = white_results_df,
  black = black_results_df,
  light_black = black_light_results_df,
  dark_black = black_dark_results_df
)

 
# Combine the data frames using cbind and rename the columns
combined_df <- cbind(
  as.vector(rbind(des_var_labels, "")), 
  all_results_df$vals,
  white_results_df$vals,
  black_results_df$vals,
  black_light_results_df$vals,
  black_dark_results_df$vals
)

colnames(combined_df) <- c(
  "variable",
  "all",
  "white",
  "black",
  "light_black",
  "dark_black"
)

# Create sample counts
analysis_base_imp <- readRDS(file.path(DIR_INTERIM, "analysis_base_imp1.rds"))

count_data <- analysis_base_imp %>% 
  filter(race == "white" | race == "black") %>%
  drop_na(skin_tone_scale) %>% 
  filter(hs_completed == 1) %>% 
  drop_na("compcoll25") %>% 
  drop_na("hiskil19") %>%   
  drop_na("covid_jobloss")

counts <- create_sample_counts(count_data)

 

# Final combined data frame
des_df <- rbind(combined_df, counts)



# Save results

out_dir <- here::here(DIR_RESULTS)   # DIR_INTERIM set in 00_setup.R
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
 
rds_file <- file.path(out_dir, "des_df.rds")
 
saveRDS(des_df, rds_file)

message("[des_df] Saved: ", rds_file)
