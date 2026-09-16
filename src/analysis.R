# ====================
# SURVEY ANALYSIS 
# ====================
 
 
#' Run survey analysis on results
#' @param results_list Results from process_multiple_imputations

# Helper function to fit main models for one outcome
 
fit_main_models <- function(imp, outcome_var) {
  
  # Main effects and interactions
  models <- list( 
    race_skin_white = with(imp, lm(get(outcome_var) ~ race_skin)), 
    race_skin_light = with(imp, lm(get(outcome_var) ~ relevel(race_skin, ref = "light_black"))) 
  )
  
  # Group means - define all subgroups
  subgroups <- list(
    white_vs_zero       = quote(race == "white"),
    black_vs_zero       = quote(race == "black"),
    light_black_vs_zero = quote(race_skin == "light_black"),
    dark_black_vs_zero  = quote(race_skin == "dark_black")
  )
  
  # Fit subgroup models
  subgroup_models <- lapply(subgroups, function(condition) {
    with(imp, lm(get(outcome_var) ~ 1, subset = eval(condition)))
  })
  
  # Combine all models
  all_models <- c(models, subgroup_models)
  
  # Pool results 
  lapply(all_models, function(model) summary(pool(model)))
}


# Main streamlined function - now uses the models_function parameter
run_regression_analysis <- function(results_list, models_function) {
  imp <- imputationList(results_list)
  
  # Fit models for all three outcomes using the specified function
  outcomes <- c("ate_eif", "nde_eif", "nie_eif")
  results <- setNames(
    lapply(outcomes, function(outcome) models_function(imp, outcome)),
    c("ate", "nde", "nie")
  )
  
  # Combine results
  c(results, list(imp = imp))
}


# Function to extract and format results into clean tables
format_main_results <- function(analysis_results) {
  
  # Helper function to extract estimate, se, and p-value from pooled results
  extract_stats <- function(pooled_summary, row_name = "(Intercept)") {
    if (is.null(pooled_summary) || nrow(pooled_summary) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    row_idx <- which(pooled_summary$term == row_name)
    if (length(row_idx) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    data.frame(
      estimate = pooled_summary$estimate[row_idx],
      se = pooled_summary$std.error[row_idx],
      p_value = pooled_summary$p.value[row_idx]
    )
  }
  
  # Create results table
  results_table <- data.frame(
    Effect = character(),
    Group = character(),
    Estimate = numeric(),
    SE = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )

  # Extract NDE results
  nde_white <- extract_stats(analysis_results$nde$white_vs_zero) 
  nde_black <- extract_stats(analysis_results$nde$black_vs_zero) 
  nde_light_black <- extract_stats(analysis_results$nde$light_black_vs_zero)
  nde_dark_black <- extract_stats(analysis_results$nde$dark_black_vs_zero)
  
  # Extract NIE results
  nie_white <- extract_stats(analysis_results$nie$white_vs_zero) 
  nie_black <- extract_stats(analysis_results$nie$black_vs_zero) 
  nie_light_black <- extract_stats(analysis_results$nie$light_black_vs_zero)
  nie_dark_black <- extract_stats(analysis_results$nie$dark_black_vs_zero)
  
  
  # Extract ATE results
  ate_white <- extract_stats(analysis_results$ate$white_vs_zero) 
  ate_black <- extract_stats(analysis_results$ate$black_vs_zero)
  ate_light_black <- extract_stats(analysis_results$ate$light_black_vs_zero)
  ate_dark_black <- extract_stats(analysis_results$ate$dark_black_vs_zero)
  
  
  # Combine into final table
  results_table <- rbind(
    
    data.frame(Effect = "Total (ATE)", Group = "White", ate_white), 
    data.frame(Effect = "Total (ATE)", Group = "Black", ate_black),
    data.frame(Effect = "Total (ATE)", Group = "Light_Black", ate_light_black),
    data.frame(Effect = "Total (ATE)", Group = "Dark_Black", ate_dark_black),
    
    data.frame(Effect = "Indirect (NIE)", Group = "White", nie_white), 
    data.frame(Effect = "Indirect (NIE)", Group = "Black", nie_black),
    data.frame(Effect = "Indirect (NIE)", Group = "Light_Black", nie_light_black),
    data.frame(Effect = "Indirect (NIE)", Group = "Dark_Black", nie_dark_black),
    
    
    data.frame(Effect = "Direct (NDE)", Group = "White", nde_white), 
    data.frame(Effect = "Direct (NDE)", Group = "Black", nde_black), 
    data.frame(Effect = "Direct (NDE)", Group = "Light_Black", nde_light_black),
    data.frame(Effect = "Direct (NDE)", Group = "Dark_Black", nde_dark_black)
   
    )
  
  # Clean column names
  names(results_table) <- c("Effect", "Group", "Estimate", "SE", "P_Value")
  
  # Add significance stars
  results_table$Sig <- ifelse(results_table$P_Value < 0.001, "***",
                              ifelse(results_table$P_Value < 0.01,  "**",
                                     ifelse(results_table$P_Value < 0.05,  "*", "")))
  
  
  # Round numeric columns
  results_table$Estimate <- round(results_table$Estimate, 4)
  results_table$SE <- round(results_table$SE, 4)
  results_table$P_Value <- round(results_table$P_Value, 4)
  
  return(results_table)
}


 
# Function to format the table with stars and standard errors
create_regression_table <- function(data, group_var = "Group") {
  data %>%
    pivot_longer(cols = c(Estimate, SE), names_to = "type", values_to = "value") %>%
    mutate(
      final = case_when(
        (Sig != "" & type == "Estimate") ~ paste0(sprintf("%.3f", value), "$^{" , Sig, "}$"), 
        (Sig == "" & type == "Estimate") ~ paste0(sprintf("%.3f", value)), 
        (type == "SE") ~ paste0("(", sprintf("%.3f", value), ")")
      )
    ) %>%
    select(Effect, type, final, !!sym(group_var)) %>%
    pivot_wider(names_from = !!sym(group_var), values_from = final) %>% 
    mutate(
      Effect = case_when(type == "SE" ~ "", 
                           type != "SE" ~ Effect)
    ) %>%  
    select(Effect, White, Black, Light_Black, Dark_Black)
}


## ---- Mechanism analysis ------ 

# Mechanism analysis 
fit_mechanism_models <- function(imp, outcome_var) {
  
  # Group means - define all subgroups
  subgroups <- list(
    white_vs_zero       = quote(race == "white"),
    black_vs_zero       = quote(race == "black"),
    light_black_vs_zero = quote(race_skin == "light_black"),
    dark_black_vs_zero  = quote(race_skin == "dark_black")
  )
  
  # Fit subgroup models
  subgroup_models <- lapply(subgroups, function(condition) {
    with(imp, lm(get(outcome_var) ~ 1, subset = eval(condition)))
  })
  
  
  # Pool results
  lapply(subgroup_models, function(model) summary(pool(model)))
}


run_mechanism_analysis <- function(results_list, models_function) {
  imp <- imputationList(results_list)
  
  # Fit models for all three outcomes using the specified function
  outcomes <- c("ps", "cem", "atm_eif", "med_eif")
  results <- setNames(
    lapply(outcomes, function(outcome) models_function(imp, outcome)),
    c("ps", "cem", "atm", "med")
  )
  
  # Combine results
  c(results, list(imp = imp))
}


format_mechanism_results <- function(analysis_results) {
  
  # Helper function to extract estimate, se, and p-value from pooled results
  extract_stats <- function(pooled_summary, row_name = "(Intercept)") {
    if (is.null(pooled_summary) || nrow(pooled_summary) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    row_idx <- which(pooled_summary$term == row_name)
    if (length(row_idx) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    data.frame(
      estimate = pooled_summary$estimate[row_idx],
      se = pooled_summary$std.error[row_idx],
      p_value = pooled_summary$p.value[row_idx]
    )
  }
  
  # Create results table
  results_table <- data.frame(
    Effect = character(),
    Group = character(),
    Estimate = numeric(),
    SE = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract PS results
  ps_white <- extract_stats(analysis_results$ps$white_vs_zero) 
  ps_black <- extract_stats(analysis_results$ps$black_vs_zero) 
  ps_light_black <- extract_stats(analysis_results$ps$light_black_vs_zero)
  ps_dark_black <- extract_stats(analysis_results$ps$dark_black_vs_zero)
  
  # Extract CEM results
  cem_white <- extract_stats(analysis_results$cem$white_vs_zero) 
  cem_black <- extract_stats(analysis_results$cem$black_vs_zero)
  cem_light_black <- extract_stats(analysis_results$cem$light_black_vs_zero)
  cem_dark_black <- extract_stats(analysis_results$cem$dark_black_vs_zero)
  
  # Extract ATM results
  atm_white <- extract_stats(analysis_results$atm$white_vs_zero) 
  atm_black <- extract_stats(analysis_results$atm$black_vs_zero)
  atm_light_black <- extract_stats(analysis_results$atm$light_black_vs_zero)
  atm_dark_black <- extract_stats(analysis_results$atm$dark_black_vs_zero)
  
  # Extract MED results
  med_white <- extract_stats(analysis_results$med$white_vs_zero) 
  med_black <- extract_stats(analysis_results$med$black_vs_zero)
  med_light_black <- extract_stats(analysis_results$med$light_black_vs_zero)
  med_dark_black <- extract_stats(analysis_results$med$dark_black_vs_zero)
  
  # Combine into final table
  results_table <- rbind(
    data.frame(Effect = "PS", Group = "White", ps_white), 
    data.frame(Effect = "PS", Group = "Black", ps_black),
    data.frame(Effect = "PS", Group = "Light_Black", ps_light_black),
    data.frame(Effect = "PS", Group = "Dark_Black", ps_dark_black),
    
    data.frame(Effect = "CEM", Group = "White", cem_white), 
    data.frame(Effect = "CEM", Group = "Black", cem_black), 
    data.frame(Effect = "CEM", Group = "Light_Black", cem_light_black),
    data.frame(Effect = "CEM", Group = "Dark_Black", cem_dark_black),
    
    data.frame(Effect = "ATM", Group = "White", atm_white), 
    data.frame(Effect = "ATM", Group = "Black", atm_black),
    data.frame(Effect = "ATM", Group = "Light_Black", atm_light_black),
    data.frame(Effect = "ATM", Group = "Dark_Black", atm_dark_black), 
    
    data.frame(Effect = "MED", Group = "White", med_white), 
    data.frame(Effect = "MED", Group = "Black", med_black),
    data.frame(Effect = "MED", Group = "Light_Black", med_light_black),
    data.frame(Effect = "MED", Group = "Dark_Black", med_dark_black)

  )
  
  # Clean column names
  names(results_table) <- c("Effect", "Group", "Estimate", "SE", "P_Value")
   
  # Add significance stars
  results_table$Sig <- ifelse(results_table$P_Value < 0.001, "***",
                              ifelse(results_table$P_Value < 0.01,  "**",
                                     ifelse(results_table$P_Value < 0.05,  "*", "")))
  
  # Round numeric columns
  results_table$Estimate <- round(results_table$Estimate, 4)
  results_table$SE <- round(results_table$SE, 4)
  results_table$P_Value <- round(results_table$P_Value, 4)
  
  return(results_table)
}

## ---- intersectional analysis ------ 

# gender analysis 
fit_intersectional_models <- function(imp, outcome_var) {
  
  # Group means - define all subgroups
  subgroups <- list(
    white_female1_vs_zero       = quote(race == "white" & female == 1),
    black_female1_vs_zero       = quote(race == "black" & female == 1),
    light_black_female1_vs_zero = quote(race_skin == "light_black" & female == 1),
    dark_black_female1_vs_zero  = quote(race_skin == "dark_black" & female == 1),
    white_female0_vs_zero       = quote(race == "white" & female == 0),
    black_female0_vs_zero       = quote(race == "black" & female == 0),
    light_black_female0_vs_zero = quote(race_skin == "light_black" & female == 0),
    dark_black_female0_vs_zero  = quote(race_skin == "dark_black" & female == 0)
  )
  
  # Fit subgroup models
  subgroup_models <- lapply(subgroups, function(condition) {
    with(imp, lm(get(outcome_var) ~ 1, subset = eval(condition)))
  })
  
  
  # Pool results
  lapply(subgroup_models, function(model) summary(pool(model)))
}


format_female1_results <- function(analysis_results) {
  
  # Helper function to extract estimate, se, and p-value from pooled results
  extract_stats <- function(pooled_summary, row_name = "(Intercept)") {
    if (is.null(pooled_summary) || nrow(pooled_summary) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    row_idx <- which(pooled_summary$term == row_name)
    if (length(row_idx) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    data.frame(
      estimate = pooled_summary$estimate[row_idx],
      se = pooled_summary$std.error[row_idx],
      p_value = pooled_summary$p.value[row_idx]
    )
  }
  
  # Create results table
  results_table <- data.frame(
    Effect = character(),
    Group = character(),
    Estimate = numeric(),
    SE = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract NDE results
  nde_white <- extract_stats(analysis_results$nde$white_female1_vs_zero) 
  nde_black <- extract_stats(analysis_results$nde$black_female1_vs_zero) 
  nde_light_black <- extract_stats(analysis_results$nde$light_black_female1_vs_zero)
  nde_dark_black <- extract_stats(analysis_results$nde$dark_black_female1_vs_zero)
  
  # Extract NIE results
  nie_white <- extract_stats(analysis_results$nie$white_female1_vs_zero) 
  nie_black <- extract_stats(analysis_results$nie$black_female1_vs_zero) 
  nie_light_black <- extract_stats(analysis_results$nie$light_black_female1_vs_zero)
  nie_dark_black <- extract_stats(analysis_results$nie$dark_black_female1_vs_zero)
  
  
  # Extract ATE results
  ate_white <- extract_stats(analysis_results$ate$white_female1_vs_zero) 
  ate_black <- extract_stats(analysis_results$ate$black_female1_vs_zero) 
  ate_light_black <- extract_stats(analysis_results$ate$light_black_female1_vs_zero)
  ate_dark_black <- extract_stats(analysis_results$ate$dark_black_female1_vs_zero)
  
  
  # Combine into final table
  results_table <- rbind(
    
    data.frame(Effect = "Total (ATE)", Group = "White", ate_white), 
    data.frame(Effect = "Total (ATE)", Group = "Black", ate_black), 
    data.frame(Effect = "Total (ATE)", Group = "Light_Black", ate_light_black),
    data.frame(Effect = "Total (ATE)", Group = "Dark_Black", ate_dark_black),
    
    
    data.frame(Effect = "Indirect (NIE)", Group = "White", nie_white), 
    data.frame(Effect = "Indirect (NIE)", Group = "Black", nie_black), 
    data.frame(Effect = "Indirect (NIE)", Group = "Light_Black", nie_light_black),
    data.frame(Effect = "Indirect (NIE)", Group = "Dark_Black", nie_dark_black),
    
    
    data.frame(Effect = "Direct (NDE)", Group = "White", nde_white), 
    data.frame(Effect = "Direct (NDE)", Group = "Black", nde_black), 
    data.frame(Effect = "Direct (NDE)", Group = "Light_Black", nde_light_black),
    data.frame(Effect = "Direct (NDE)", Group = "Dark_Black", nde_dark_black)

    
  )
  
  # Clean column names
  names(results_table) <- c("Effect", "Group", "Estimate", "SE", "P_Value")
  
  # Add significance stars
  results_table$Sig <- ifelse(results_table$P_Value < 0.001, "***",
                              ifelse(results_table$P_Value < 0.01,  "**",
                                     ifelse(results_table$P_Value < 0.05,  "*", "")))
  
  
  # Round numeric columns
  results_table$Estimate <- round(results_table$Estimate, 4)
  results_table$SE <- round(results_table$SE, 4)
  results_table$P_Value <- round(results_table$P_Value, 4)
  
  return(results_table)
}

 
format_female0_results <- function(analysis_results) {
  
  # Helper function to extract estimate, se, and p-value from pooled results
  extract_stats <- function(pooled_summary, row_name = "(Intercept)") {
    if (is.null(pooled_summary) || nrow(pooled_summary) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    row_idx <- which(pooled_summary$term == row_name)
    if (length(row_idx) == 0) {
      return(data.frame(estimate = NA, se = NA, p_value = NA))
    }
    
    data.frame(
      estimate = pooled_summary$estimate[row_idx],
      se = pooled_summary$std.error[row_idx],
      p_value = pooled_summary$p.value[row_idx]
    )
  }
  
  # Create results table
  results_table <- data.frame(
    Effect = character(),
    Group = character(),
    Estimate = numeric(),
    SE = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract NDE results
  nde_white <- extract_stats(analysis_results$nde$white_female0_vs_zero) 
  nde_black <- extract_stats(analysis_results$nde$black_female0_vs_zero) 
  nde_light_black <- extract_stats(analysis_results$nde$light_black_female0_vs_zero)
  nde_dark_black <- extract_stats(analysis_results$nde$dark_black_female0_vs_zero)
  
  # Extract NIE results
  nie_white <- extract_stats(analysis_results$nie$white_female0_vs_zero) 
  nie_black <- extract_stats(analysis_results$nie$black_female0_vs_zero) 
  nie_light_black <- extract_stats(analysis_results$nie$light_black_female0_vs_zero)
  nie_dark_black <- extract_stats(analysis_results$nie$dark_black_female0_vs_zero)
  
  
  # Extract ATE results
  ate_white <- extract_stats(analysis_results$ate$white_female0_vs_zero) 
  ate_black <- extract_stats(analysis_results$ate$black_female0_vs_zero) 
  ate_light_black <- extract_stats(analysis_results$ate$light_black_female0_vs_zero)
  ate_dark_black <- extract_stats(analysis_results$ate$dark_black_female0_vs_zero)
  
  
  # Combine into final table
  results_table <- rbind(
    
    
    data.frame(Effect = "Total (ATE)", Group = "White", ate_white), 
    data.frame(Effect = "Total (ATE)", Group = "Black", ate_black), 
    data.frame(Effect = "Total (ATE)", Group = "Light_Black", ate_light_black),
    data.frame(Effect = "Total (ATE)", Group = "Dark_Black", ate_dark_black),
    
    data.frame(Effect = "Indirect (NIE)", Group = "White", nie_white), 
    data.frame(Effect = "Indirect (NIE)", Group = "Black", nie_black), 
    data.frame(Effect = "Indirect (NIE)", Group = "Light_Black", nie_light_black),
    data.frame(Effect = "Indirect (NIE)", Group = "Dark_Black", nie_dark_black),
    
    data.frame(Effect = "Direct (NDE)", Group = "White", nde_white), 
    data.frame(Effect = "Direct (NDE)", Group = "Black", nde_black), 
    data.frame(Effect = "Direct (NDE)", Group = "Light_Black", nde_light_black),
    data.frame(Effect = "Direct (NDE)", Group = "Dark_Black", nde_dark_black)
    
    
    
  )
  
  # Clean column names
  names(results_table) <- c("Effect", "Group", "Estimate", "SE", "P_Value")
  
  # Add significance stars
  results_table$Sig <- ifelse(results_table$P_Value < 0.001, "***",
                              ifelse(results_table$P_Value < 0.01,  "**",
                                     ifelse(results_table$P_Value < 0.05,  "*", "")))
  
  
  # Round numeric columns
  results_table$Estimate <- round(results_table$Estimate, 4)
  results_table$SE <- round(results_table$SE, 4)
  results_table$P_Value <- round(results_table$P_Value, 4)
  
  return(results_table)
}



make_intersectional_N <- function(merged_list, female = 1) { 
  
  # base filter: HS completed, and non-missing on key vars
  df <- merged_list[[1]] %>% 
    filter(hs_completed == 1) %>% 
    drop_na(compcoll25, hiskil19, covid_jobloss) %>% 
    group_by(race_skin, female) %>% 
    count() %>% 
    drop_na()
  
  # ensure expected race_skin levels exist as columns later
  # (white, light_black, dark_black); fill 0 where missing
  df_wide <- df %>%
    mutate(female = ifelse(female == 0, "Male", "Female")) %>%
    pivot_wider(
      names_from = race_skin,
      values_from = n
    ) %>% ungroup()
  
  # final column order & names you asked for
  out <- df_wide %>%
    dplyr::transmute(
      Effect      = c("Men (\\textit{N})", "Women (\\textit{N})"),
      White       = .data[["white"]],
      Black       = .data[["black"]],
      Light_Black = .data[["light_black"]],
      Dark_Black  = .data[["dark_black"]],
    )
  
  # filter
  if(female == 1){
    out <- out %>% filter(Effect == "Women (\\textit{N})")
  } else 
    out <- out %>% filter(Effect == "Men (\\textit{N})")
  
  out <- out %>%
    mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
    mutate(across(everything(), ~ paste0("{", .x, "}")))
                
  return(out)
}
 


calculate_proportion <- function(data, group_var = "Group") {
  data %>%
    group_by(!!sym(group_var)) %>%
    mutate(
      total_ate = Estimate[Effect == "Total (ATE)"],
      prop_direct = if_else(Effect == "Direct (NDE)", Estimate / total_ate, NA_real_),
      prop_indirect = if_else(Effect == "Indirect (NIE)", Estimate / total_ate, NA_real_)
    ) %>%
    ungroup() %>%
    filter(!is.na(prop_direct) | !is.na(prop_indirect)) %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      `Prop. NIE` = sprintf("%.3f", mean(prop_indirect, na.rm = TRUE)),
      `Prop. NDE` = sprintf("%.3f", mean(prop_direct, na.rm = TRUE))
    ) %>%
    t() %>% # Transpose the data frame
    as.data.frame() %>% # Convert transposed data to data frame
    `colnames<-`(.[1, ]) %>% # Use first row as column names
    rownames_to_column(var = "Effect") %>% # Add row names as column
    .[-1, ] %>% 
    select(Effect, White, Black, Dark_Black, Light_Black)
}

 
make_intersectional_N <- function(merged_list, female = 1) { 
  
  # base filter: HS completed, and non-missing on key vars
  base <- merged_list[[1]] %>% 
    filter(hs_completed == 1) %>% 
    drop_na(compcoll25, hiskil19, covid_jobloss, race)  # <-- use race here
  
  ## 1) Total White / Black counts from `race`
  df_race <- base %>%
    group_by(female, race) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(race %in% c("white", "black")) %>%
    tidyr::pivot_wider(
      names_from  = race,
      values_from = n,
      values_fill = 0
    )
  
  ## 2) Light / Dark Black counts from `race_skin`
  df_skin <- base %>%
    drop_na(race_skin) %>%
    group_by(female, race_skin) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(race_skin %in% c("light_black", "dark_black")) %>%
    tidyr::pivot_wider(
      names_from  = race_skin,
      values_from = n,
      values_fill = 0
    )
  
  ## 3) Merge race + skin-tone tables by sex
  df_wide <- df_race %>%
    left_join(df_skin, by = "female") %>%
    # make sure missing cols exist and are 0
    mutate(
      white       = dplyr::coalesce(.data[["white"]],       0L),
      black       = dplyr::coalesce(.data[["black"]],       0L),
      light_black = dplyr::coalesce(.data[["light_black"]], 0L),
      dark_black  = dplyr::coalesce(.data[["dark_black"]],  0L)
    ) %>%
    ungroup()
  
  ## 4) Final column order
  out <- df_wide %>%
    mutate(
      Effect = dplyr::if_else(female == 0,
                              "Men (\\textit{N})",
                              "Women (\\textit{N})")
    ) %>%
    dplyr::transmute(
      Effect,
      White       = .data[["white"]],
      Black       = .data[["black"]],          # <-- from `race`
      Light_Black = .data[["light_black"]],    # <-- from `race_skin`
      Dark_Black  = .data[["dark_black"]]      # <-- from `race_skin`
    )
  
  # filter by requested sex
  if (female == 1) {
    out <- out %>% filter(Effect == "Women (\\textit{N})")
  } else {
    out <- out %>% filter(Effect == "Men (\\textit{N})")
  }
  
  # pretty-print numbers and wrap with {}
  out <- out %>%
    mutate(across(where(is.numeric),
                  ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
    mutate(across(everything(), ~ paste0("{", .x, "}")))
  
  return(out)
}

