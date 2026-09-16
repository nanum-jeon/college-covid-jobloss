# scr/non_imputed.R

# Mediation with EIF (Complete Data)
fit_non_imputed_mediation_models <- function(data,  
                                             avar_name, 
                                             mvar_name, 
                                             yvar_name, 
                                             xvars = NULL, 
                                             eps = 0.001,
                                             K = 5,
                                             sl_library = c("SL.mean", "SL.glmnet", "SL.ranger")) {
  
  # Default covariates if not specified
  if (is.null(xvars)) {
    xvars <- exprs(
      race_white, skin_tone_scale, 
      female, maeduc, faeduc, parinc,
      rural_12, south_12, intact_12, sibsz,
      asvab_pst, hs_gpa, col_prep,
      delinq, subs_use, fight,
      cohabited_18, child_18,
      tchgd, schsafe,
      stolen, threatened,
      pct_peer_about75, pct_peer_more_than90
    )
  }
  
  # Complete data analysis
  df <- data %>% 
    filter(hs_completed == 1) %>% 
    na.omit()
  
  # Convert variable names to symbols
  avar <- sym(avar_name)
  mvar <- sym(mvar_name)
  yvar <- sym(yvar_name)
  
  # Build model formulas
  # A ~ X (treatment model)
  a_rhs <- xvars %>%
    reduce(~ expr(!!.x + !!.y))
  a_form <- as.formula(expr(!!avar ~ !!a_rhs))
  
  # Y ~ X + M + A (outcome model)
  y_rhs <- c(xvars, mvar, avar) %>%
    reduce(~ expr(!!.x + !!.y))
  y_form <- as.formula(expr(!!yvar ~ !!y_rhs))
  
  # M ~ X + A (mediator model)
  m_rhs <- c(xvars, avar) %>%
    reduce(~ expr(!!.x + !!.y))
  m_form <- as.formula(expr(!!mvar ~ !!m_rhs))
  
  cat("Treatment model formula:", deparse(a_form), "\n")
  cat("Outcome model formula:", deparse(y_form), "\n")
  cat("Mediator model formula:", deparse(m_form), "\n")
  
  
  # Create cross-fitting split
  cf_fold <- createFolds(df[[yvar_name]], K)
  
  main_list <- vector(mode = "list", K)
  
  for(k in 1:K) {
    
    cat("  Cross-fitting fold", k, "\n")
    
    #################################################
    # Design Matrices for different models
    #################################################
    
    aux <- df[-cf_fold[[k]], ]
    
    # Treatment model design matrix
    aux_X <- model.matrix(a_form, data = aux)[, -1] %>% as_tibble()
    df_X <- model.matrix(a_form, data = df)[, -1] %>% as_tibble()
    
    # Mediator model design matrix 
    aux_XA <- model.matrix(m_form, data = aux)[, -1] %>% as_tibble()
    df_XA <- model.matrix(m_form, data = df)[, -1] %>% as_tibble()
    
    # Outcome model design matrix
    aux_XMA <- model.matrix(y_form, data = aux)[, -1] %>% as_tibble()
    df_XMA <- model.matrix(y_form, data = df)[, -1] %>% as_tibble()
    
    #################################################
    # Treatment model | X
    #################################################
    
    a_mod <- SuperLearner(
      Y          = aux[[avar_name]],
      X          = aux_X,
      newX       = df_X,
      family     = binomial(),
      SL.library = sl_library,
      control    = list(saveFitLibrary = TRUE, trimLogit = 0.05),
      cvControl  = list(V = 5L, stratifyCV = TRUE, shuffle = TRUE, validRows = NULL)
    )
    
    df$a_fit <- clip01(a_mod$SL.predict, eps)
    
    #################################################
    # Mediator model | X, A
    #################################################
    
    m_mod <- SuperLearner(
      Y          = aux[[mvar_name]],
      X          = aux_XA,
      newX       = df_XA,
      family     = binomial(),
      SL.library = sl_library,
      control    = list(saveFitLibrary = TRUE, trimLogit = 0.05),
      cvControl  = list(V = 5L, stratifyCV = TRUE, shuffle = TRUE, validRows = NULL)
    )
    
    df$m_fit <- m_mod$SL.predict
    
    #################################################
    # Outcome model for Y(a, M(a)) | X, M, A
    #################################################
    
    y_mod <- SuperLearner(
      Y          = aux[[yvar_name]],
      X          = aux_XMA,
      newX       = df_XMA,
      family     = binomial(),
      SL.library = sl_library,
      control    = list(saveFitLibrary = TRUE, trimLogit = 0.05),
      cvControl  = list(V = 5L, shuffle = TRUE, stratifyCV = TRUE, validRows = NULL)
    )
    
    #################################################
    # Estimate eta(e, e*, X)
    #################################################
    
    # Predictions for mediator M under different treatment levels
    df_XA_0 <- df_XA %>% mutate(!!avar_name := 0)
    df_XA_1 <- df_XA %>% mutate(!!avar_name := 1)
    
    df$m0_fit <- clip01(predict(m_mod, newdata = df_XA_0)$pred, eps)
    df$m1_fit <- clip01(predict(m_mod, newdata = df_XA_1)$pred, eps)
    
    df$pm_fit <- df$m1_fit - df$m0_fit
    
    # Outcome predictions at M=0/1 for A=0/1
    df_XMA_11 <- df_XMA %>% mutate(!!avar_name := 1, !!mvar_name := 1)
    df_XMA_10 <- df_XMA %>% mutate(!!avar_name := 1, !!mvar_name := 0)
    df_XMA_01 <- df_XMA %>% mutate(!!avar_name := 0, !!mvar_name := 1)
    df_XMA_00 <- df_XMA %>% mutate(!!avar_name := 0, !!mvar_name := 0)
    
    mu_1_1 <- predict(y_mod, newdata = df_XMA_11)$pred
    mu_1_0 <- predict(y_mod, newdata = df_XMA_10)$pred
    mu_0_1 <- predict(y_mod, newdata = df_XMA_01)$pred
    mu_0_0 <- predict(y_mod, newdata = df_XMA_00)$pred
    
    # controlled effect of M on Y at A=1
    df$cem_fit <- mu_1_1 - mu_1_0
    
    # Averaged η
    df$eta_11_fit <- mu_1_1 * df$m1_fit + mu_1_0 * (1 - df$m1_fit)
    df$eta_00_fit <- mu_0_1 * df$m0_fit + mu_0_0 * (1 - df$m0_fit)
    df$eta_10_fit <- mu_1_1 * df$m0_fit + mu_1_0 * (1 - df$m0_fit)
    df$eta_01_fit <- mu_0_1 * df$m1_fit + mu_0_0 * (1 - df$m1_fit)
    
    #################################################
    # Estimate E[Y | X, M, E = 1]
    #################################################
    
    df_XMA_1 <- df_XMA %>% mutate(!!avar_name := 1)
    df_XMA_0 <- df_XMA %>% mutate(!!avar_name := 0)
    
    df$y1_fit <- predict(y_mod, newdata = df_XMA_1)$pred
    df$y0_fit <- predict(y_mod, newdata = df_XMA_0)$pred
    
    main_list[[k]] <- df[cf_fold[[k]], ]
  }
  
  ################### 
  # Construct rEIFs # 
  ####################
  
  main_df <- reduce(main_list, bind_rows) %>%
    mutate(
      ps = a_fit,
      pm = pm_fit, 
      cem = cem_fit, 
      avar_col = !!sym(avar_name),
      mvar_col = !!sym(mvar_name), 
      yvar_col = !!sym(yvar_name),
      m0_eif = (1 - avar_col)/(1 - a_fit) * (mvar_col - m0_fit) + m0_fit, 
      m1_eif = (avar_col)/(a_fit) * (mvar_col - m1_fit) + m1_fit,
      y0_eif = (1 - avar_col)/(1 - a_fit) * (yvar_col - y0_fit) + y0_fit,
      y1_eif = (avar_col)/(a_fit) * (yvar_col - y1_fit) + y1_fit,
      y00_eif = (1 - avar_col)/(1 - a_fit) * (yvar_col - eta_00_fit) + eta_00_fit,
      y10_eif = ((avar_col)*(m0_fit)/(a_fit*m1_fit)) * (yvar_col - y1_fit) + ((1-avar_col)/(1-a_fit))*(y1_fit - eta_10_fit) + eta_10_fit,
      y11_eif = (avar_col)/(a_fit) * (yvar_col - eta_11_fit) + eta_11_fit,
      nde_eif = y10_eif - y00_eif,
      nie_eif = y11_eif - y10_eif, 
      ate_eif = nde_eif + nie_eif,
      atm_eif = m_fit + avar_col/a_fit * (mvar_col - m_fit)
    ) %>%
    select(-avar_col, -mvar_col, -yvar_col)  # Clean up temporary columns
  
  return(main_df)
}


# ------- Analysis -------- # 
# Function to fit models on a single data frame 
fit_main_models_non_imputed <- function(df, outcome_var) {
  # Make sure the outcome exists and drop NAs only on needed vars
  df <- df %>%
    dplyr::select(all_of(c(outcome_var, "race", "race_skin"))) %>%
    dplyr::filter(!is.na(.data[[outcome_var]]))
  
  # --- Main effects models ---
  
  # 1) race model (white as ref, assuming factor levels already set)
  formula_white_ref  <- as.formula(paste(outcome_var, "~ race"))
  
  # 2) race_skin model with light_black as ref
  #    make sure race_skin is factor first
  df <- df %>%
    dplyr::mutate(
      race_skin = factor(race_skin),
      race_skin = stats::relevel(race_skin, ref = "light_black")
    )
  formula_light_ref  <- as.formula(paste(outcome_var, "~ race_skin"))
  
  models <- list(
    race_white_ref   = lm(formula_white_ref,  data = df),
    race_skin_light  = lm(formula_light_ref, data = df)
  )
  
  # --- Group means (intercept-only models) ---
  # Define each group's filter based on how vars are stored
  subgroup_defs <- list(
    white       = quote(race == "white"),
    black       = quote(race == "black"),
    light_black = quote(race_skin == "light_black"),
    dark_black  = quote(race_skin == "dark_black")
  )
  
  formula_intercept_only <- as.formula(paste(outcome_var, "~ 1"))
  
  subgroup_models <- list()
  for (grp in names(subgroup_defs)) {
    rows <- eval(subgroup_defs[[grp]], envir = df)
    subset_df <- df[rows, , drop = FALSE]
    
    # Only fit if we actually have data
    if (nrow(subset_df) > 0) {
      subgroup_models[[paste0(grp, "_vs_zero")]] <-
        lm(formula_intercept_only, data = subset_df)
    }
  }
  
  # Combine and return summaries
  all_models <- c(models, subgroup_models)
  lapply(all_models, summary)
}

# Function to run regression analysis on non-imputed data
run_regression_analysis_non_imputed <- function(df, models_function) {
  
  # Define the outcome variables you want to analyze
  outcomes <- c("ate_eif", "nde_eif", "nie_eif")
  
  # Apply the modeling function to each outcome on the single data frame 'df'
  results <- setNames(
    lapply(outcomes, function(outcome) models_function(df, outcome)),
    c("ate", "nde", "nie")
  )
  
  # Return the nested list of results
  return(results)
}


### format results

# Function to extract and format coefficients from a single model
format_single_model <- function(model_summary, model_name) {
  if (inherits(model_summary, "summary.lm")) {
    coef_table <- model_summary$coefficients
    
    # Create a clean data frame
    result <- data.frame(
      Model = model_name,
      Term = rownames(coef_table),
      Estimate = round(coef_table[, "Estimate"], 4),
      Std_Error = round(coef_table[, "Std. Error"], 4),
      t_value = round(coef_table[, "t value"], 3),
      p_value = round(coef_table[, "Pr(>|t|)"], 4),
      Significance = case_when(
        coef_table[, "Pr(>|t|)"] < 0.001 ~ "***",
        coef_table[, "Pr(>|t|)"] < 0.01 ~ "**",
        coef_table[, "Pr(>|t|)"] < 0.05 ~ "*",
        coef_table[, "Pr(>|t|)"] < 0.1 ~ ".",
        TRUE ~ ""
      ),
      R_squared = round(model_summary$r.squared, 4),
      Adj_R_squared = round(model_summary$adj.r.squared, 4),
      N_obs = model_summary$df[1] + model_summary$df[2]
    )
    
    rownames(result) <- NULL
    return(result)
  } else {
    return(NULL)
  }
}

# Function to format all models for one outcome
format_outcome_results <- function(outcome_results, outcome_name) {
  all_formatted <- list()
  
  for (model_name in names(outcome_results)) {
    formatted <- format_single_model(outcome_results[[model_name]], model_name)
    if (!is.null(formatted)) {
      formatted$Outcome <- outcome_name
      all_formatted[[model_name]] <- formatted
    }
  }
  
  # Combine all models for this outcome
  do.call(rbind, all_formatted)
}

# Main function to format all regression results
format_all_results <- function(results) {
  all_outcomes <- list()
  
  for (outcome_name in names(results)) {
    outcome_formatted <- format_outcome_results(results[[outcome_name]], outcome_name)
    all_outcomes[[outcome_name]] <- outcome_formatted
  }
  
  # Combine all outcomes
  final_results <- do.call(rbind, all_outcomes)
  
  # Reorder columns for better readability
  final_results <- final_results[, c("Outcome", "Model", "Term", "Estimate", 
                                     "Std_Error", "t_value", "p_value", 
                                     "Significance", "R_squared", "Adj_R_squared", "N_obs")]
  
  return(final_results)
}

 
# Function to create group estimate summary
create_group_means_summary <- function(results) {
  
  # Filter for group means (intercept-only models)
  group_means <- results %>%
    filter(grepl("_vs_zero", Model), Term == "(Intercept)") %>%
    select(Outcome, Model, Estimate, Std_Error, p_value, Significance, N_obs) %>%
    arrange(Outcome, Model)
  
  # Clean up model names
  group_means$Group <- gsub("_vs_zero", "", group_means$Model)
  group_means <- group_means %>%
    select(Outcome, Group, Estimate, SE = Std_Error, P_Value = p_value, Sig = Significance, N_obs)
  
  return(group_means)
}
 