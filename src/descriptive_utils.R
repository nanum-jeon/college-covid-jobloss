
pool_svyglm <- function(model_list) {
  if (length(model_list) == 0) {
    stop("model_list is empty. Please provide a list of svyglm models.")
  }
  
  m <- length(model_list)  # Total number of models/imputations
  
  b <- se <- SSR <- SST <- list()
  
  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    
    b[[i]] <- coef(model)
    se[[i]] <- summary(model)$coef[, 2]
    
    residuals <- residuals(model)
    y_values <- model$y
    
    SSR[[i]] <- sum(residuals^2)
    SST[[i]] <- sum((y_values - mean(y_values))^2)
  }
  
  # Convert lists to matrices for easier computation
  b_matrix <- do.call(cbind, b)
  se_matrix <- do.call(cbind, se)
  SSR_vector <- unlist(SSR)
  SST_vector <- unlist(SST)
  
  # Pooling coefficients
  b.pool <- rowMeans(b_matrix)
  between.var <- apply(b_matrix, 1, function(x) var(x, na.rm = TRUE))
  within.var <- rowMeans(se_matrix^2, na.rm = TRUE)
  total.var <- within.var + between.var + (between.var / m)
  se.pool <- sqrt(total.var)
  t.pool <- b.pool / se.pool
  pvalue.pool <- 2 * (1 - pnorm(abs(t.pool)))
  
  # Pooling R-squared
  R2 <- 1 - (SSR_vector / SST_vector)
  r.squared <- mean(R2)
  
  # Assuming the last model's n and p are representative
  n <- nobs(model_list[[m]])
  p <- length(model_list[[m]]$coefficients) - 1
  bic.null <- n * log(1 - r.squared) + p * log(n)
  
  coefficients <- data.frame(b.pool, se.pool, t.pool, pvalue.pool)
  coefficients <- round(coefficients, 3)
  
  # Return everything in a list
  return(list(coef = coefficients, n = n, r.squared = r.squared, bic.null = bic.null))
}

convertModel <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef$b.pool, 
    se = model$coef$se.pool, 
    pvalues = model$coef$pvalue.pool,
    gof.names = c("R2","BIC (null)","N"), 
    gof = c(model$r.squared, model$bic.null, model$n), 
    gof.decimal = c(T,F,F)
  )
}


# Utility functions for survey analysis

#' Fit survey GLM models for a list of variables across multiple survey designs
#' 
#' @param var_names Character vector of variable names to model
#' @param svy_design_list List of survey design objects
#' @param subset_condition Optional condition to subset the survey designs (as a string)
#' @return List of model lists, one for each variable
fit_survey_models <- function(var_names, svy_design_list, subset_condition = NULL) {
  models <- list()
  
  for (var in var_names) {
    model_list <- list()
    
    for (i in seq_along(svy_design_list)) {
      formula <- as.formula(paste(var, "~ 1"))
      
      if (!is.null(subset_condition)) {
        # Apply subset condition
        design_subset <- eval(parse(text = paste0("subset(svy_design_list[[i]], ", subset_condition, ")")))
        model_list[[i]] <- svyglm(formula, design_subset)
      } else {
        model_list[[i]] <- svyglm(formula, svy_design_list[[i]])
      }
    }
    
    models[[var]] <- model_list
  }
  
  return(models)
}

#' Pool survey GLM results for multiple variables
#' 
#' @param model_list List of model lists from fit_survey_models()
#' @return List of pooled results for each variable
pool_survey_results <- function(model_list) {
  pooled_results <- list()
  
  for (var in names(model_list)) {
    pooled_results[[var]] <- pool_svyglm(model_list[[var]])
  }
  
  return(pooled_results)
}

#' Format pooled results into a data frame
#' 
#' @param pooled_results List of pooled results from pool_survey_results()
#' @param var_order Character vector specifying the order of variables
#' @return Data frame with formatted coefficients and standard errors
format_pooled_results <- function(pooled_results, var_order = NULL) {
  if (is.null(var_order)) {
    var_order <- names(pooled_results)
  }
  
  results_df <- data.frame(vars = character(), vals = character(), stringsAsFactors = FALSE)
  
  for (var in var_order) {
    if (var %in% names(pooled_results)) {
      pooled_result <- pooled_results[[var]]
      coefs <- pooled_result$coef$b.pool
      ses <- pooled_result$coef$se.pool
      
      # Format coefficients and standard errors to three decimal places
      formatted_coefs <- formatC(coefs, format = "f", digits = 3)
      formatted_ses <- formatC(ses, format = "f", digits = 3)
      
      temp_df <- data.frame(
        vars = rep(var, 2 * length(coefs)),
        vals = c(as.character(formatted_coefs), paste0("(", as.character(formatted_ses), ")")),
        stringsAsFactors = FALSE
      )
      
      results_df <- rbind(results_df, temp_df)
    }
  }
  
  return(results_df)
}

#' Complete workflow for fitting and formatting survey results for a subgroup
#' 
#' @param var_names Character vector of variable names
#' @param svy_design_list List of survey design objects
#' @param subset_condition Optional subset condition (as string)
#' @param var_order Optional variable ordering
#' @return Formatted data frame with results
analyze_subgroup <- function(var_names, svy_design_list, subset_condition = NULL, var_order = NULL) {
  # Fit models
  models <- fit_survey_models(var_names, svy_design_list, subset_condition)
  
  # Pool results
  pooled <- pool_survey_results(models)
  
  # Format results
  formatted <- format_pooled_results(pooled, var_order)
  
  return(formatted)
}

#' Create sample counts for different subgroups
#' 
#' @param data Data frame with survey data
#' @param race_var Name of race variable (default: "race")
#' @param race_skin_var Name of race-skin combination variable (default: "race_skin")
#' @return Data frame with sample counts
create_sample_counts <- function(data, race_var = "race", race_skin_var = "race_skin") {
  counts <- data.frame(
    variable = "N",
    all = nrow(data),
    white = sum(data[[race_var]] == "white", na.rm = TRUE),
    black = sum(data[[race_var]] == "black", na.rm = TRUE),
    light_black = sum(data[[race_skin_var]] == "light_black", na.rm = TRUE),
    dark_black = sum(data[[race_skin_var]] == "dark_black", na.rm = TRUE)
  )
  
  return(counts)
}

#' Combine results from multiple subgroups into a single table
#' 
#' @param var_labels Character vector of variable labels
#' @param ... Named data frames with results (e.g., all_results, white_results, etc.)
#' @return Combined data frame with all results
combine_subgroup_results <- function(var_labels, ...) {
  result_dfs <- list(...)
  
  # Extract values columns from each data frame
  vals_columns <- lapply(result_dfs, function(df) df$vals)
  
  # Combine with variable labels
  combined_df <- cbind(
    as.vector(rbind(var_labels, "")),
    do.call(cbind, vals_columns)
  )
  
  # Set column names
  col_names <- c("variable", names(result_dfs))
  colnames(combined_df) <- col_names
  
  return(combined_df)
}

 
### Table 3

pool_mean_by <- function(var_formula, by_formula, subset_expr = NULL, design_list = subset_design_list) {
  by_list <- lapply(design_list, function(d) {
    d2 <- if (is.null(subset_expr)) d else subset(d, eval(subset_expr))
    svyby(var_formula, by_formula, design = d2,
          FUN = svymean, na.rm = TRUE,
          keep.names = FALSE, drop.empty.groups = TRUE)
  })
  
  pooled <- MIcombine(by_list)
  
  # grab the grouping columns (clean names, no numbering)
  group_vars <- all.vars(by_formula)
  group_df   <- as_tibble(by_list[[1]][, group_vars, drop = FALSE])
  
  # bind pooled estimates
  bind_cols(
    group_df,
    tibble(
      mean = as.numeric(coef(pooled)),
      se   = sqrt(diag(vcov(pooled)))
    )
  )
}


normalize_result <- function(df,
                             analysis,
                             group_col   = NULL,
                             group_value = NULL,
                             fix_highskill = NULL,
                             fix_college   = NULL) {
  # Optionally fix columns
  if (!is.null(fix_highskill)) {
    df$hiskil19 <- rep_len(fix_highskill, nrow(df))
  }
  if (!is.null(fix_college)) {
    df$compcoll25 <- rep_len(fix_college, nrow(df))
  }
  
  out <- df %>%
    dplyr::mutate(analysis = analysis) %>%
    dplyr::relocate(analysis, .before = dplyr::everything())
  
  # add/rename "group"
  if (!is.null(group_value)) {
    out <- out %>% dplyr::mutate(group = group_value)
  } else if (!is.null(group_col)) {
    out <- out %>% dplyr::rename(group = !!rlang::sym(group_col))
  } else {
    out <- out %>% dplyr::mutate(group = "all")
  }
  
  out %>% dplyr::select(analysis, group, hiskil19, compcoll25, mean, se)
}

