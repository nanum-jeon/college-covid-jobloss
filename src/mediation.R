# scr/modeling.R

# Trim propensity score 
clip01 <- function(p, eps = 0.001) pmin(pmax(p, eps), 1 - eps)

# Mediation with EIF (Imputed Data)
fit_mediation_models <- function(data_list, 
                                 avar_name, 
                                 mvar_name, 
                                 yvar_name, 
                                 xvars = NULL,
                                 subset_conditions = NULL,
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
  
  # Default subset conditions if not specified
  if (is.null(subset_conditions)) {
    # Build subset conditions dynamically using actual variable names
    subset_conditions <- substitute(
      race %in% c("white","black") &
        hs_completed == 1 & 
        !is.na(skin_tone_scale) &
        !is.na(AVAR) &
        !is.na(MVAR) &
        !is.na(YVAR),
      list(AVAR = as.name(avar_name), 
           MVAR = as.name(mvar_name), 
           YVAR = as.name(yvar_name))
    )
  }
  
  
  # Convert variable names to symbols
  avar <- sym(avar_name)
  mvar <- sym(mvar_name)
  yvar <- sym(yvar_name)
  
  # Subset data based on conditions
  subset_data_list <- lapply(data_list, function(des) {
    subset(des, eval(subset_conditions))
  })
  
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
  
  # Number of MI datasets
  I <- length(subset_data_list)
  
  # Results storage
  results_list <- vector("list", I)
  
  for(i in 1:I) { 
    
    cat("Processing imputed sample", i, "\n")
    
    # Create cross-fitting split
    df <- subset_data_list[[i]]
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
        atm_eif = m_fit + avar_col/a_fit * (mvar_col - m_fit),
        med_eif = m1_eif - m0_eif
      ) %>%
      select(-avar_col, -mvar_col, -yvar_col)  # Clean up temporary columns
    
    # Store results from this imputation
    results_list[[i]] <- main_df
  }
  
  return(results_list)
}


