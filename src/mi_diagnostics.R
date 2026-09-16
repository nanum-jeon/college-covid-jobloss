# src/mi_diagnostics.R
# Helpers to compute MI-pooled descriptives (means and proportions)
 

pool_mean_num <- function(imp, var) {
  m <- imp$m
  ests <- lapply(seq_len(m), function(j) {
    x  <- mice::complete(imp, j)[[var]]
    n  <- sum(!is.na(x))
    mu <- mean(x, na.rm = TRUE)
    s2 <- stats::var(x, na.rm = TRUE)
    data.frame(qhat = mu,
               u    = ifelse(n > 0, s2 / n, NA_real_),  # within-imputation var of the mean
               n = n,
               s2 = s2)
  })
  ests <- do.call(rbind, ests)
  ests <- ests[is.finite(ests$qhat) & is.finite(ests$u), , drop = FALSE]
  if (nrow(ests) == 0) {
    return(data.frame(mean_mi = NA_real_, se_mi = NA_real_,
                      lwr95 = NA_real_, upr95 = NA_real_,
                      sd_within_mi = NA_real_))
  }
  # Older mice expects positional args: Q, U, n
  ps <- mice::pool.scalar(ests$qhat, ests$u, ests$n)
  se <- sqrt(ps$t)
  data.frame(
    mean_mi = ps$qbar,
    se_mi   = se,
    lwr95   = ps$qbar - 1.96 * se,
    upr95   = ps$qbar + 1.96 * se,
    # descriptive "MI SD": average within-imputation SD
    sd_within_mi = sqrt(mean(ests$s2, na.rm = TRUE))
  )
}

pool_prop_cat <- function(imp, var, level, levels_all) {
  m <- imp$m
  ests <- lapply(seq_len(m), function(j) {
    x <- mice::complete(imp, j)[[var]]
    x <- factor(x, levels = levels_all)
    n <- sum(!is.na(x))
    p <- if (n > 0) mean(x == level, na.rm = TRUE) else NA_real_
    u <- if (n > 0) p * (1 - p) / n else NA_real_   # var(mean) for Bernoulli
    data.frame(qhat = p, u = u, n = n)
  })
  ests <- do.call(rbind, ests)
  ests <- ests[is.finite(ests$qhat) & is.finite(ests$u), , drop = FALSE]
  if (nrow(ests) == 0) {
    return(data.frame(prop_mi = NA_real_, se_mi = NA_real_,
                      lwr95 = NA_real_, upr95 = NA_real_))
  }
  # Positional args again
  ps <- mice::pool.scalar(ests$qhat, ests$u, ests$n)
  se <- sqrt(ps$t)
  data.frame(
    prop_mi = ps$qbar,
    se_mi   = se,
    lwr95   = ps$qbar - 1.96 * se,
    upr95   = ps$qbar + 1.96 * se
  )
}


pooled_descriptives_all <- function(imp, vars = NULL,
                                    include_non_imputed = FALSE,
                                    exclude = character()) {
  stopifnot(inherits(imp, "mids"))
  dat_obs <- imp$data
  
  # Pick variables
  if (is.null(vars)) {
    vars <- if (include_non_imputed) colnames(dat_obs) else names(imp$method)[nzchar(imp$method)]
  }
  vars <- setdiff(vars, exclude)
  
  num_rows <- list(); cat_rows <- list()
  
  for (v in vars) {
    x_obs <- dat_obs[[v]]
    n_missing   <- sum(is.na(x_obs))
    cc_n        <- sum(!is.na(x_obs))
    n_total     <- cc_n + n_missing
    prop_imputed <- if (n_total > 0) n_missing / n_total else NA_real_
    was_imputed <- n_missing > 0
    is_num <- is.numeric(x_obs) && !is.factor(x_obs)
    
    if (is_num) {
      cc_mean <- mean(x_obs, na.rm = TRUE)
      cc_sd   <- sd(x_obs,   na.rm = TRUE)
      
      if (was_imputed) {
        mi <- pool_mean_num(imp, v)
        mi_mean <- mi$mean_mi
        mi_se   <- mi$se_mi
      } else {
        mi_mean <- cc_mean   # MI == CC when no missing
        mi_se   <- NA_real_
      }
      
      num_rows[[v]] <- data.frame(
        variable = v,
        prop_imputed = prop_imputed,
        n_missing = n_missing,
        n_complete = cc_n, cc_mean = cc_mean, cc_sd = cc_sd,
        mi_mean = mi_mean, mi_se = mi_se,
        diff_cc_mi = mi_mean - cc_mean
      )
    } else {
      # collect all levels across observed + imputations
      levs <- unique(na.omit(as.character(x_obs)))
      for (j in seq_len(imp$m)) {
        levs <- union(levs, unique(na.omit(as.character(mice::complete(imp, j)[[v]]))))
      }
      levels_all <- sort(levs)
      
      x_cc   <- factor(x_obs, levels = levels_all)
      cc_tab <- prop.table(table(x_cc))
      
      lev_tbl <- do.call(rbind, lapply(levels_all, function(L) {
        cc_prop <- as.numeric(cc_tab[L]); if (is.na(cc_prop)) cc_prop <- 0
        if (was_imputed) {
          pooled  <- pool_prop_cat(imp, v, level = L, levels_all = levels_all)
          mi_prop <- pooled$prop_mi
          mi_se   <- pooled$se_mi
        } else {
          mi_prop <- cc_prop   # MI == CC when no missing
          mi_se   <- NA_real_
        }
        data.frame(
          level = L,
          prop_imputed = prop_imputed,
          n_missing = n_missing,
          n_complete = sum(!is.na(x_cc)),
          cc_prop = cc_prop,
          mi_prop = mi_prop, mi_se = mi_se,
          diff_cc_mi = mi_prop - cc_prop
        )
      }))
      
      cat_rows[[v]] <- cbind(variable = v, lev_tbl, row.names = NULL)
    }
  }
  
  list(
    numeric = if (length(num_rows)) do.call(rbind, num_rows) else NULL,
    categorical = if (length(cat_rows)) do.call(rbind, cat_rows) else NULL
  )
}

 
