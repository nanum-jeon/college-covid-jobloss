# src/imputation.R
impute_covariates <- function(covariates, m = 5, maxit = 5, seed = 2024, method = "cart") {
  stopifnot("PUBID" %in% names(covariates))
  
  # mice setup
  library(mice)
  dat <- covariates
  
  # IDs not imputed
  meth <- rep("", ncol(dat)); names(meth) <- names(dat)
  meth["PUBID"] <- ""  # leave as-is
  
  # Use given method for non-ID variables that have NA
  has_na <- sapply(dat, function(x) any(is.na(x)))
  meth[has_na & names(meth) != "PUBID"] <- method
  
  # Predictor matrix: exclude ID
  pred <- mice::quickpred(dat, exclude = "PUBID")
  pred[, "PUBID"] <- 0
  
  set.seed(seed)
  imp <- mice(dat, m = m, maxit = maxit, method = meth, predictorMatrix = pred, printFlag = FALSE)
  imp
}

complete_list <- function(imp) {
  stopifnot(inherits(imp, "mids"))
  lapply(seq_len(imp$m), function(i) complete(imp, i))
}
