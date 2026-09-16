# scripts/imputation_diagnostic.R
source("src/labels.R")
source("src/utils.R")
source("scripts/00_setup.R")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(mice); library(readr); library(rlang)
})

# helpers for pooled descriptives
source(file.path(PARENT, "src", "mi_diagnostics.R"))

# load mids
imp <- readRDS(file.path(DIR_INTERIM, "imp_covars_mids.rds"))
dat_obs <- imp$data
dir.create(DIR_RESULTS, showWarnings = FALSE, recursive = TRUE)
 

# MI-pooled descriptives (all imputed vars)
out <- pooled_descriptives_all(imp, include_non_imputed = TRUE, exclude = "PUBID")
 
# reorder
out$numeric     <- reorder_by_var(out$numeric, var_order) 

 
write_csv(out$numeric, file.path(DIR_RESULTS, "mi_pooled_descriptives.csv"))


message("Imputation diagnostics complete. Files written to: ", DIR_RESULTS)
