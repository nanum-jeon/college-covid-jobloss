# scripts/25_impute_covariates.R
source("scripts/00_setup.R")
source(file.path(PARENT, "src", "imputation.R"))

# Build or load background covariates
 
covariates <- readRDS(file.path(DIR_INTERIM, "covariates.rds")) 

# Impute
imp <- impute_covariates(covariates, m = 5, maxit = 5, seed = 2024, method = "cart")
completed_data_list <- complete_list(imp)

# Save

saveRDS(imp, file.path(DIR_INTERIM, "imp_covars_mids.rds"))
saveRDS(completed_data_list, file.path(DIR_INTERIM, "imp_covars_list.rds"))

# Quick checks
message("[mice] iterations: ", imp$iteration, "; m = ", imp$m)
print(sapply(completed_data_list, function(d) sum(colSums(is.na(d)))))
