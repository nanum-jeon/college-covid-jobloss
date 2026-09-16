## scripts/24_build_analytic_sample.R
## ------------------------------------------------------------------
## Script: Merge imputed covariate sets with generated variables,
##         attach survey info (PSU/strata/weights),
##         build one svydesign per imputation, save outputs.
## ------------------------------------------------------------------

library(dplyr)
library(here)
source("src/survey_utils.R")  # load our utility functions

# ---- 0) Parameters -------------------------------------------------
KEYS <- c("PUBID")          # keys for joining (add 'year' if panel)
WT   <- "any_weight"        # name of weight variable
STR  <- "VSTRAT"            # strata variable name
PSU  <- "VPSU"              # PSU variable name


# ---- 1) Survey info: PSU/strata/weight -----------------------------
survey_info <- build_nlsy97_survey_info(
  core_path   = here::here(DIR_DATA, "college_covid_jobloss", "college_covid_jobloss.R"),
  weight_path = file.path(DIR_DATA, "customweight_nlsy97_any.dat"),
  weight_name = WT
) |>
  # standardize types
  dplyr::mutate(PUBID = as.integer(PUBID),
                !!WT := as.numeric(.data[[WT]])) |>
  # keep only needed columns
  dplyr::select(PUBID, all_of(c(PSU, STR, WT)))

# ---- 2) Generated blocks (outcome/treatment/mediator/moderator) ----
# read each block from interim folder and coerce PUBID to integer
outcome   <- readRDS(file.path(DIR_INTERIM, "outcome_vars.rds"))   |> coerce_pubid_int()
treatment <- readRDS(file.path(DIR_INTERIM, "treatment_vars.rds")) |> coerce_pubid_int()
mediator  <- readRDS(file.path(DIR_INTERIM, "mediator_vars.rds"))  |> coerce_pubid_int()
moderator <- readRDS(file.path(DIR_INTERIM, "moderator_vars.rds")) |> coerce_pubid_int()
covariates <-  readRDS(file.path(DIR_INTERIM, "covariates.rds"))|> coerce_pubid_int()


# quick check: ensure keys exist in each block
invisible(lapply(list(outcome, treatment, mediator, moderator),
                 check_keys_present, keys = KEYS))

# ---- 3) Imputed covariate list -------------------------------------
imp_covars_list <- readRDS(file.path(DIR_INTERIM, "imp_covars_list.rds"))
stopifnot(is.list(imp_covars_list))
imp_covars_list <- lapply(imp_covars_list, coerce_pubid_int)
invisible(lapply(imp_covars_list, check_keys_present, keys = KEYS))


# ---- 4) Merge each imputation with generated blocks + survey info --
merge_one <- function(cov_i) {
  x <- merge_blocks(cov_i, outcome, treatment, mediator, moderator, keys = KEYS)
  dplyr::left_join(x, survey_info, by = "PUBID")
}
merged_list <- lapply(imp_covars_list, merge_one)


# ---- 5) Build survey designs (one per imputation) ------------------
svy_list <- make_svy_list(merged_list, weight = WT, strata = STR, psu = PSU, nest = TRUE)


# ---- 6) Build non-imputed sample 
non_imputed <- merge_one(covariates)
 

# ---- 7) Save artifacts for downstream analysis ---------------------
dir.create(DIR_INTERIM, showWarnings = FALSE, recursive = TRUE)
saveRDS(merged_list, file.path(DIR_INTERIM, "merged_list.rds"))           # all merged datasets
saveRDS(svy_list,     file.path(DIR_INTERIM, "svy_list.rds"))             # list of svydesign objects
saveRDS(merged_list[[1]], file.path(DIR_INTERIM, "analysis_base_imp1.rds")) # first imputation for quick EDA
saveRDS(survey_info, file.path(DIR_INTERIM, "survey_info.rds"))
saveRDS(non_imputed, file.path(DIR_INTERIM, "non_imputed.rds")) # non-imputed data

# ---- 8) Quick log to console for sanity ----------------------------
message("Imputations processed: ", length(merged_list))
message("Rows (imp1): ", nrow(merged_list[[1]]))
message("NA weight share (imp1): ", mean(is.na(merged_list[[1]][[WT]])))


 

