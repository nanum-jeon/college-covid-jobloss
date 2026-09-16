# src/05_survey_utils.R
# -------------------------------------------------------------------
# Utility functions to (1) load NLSY97 survey info and attach weights,
# (2) coerce ID variables consistently,
# (3) merge multiple data blocks into one dataset,
# (4) build survey design objects from merged data frames.
# Depends: dplyr, survey
# -------------------------------------------------------------------

# 1. Build a cleaned survey info table from core + weight file.
#    - core_path: path to NLSY97 .R file
#    - weight_path: path to .dat weight file
#    - weight_name: name to give the weight variable (default: any_weight)
#    - keep: columns to pull from the core dataset
build_nlsy97_survey_info <- function(core_path,
                                     weight_path,
                                     weight_name = "any_weight",
                                     keep = c("PUBID","VPSU_1997","VSTRAT_1997")) {
  library(dplyr)
  
  # load core file (your custom loader)
  core <- load_nlsy97_core(core_path) |>
    dplyr::select(dplyr::all_of(keep)) |>
    # standardize types/names
    dplyr::transmute(
      PUBID  = as.integer(.data$PUBID),
      VPSU   = as.integer(.data$VPSU_1997),
      VSTRAT = as.integer(.data$VSTRAT_1997)
    )
  
  # read weight file using base read.table
  wts <- read.table(weight_path, col.names = c("PUBID", weight_name))
  
  # join PSU/strata with weight file; drop duplicates
  dplyr::left_join(core, wts, by = "PUBID") |>
    dplyr::distinct(PUBID, .keep_all = TRUE)
}

# 2. Ensure PUBID is integer (avoids mismatches on join).
coerce_pubid_int <- function(df) {
  if (!"PUBID" %in% names(df)) stop("PUBID not found in df")
  df$PUBID <- as.integer(df$PUBID)
  df
}

# 3. Merge one or more “blocks” (data frames) into a base covariate set.
#    - cov_df: the starting data frame (usually one imputed covariate set)
#    - ...: additional blocks (outcome, treatment, mediator, moderator)
#    - keys: merge keys (default: PUBID, add 'year' if panel)
merge_blocks <- function(cov_df, ..., keys = c("PUBID")) {
  library(dplyr)
  out <- cov_df
  for (b in list(...)) out <- dplyr::left_join(out, b, by = keys)
  out
}

# 4. Create a survey design object (no replicate weights version).
#    - df: merged data frame
#    - weight, strata, psu: names of weight/strata/PSU variables
#    - nest: whether to nest strata (TRUE recommended)
make_svy_design <- function(df,
                            weight = "any_weight",
                            strata  = "VSTRAT",
                            psu     = "VPSU",
                            nest = TRUE) {
  library(survey)
  survey::svydesign(
    ids     = as.formula(paste0("~", psu)),
    strata  = as.formula(paste0("~", strata)),
    weights = as.formula(paste0("~", weight)),
    nest    = nest,
    data    = df
  )
}

# 5. Build a list of survey designs (one per imputation).
make_svy_list <- function(merged_list,
                          weight = "any_weight",
                          strata  = "VSTRAT",
                          psu     = "VPSU",
                          nest = TRUE) {
  lapply(merged_list, make_svy_design,
         weight = weight, strata = strata, psu = psu, nest = nest)
}

# 6. Light checks: ensure keys present before merging.
check_keys_present <- function(df, keys) {
  miss <- setdiff(keys, names(df))
  if (length(miss)) stop("Missing key(s): ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

# 7. Light check: ensure weight variable exists and is finite.
check_weights_ok <- function(df, weight = "any_weight") {
  if (!weight %in% names(df)) stop("Weight variable '", weight, "' not found.")
  bad <- !is.finite(df[[weight]]) & !is.na(df[[weight]])
  if (any(bad)) warning("Non-finite weights detected.")
  invisible(TRUE)
}
