# scripts/clean_covariates.R
source("scripts/00_setup.R")

file.path(DIR_DATA, "CPIAUCSL.csv")

# 1) Load CPI
cpi_obj <- load_cpi(file.path(DIR_DATA, "CPIAUCSL.csv"), base_year = 2022)
cpi     <- cpi_obj$cpi
cpi_2022 <- cpi$cpi[cpi$year == 2022]


# 2) Load NLSY97 core (this wraps your `source(...)` + renaming)
raw <- load_nlsy97_core(file.path(DIR_DATA, "college_covid_jobloss", "college_covid_jobloss.R"))

# 3) Time variables used later (and for family factors)
dob_m     <- build_dob_months(raw)
int_month <- build_int_month(raw, dob_m)
age_last  <- build_age_last(raw, int_month)

# 4) BACKGROUND COVARIATES (today’s focus)
background_covars <- build_background_covars(raw, cpi)

# 5) FAMILY FACTORS (keep if you want them now, else comment these 3 lines)
family_merged <- build_family_factors(raw, age_last, dob_m)
covariates    <- dplyr::left_join(background_covars, family_merged, by = "PUBID")
 
 
# 6) Quick QA 
if (exists("covariates")) print(colSums(is.na(covariates)))

# 7) Save  
# Optional: write to data/intermediate for downstream scripts
out_dir <- here::here(DIR_INTERIM)   # DIR_INTERIM set in 00_setup.R
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rds_file <- file.path(out_dir, "covariates.rds")
csv_file <- file.path(out_dir, "covariates.csv")

# Save both formats
saveRDS(covariates, rds_file)
readr::write_csv(covariates, csv_file)

message("[20] Saved: ", rds_file, " and ", rds_file)
message("[20] Saved: ", csv_file, " and ", csv_file)

# Return to workspace if sourcing interactively
invisible(covariates)
