# scripts/31_create_treatment.R
# -----------------------------------------
# Creates BA-by-25 (month-based) and HS-by-20 flags.
# Requires: make_treatment(), make_hs_by20(), load_nlsy97_core(), build_dob_months()
# -----------------------------------------

source(here::here("scripts", "00_setup.R"))  # sets DIR_* and loads packages
# 00_setup.R should source your src/ files that define the helpers above.

message("[31] Loading NLSY97 data...")

raw_path <- here::here(DIR_DATA, "college_covid_jobloss", "college_covid_jobloss.R")
ged_path <- here::here(DIR_DATA, "nlsy97_GED", "nlsy97_GED.R")

raw <- load_nlsy97_core(raw_path)
ged <- load_nlsy97_core(ged_path)

stopifnot("PUBID" %in% names(raw), "PUBID" %in% names(ged))

# DOB months (same scale as CVC_* month variables)
dob_m <- build_dob_months(raw)
stopifnot(all(c("PUBID", "dob_m") %in% names(dob_m)))

# Merge core pieces
df <- raw %>%
  dplyr::left_join(ged,   by = "PUBID") %>%
  dplyr::left_join(dob_m, by = "PUBID")

df

# ------------------------
# Education variables
# ------------------------

message("[31] Computing BA-by-25 (month-based)...")
ba_tbl <- make_treatment(df)  # returns PUBID, age_at_ba, ba_by25

# quick QA
print(with(ba_tbl, table(ba_by25, useNA = "always")))
suppressWarnings(print(summary(ba_tbl$age_at_ba)))
message(sprintf("[31] BA-by-25 rate: %.2f%%",
                100 * mean(ba_tbl$ba_by25 == 1, na.rm = TRUE)))


# BA degree by age 25 
ba_by25 <- ba_tbl %>% dplyr::select(PUBID, ba_by25)

message("[31] Computing HS-by-20...")
hs_tbl <- make_hs_by20(df) # returns PUBID, hs_by20, age_at_hs, age_at_ged

# quick QA
print(with(hs_tbl, table(hs_by20, useNA = "always")))

# High shool completion by age 20 
hs_by20 <- hs_tbl %>% dplyr::select(PUBID, hs_by20)

# High school completion by age 20 (original coding)
hs_completed <- make_hs_completed(df)

# ---------------------
# Load compcoll25 
# ---------------------
stata_path <- here::here(DIR_DATA, "edurose97_socioeconomicoutcomes_20211226.dta")

compcoll25 <- read_dta(stata_path) %>% 
  mutate(PUBID = R0000100) %>%
  dplyr::select(PUBID, compcoll25) 

 
# ------------------------
# Combine & (optionally) save
# ------------------------
treatment_vars <- ba_by25 %>%
  dplyr::left_join(hs_by20, by = "PUBID") %>%
  dplyr::left_join(compcoll25, by = "PUBID") %>% 
  left_join(hs_completed, by = "PUBID")

# Confirmed they are the same 
treatment_vars %>% 
  count(hs_by20, hs_completed)

# Optional: write to data/intermediate for downstream scripts
out_dir <- here::here(DIR_INTERIM)   # DIR_INTERIM set in 00_setup.R
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

rds_file <- file.path(out_dir, "treatment_vars.rds")
csv_file <- file.path(out_dir, "treatment_vars.csv")

# Save both formats
saveRDS(treatment_vars, rds_file)
readr::write_csv(treatment_vars, csv_file)

message("[31] Saved: ", rds_file, " and ", csv_file)

# Return to workspace if sourcing interactively
invisible(treatment_vars)
