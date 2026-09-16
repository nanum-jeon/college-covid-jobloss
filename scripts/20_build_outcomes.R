# scripts/30_build_outcomes.R
source(here::here("scripts", "00_setup.R"))   # sets DIR_* and loads packages
# 00_setup.R should already source src/*.R (including outcomes & loaders)

# ---- paths to raw scripts (relative to repo) ----
pth_disruption <- file.path(DIR_DATA, "covid_job_disruption", "covid_job_disruption.R")
pth_disp       <- file.path(DIR_DATA, "nlsy97_covid_displacement", "nlsy97_covid_displacement.R")
pth_supp_disp  <- file.path(DIR_DATA, "nlsy97_covid_supplement_displacement", "nlsy97_covid_supplement_displacement.R")

# ---- load raw extracts (your loader should standardize PUBID, strip suffixes) ----
disruption              <- load_nlsy97_covid(pth_disruption) %>% normalize_names()
displacement            <- load_nlsy97_covid(pth_disp) %>% normalize_names()
supplement_displacement <- load_nlsy97_covid(pth_supp_disp) %>% normalize_names()

# Sanity: one row per PUBID per feed
stopifnot(!anyDuplicated(disruption$PUBID),
          !anyDuplicated(displacement$PUBID),
          !anyDuplicated(supplement_displacement$PUBID))



# ---- build outcomes ----
covid_jobloss <- make_covid_jobloss(
  disruption = disruption,
  supp = supplement_displacement,
  disp = displacement
)

covid_disruption <- make_covid_disruption(disruption)


covid_displacement <- make_covid_displacement(
  supp = supplement_displacement,
  disp = displacement
)

covid_illness_jobloss <- make_covid_illness_jobloss(
  supp = supplement_displacement,
  disp = displacement
)

covid_care_jobloss <- make_covid_care_jobloss(
  supp = supplement_displacement
)
 
# quick checks
covid_care_jobloss %>% dplyr::count(covid_care_jobloss, name = "n") %>% print(n = 30)

# ---- combine outcomes ----
outcome_vars <- covid_jobloss %>%
  dplyr::left_join(covid_disruption,     by = "PUBID") %>%
  dplyr::left_join(covid_displacement,     by = "PUBID") %>%
  dplyr::left_join(covid_illness_jobloss,  by = "PUBID") %>%
  dplyr::left_join(covid_care_jobloss,     by = "PUBID")  

# distribution checks
outcome_vars %>%
  dplyr::count(covid_jobloss, covid_disruption, covid_displacement, covid_illness_jobloss, covid_care_jobloss, name = "n") %>%
  print(n = 30)

 

outcome_vars %>% dplyr::count(covid_displacement,    name = "n") 
outcome_vars %>% dplyr::count(covid_illness_jobloss, name = "n") 
outcome_vars %>% dplyr::count(covid_jobloss,         name = "n") 
 

# ---- export outcome_vars to intermediate for downstream scripts ----
out_dir <- here::here(DIR_INTERIM)   # DIR_INTERIM set in 00_setup.R
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

csv_file <- file.path(out_dir, "outcome_vars.csv")
rds_file <- file.path(out_dir, "outcome_vars.rds")

readr::write_csv(outcome_vars, csv_file)
saveRDS(outcome_vars, rds_file)

message("[outcome_vars] Saved: ", csv_file, " and ", rds_file)

# Return invisibly for interactive sourcing
invisible(outcome_vars)