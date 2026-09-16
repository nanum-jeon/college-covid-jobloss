# scripts/32_create_mediator.R
# -----------------------------------------
# Creates mediators in 2019
# -----------------------------------------

source(here::here("scripts", "00_setup.R"))  # sets DIR_* and loads packages
# 00_setup.R should source your src/ files that define the helpers above.

message("[31] Loading NLSY97 data...")

raw_path <- here::here(DIR_DATA, "college_covid_jobloss", "college_covid_jobloss.R")
onet_path <- here::here(DIR_DATA, "face_remote_values.csv")
coc_soc_xwalk_path <- here::here(DIR_DATA, "2002_coc_soc_crosswalk.csv")

raw <- load_nlsy97_core(raw_path)
onet_remote_raw <- load_onet_remote(onet_path)
coc_soc_xwalk <- load_coc_soc_xwalk(coc_soc_xwalk_path)

 

# Mediator A: high-skill (strict 2019)
highskill <- make_highskill_mediator(raw)

# Mediator B: employed in 2019
employed  <- compute_employed_2019(raw)

# Mediator C: remote index (strict 2019; SOC granularity matched to crosswalk)
#onet_remote_idx <- make_onet_remote_index(onet_remote_raw)
#remote <- make_remote_mediator(raw)
 

# ---------------------
# Load hiskil19 
# ---------------------
stata_path <- here::here(DIR_DATA, "edurose97_socioeconomicoutcomes_20211226.dta")

hiskil19 <- read_dta(stata_path) %>% 
  mutate(PUBID = R0000100) %>%
  dplyr::select(PUBID, hiskil19) 

  

# Combine
mediator_vars <- highskill %>%
  left_join(employed, by = "PUBID") %>%
 # left_join(remote, by = "PUBID") %>% 
  left_join(hiskil19, by = "PUBID")

mediator_vars %>%
  count(employed_2019, highskill_19, hiskil19)



# ---- export mediator_vars to intermediate for downstream scripts ----
out_dir <- here::here(DIR_INTERIM)   # DIR_INTERIM set in 00_setup.R
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

csv_file <- file.path(out_dir, "mediator_vars.csv")
rds_file <- file.path(out_dir, "mediator_vars.rds")

readr::write_csv(mediator_vars, csv_file)
saveRDS(mediator_vars, rds_file)

message("[mediator_vars] Saved: ", csv_file, " and ", rds_file)

# Return invisibly for interactive sourcing
invisible(mediator_vars)
