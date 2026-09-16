# src/io.R

# ---- CPI loader ----
load_cpi <- function(path, base_year = 2022) {
  cpi <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(year = lubridate::year(DATE)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(cpi = mean(CPIAUCSL), .groups = "drop")
  list(cpi = cpi, base_year = base_year)
}

# --- Generic safe source: returns `new_data` as tibble, no globals leaked ---
source_to_df <- function(script_path) {
  env <- new.env(parent = globalenv()) # easiest during development
  sys.source(script_path, envir = env)
  if (!exists("new_data", envir = env, inherits = FALSE)) {
    stop("`new_data` not found after sourcing: ", script_path)
  }
  tibble::as_tibble(env$new_data)
}

# --- Standardizer used by specific loaders ---
.standardize_nlsy <- function(df, id_col = "PUBID_1997", strip_suffix = "_XRND$") {
  df |>
    dplyr::rename(PUBID = all_of(id_col)) |>
    dplyr::rename_with(~ gsub(strip_suffix, "", .x))
}

# --- Specific loader you used to call `load_nlsy97_core()` ---
load_nlsy97_core <- function(script_path) {
  source_to_df(script_path) |>
    .standardize_nlsy(id_col = "PUBID_1997", strip_suffix = "_XRND$")
}

# --- If you have COVID extracts with *_COVID suffixes, make a variant ---
load_nlsy97_covid <- function(script_path) {
  source_to_df(script_path) |>
    .standardize_nlsy(id_col = "PUBID_1997", strip_suffix = "_COVID$")
}
 
# --- Remote index related data --- #

load_onet_remote <- function(path){
  onet_remote_raw <- read.csv(path, header = TRUE, 
                              col.names = c("onet_soc", "title", "element", "value"))
}

load_coc_soc_xwalk <- function(path){
  coc_soc_xwalk <- read.csv(path, header = TRUE, col.names = c("coc_2002", "soc_2002")) %>%
    mutate(soc = str_extract(soc_2002, "\\d{2}-\\d{1}"))
  
}

 
