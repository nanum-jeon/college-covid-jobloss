# scripts/00_setup.R
 
# ---- packages ----
required_pkgs <- c(
  "here","readr","dplyr","tidyr","stringr","lubridate",
  "ggplot2","scales","janitor","broom","haven","readxl", "forcats", "gt",
  "survey", "mitools", "xtable", "srvyr", "gbm", "ranger", "rlang", "glmnet",
  "rsample", "caret", "SuperLearner", "purrr", "mice", "kableExtra", "tidyverse", 
  "ggrepel", "patchwork", "cowplot", "RColorBrewer", "lme4", "stargazer"
)
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- parent directory ----
PARENT  <- here::here()

# ---- paths ----
DIR_DATA      <- file.path(PARENT, "data")
DIR_INTERIM   <- file.path(PARENT, "intermediate")
DIR_RESULTS   <- file.path(PARENT, "results")
DIR_LOGS      <- file.path(PARENT, "logs")

invisible(lapply(
  c(DIR_INTERIM, DIR_RESULTS, DIR_LOGS),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

dir.create(file.path(DIR_RESULTS, "figure"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DIR_RESULTS, "table"), showWarnings = FALSE, recursive = TRUE)

# ---- source reusable functions ----
# only helper files (names not starting with a number), so no analysis runs on setup
invisible(lapply(list.files(file.path(PARENT, "src"), pattern = "^[^0-9].*\\.R$", full.names = TRUE), source))


# ---- options ----
options(dplyr.summarise.inform = FALSE)
set.seed(90066)
