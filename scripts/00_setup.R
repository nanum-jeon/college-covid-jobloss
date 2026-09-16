# scripts/00_setup.R
 
# ---- packages ----
required_pkgs <- c(
  "here","readr","dplyr","tidyr","stringr","lubridate",
  "ggplot2","scales","janitor","broom","haven","readxl", "forcats", "gt",
  "survey", "mitools", "xtable", "srvyr", "gbm", "ranger", "rlang", "glmnet",
  "rsample", "caret", "SuperLearner", "purrr", "mice", "kableExtra", "tidyverse", 
  "ggrepel", "patchwork", "cowplot", "RColorBrewer", "lme4", "stargazer", "forcats"
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
  c(DIR_INTERIM, DIR_PREP, DIR_STATA, DIR_RESULTS, DIR_LOGS),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

dir.create(file.path(DIR_RESULTS, "figure"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DIR_RESULTS, "table"), showWarnings = FALSE, recursive = TRUE)

# ---- source reusable functions ----
invisible(lapply(list.files(file.path(PARENT, "src"), pattern = "\\.R$", full.names = TRUE), source))


# ---- options ----
options(dplyr.summarise.inform = FALSE)
set.seed(90066)
