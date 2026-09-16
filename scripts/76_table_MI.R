# scripts/62_tables_MI.R
# Create LaTeX table(s) for MI-pooled descriptives
# - input:  results/mi_pooled_descriptives.csv
# - output: results/mi_pooled_descriptives.tex
# Requires LaTeX packages: booktabs, threeparttable (and kableExtra)

source("src/labels_utils.R")
source("src/utils.R")

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(knitr); library(kableExtra)
})

# Paths
if (!exists("DIR_RESULTS")) DIR_RESULTS <- "results"
in_csv  <- file.path(DIR_RESULTS, "mi_pooled_descriptives.csv")
out_tex <- file.path(DIR_RESULTS, "mi_pooled_descriptives.tex")

# Read
stopifnot(file.exists(in_csv))
num <- read_csv(in_csv, show_col_types = FALSE)

# Optional reorder by var_order
num <- reorder_by_var(num, var_order)  # lives in utils.R

# Check alignment (set strict=TRUE in utils if you want to stop on mismatch)
check_var_alignment(var_order, var_labels, var_section)

default_section <- "Other"

# --- Helpers (yours) ---
fmt3 <- function(x) ifelse(is.na(x), "", sprintf("%.3f", x))
fmt3_paren <- function(x, y) ifelse(is.na(x), "", sprintf("%.3f (%.3f)", x, ifelse(is.na(y), 0, y)))
fmt_missing <- function(n_miss, prop) {
  ifelse(is.na(n_miss), "",
         sprintf("%d (%.1f\\%%)", n_miss, ifelse(is.na(prop), 0, 100 * prop)))
}


# --- Build display table (split numeric cols) ---
tab <- num %>%
  mutate(
    Variable = ifelse(variable %in% names(var_labels), var_labels[variable], variable),
    Section  = ifelse(variable %in% names(var_section), var_section[variable], default_section),
    
    # plain numeric strings for S-columns
    `CC Mean`      = fmt3(cc_mean),
    `CC SD`        = fmt3(cc_sd),
    `MI Mean`      = fmt3(mi_mean),
    `MI SE`        = fmt3(mi_se),
    `Δ (MI − CC)`  = fmt3(diff_cc_mi),   # can be negative; S[+1.3] will show sign if present
    Missing        = fmt_missing(n_missing, prop_imputed) # text like "12 (3.4\%)"
  ) %>%
  mutate(
    Section  = factor(Section, levels = section_levels),
    variable = factor(variable, levels = var_order)
  ) %>%
  arrange(Section, variable) %>%
  select(Section, Variable, `CC Mean`, `CC SD`, `MI Mean`, `MI SE`, `Δ (MI − CC)`, Missing)

# --- LaTeX table (S columns; headers wrapped to avoid parsing) ---
base_tbl <- kable(
  tab %>% select(-Section),
  format   = "latex",
  booktabs = TRUE,
  align    = c(
    "l",
    "S[table-format=2.3]",  # CC Mean
    "S[table-format=2.3]",  # CC SD
    "S[table-format=2.3]",  # MI Mean
    "S[table-format=2.3]",  # MI SE
    "S[table-format=+1.3]", # Δ (allow sign)
    "r"                     # Missing is text like "12 (3.4\%)"
  ),
  caption  = "Comparison of Complete-Case and Imputed Samples",
  col.names = c(
    "",
    "\\multicolumn{1}{c}{\\textbf{Mean}}",
    "\\multicolumn{1}{c}{\\textbf{SD}}",
    "\\multicolumn{1}{c}{\\textbf{Mean}}",
    "\\multicolumn{1}{c}{\\textbf{SE}}",
    "\\multicolumn{1}{c}{\\textbf{$\\Delta$ (MI$-$CC)}}",
    "\\textbf{Missing}"
  ),
  escape = FALSE
) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" " = 1, "Complete-case" = 2, "Multiple imputation" = 2, "Difference" = 1, " " = 1), bold = TRUE) %>%
  footnote(
    symbol = "\\\\textit{Note:} Complete-case columns report the mean and standard deviation. Multiple-imputation columns report the pooled mean and standard error. The last column reports the number of missing observations and the percent missing out of total cases. Multiple imputation used 5 datasets, classification and regression trees (CART), up to 5 iterations.",
    symbol_manual = " ",
    threeparttable = TRUE,
    escape = FALSE
  )
# --- Section headers with pack_rows() ---
sec_idx <- split(seq_len(nrow(tab)), tab$Section)
latex_tbl <- base_tbl
for (s in section_levels[section_levels != "Other"]) {
  if (!is.null(sec_idx[[s]])) {
    rng <- range(sec_idx[[s]])
    latex_tbl <- kableExtra::pack_rows(latex_tbl, s, start = rng[1], end = rng[2])
  }
}
 

# --- Patch kableExtra's wrong column count in \multicolumn{<big>}{l}{...} ---
# kableExtra sometimes emits \multicolumn{98}{l}{...} when using S columns.
# Replace any \multicolumn{<digits>}{l} with the true number of columns.
ncols <- ncol(tab) - 1L  # exclude Section; equals columns in the printed table
latex_tbl <- gsub(
  "\\\\multicolumn\\{\\d+\\}\\{l\\}",
  paste0("\\\\multicolumn\\{", ncols, "\\}\\{l\\}"),
  latex_tbl
)

# --- Write out ---
  
# make the subfolder if needed
dir.create(file.path(DIR_RESULTS, "table"), showWarnings = FALSE, recursive = TRUE)

# now write out
out_tex <- file.path(DIR_RESULTS, "table", "table_imputation_results.tex")
writeLines(latex_tbl, out_tex)
message("LaTeX table written: ", out_tex)

 
