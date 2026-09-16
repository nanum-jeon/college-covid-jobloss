# scripts/61_tables.R

# Load descriptive statistics data 

des_df <- readRDS(here::here(DIR_RESULTS, "des_df.rds"))


# 1) Split N from main, format variable names with visible braces

n_row <- des_df %>% filter(variable == "N")
n_line <- n_row %>%
  mutate(variable = "{\\textbf{\\textit{N}}}") %>%
  mutate(across(-variable, ~ paste0("{", format(as.numeric(.x), big.mark = ","), "}")))

main_data <- des_df %>%
  filter(variable != "N") %>%
  mutate(
    section  = des_var_section[variable],
    section  = factor(section, levels = des_section_levels),
    # show literal braces in LaTeX:
    variable = sprintf("{ %s }", variable)
  ) %>%
  fill(section, .direction = "down") 



# 2) Keep section run-lengths for pack_rows indices
run_lengths <- main_data %>%
  group_by(section, variable) %>%
  summarise(n_rows = n(), .groups = "drop")


# Select white, light black, and dark black only 
table_data <- bind_rows(main_data %>% select(variable, all, white, black, light_black, dark_black),
                        n_line) %>% select(variable,  dark_black, light_black, white )



# 4) Build kable (no S in `align`; set S via column_spec)
tab <- table_data %>%
  kbl(
    format   = "latex",
    caption  = "Descriptive Statistics in College Completion (A), High-Skilled Occupation (M), COVID-19
Job Loss (Y), and Background Characteristics (X) by Race and Skin Color (R)",
    booktabs = TRUE,
    col.names = c("", 
                  "\\multicolumn{1}{c}{\\textbf{Dark Black}}", 
                  "\\multicolumn{1}{c}{\\textbf{Light Black}}",
                  "\\multicolumn{1}{c}{\\textbf{White}}"),
    align    = c("l", rep("r", 5)),
    label = "descriptives",
    escape   = FALSE,
    row.names = FALSE
  ) %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(2:4, latex_column_spec = "S\\[table-format=1.3,table-column-width=2cm\\]") %>%
  # first (label) column width
  column_spec(1, width = "6.5cm") %>%
  kable_styling(font_size = 8, latex_options = "hold_position")

# 5) Add section groupings + remember end rows
current_row <- 1
section_end_rows <- integer(0)

for (sec in des_section_levels) {
  sec_df <- run_lengths %>% dplyr::filter(section == sec)
  if (nrow(sec_df) == 0) next
  total_rows <- sum(sec_df$n_rows)
  start_row  <- current_row
  end_row    <- current_row + total_rows - 1
  
  tab <- tab %>% pack_rows(as.character(sec), start_row, end_row,
                           latex_gap_space = "0.2em",
                           bold = TRUE)
  
  section_end_rows <- c(section_end_rows, end_row)
  current_row <- end_row + 1
}

# 6) Insert a \midrule after each section (except the last one)
if (length(section_end_rows) > 1) {
  for (r in head(section_end_rows, -1)) {
    tab <- tab %>%
      row_spec(r, extra_latex_after = "\\midrule")
  }
}

# 7) Draw a rule before the final 'N' row (assuming it's the penultimate row)
tab <- tab %>%
  row_spec(nrow(table_data) - 1, extra_latex_after = "\\addlinespace\\midrule")


# 6) Emphasize the bottom N row (row = nrow(table_data))
#tab <- tab %>% row_spec(nrow(table_data), bold = TRUE)

# 7) Optional footnote (no need to gsub multicolumn widths)
tab <- tab %>%
  footnote(
    general = "{\\\\scriptsize Descriptive statistics are based on pooled means computed across five imputed data sets, taking into account the complex survey design of the NLSY97. Parents' income in 1997 has been adjusted to 2022 dollars. \\\\textit{N} represents the number of observations from the unweighted survey data.}",
    general_title = "{\\\\scriptsize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )


tab
# --- Write out ---
# make the subfolder if needed
dir.create(file.path(DIR_RESULTS, "table"), showWarnings = FALSE, recursive = TRUE)

out_tex <- file.path(DIR_RESULTS, "table", "table_descriptive.tex")
writeLines(tab, out_tex)
message("LaTeX table written: ", out_tex)

 

