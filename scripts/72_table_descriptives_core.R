# scripts/63_table_descriptive_core_vars.R

 
# load results  
combined_results <- readRDS(file.path(DIR_RESULTS, "pooled_means_college_skill_covid.rds"))

combined_results %>% print(n = 100)
# ====== Build the table ======
  
# Create the base table
tab_core_vars <- combined_results %>%
  pivot_longer(cols = c(mean, se), names_to = "type", values_to = "value") %>%
  mutate(
    final = case_when(
      type == "mean" ~ sprintf("%.3f", value), 
      type == "se" ~ paste0("(", sprintf("%.3f", value), ")")
    )
  ) %>%
  select(analysis, compcoll25, hiskil19, group, type, final) %>%
  pivot_wider(names_from = group, values_from = final) %>% 
  arrange(analysis)

 

# Add descriptive labels
tab_core_vars <- tab_core_vars %>% 
  filter(analysis != "outcome_moderator")  %>% 
  mutate(
    label = c(
      "{High-skilled, No College}", "{}", 
      "{High-skilled, College}", "{}", 
      "{COVID-19 Job Loss, No College}", "{}", 
      "{COVID-19 Job Loss, College}", "{}",
      "{COVID-19 Job Loss, No College, No High-skilled}", "{}", 
      "{COVID-19 Job Loss, College, No High-skilled}", "{}",
      "{COVID-19 Job Loss, No College, High-skilled}", "{}", 
      "{COVID-19 Job Loss, College, High-skilled}", "{}"
    )
  ) %>%
  select(label, all, white, black, light_black, dark_black)
 
 
 
tab_core_vars_latex <- tab_core_vars %>%
  kbl(
    format   = "latex",
    caption  = "Distributions of High Skilled Occupations in 2019 (M) and COVID-19 Job Loss (Y) by College Completion (A) and Race and Skin Color (R)",
    booktabs = TRUE,
    # <-- no multicolumn in headers; just plain text
    col.names = c("",
                  "\\multicolumn{1}{c}{\\textbf{All}}", 
                  "\\multicolumn{1}{c}{\\textbf{White}}",
                  "\\multicolumn{1}{c}{\\textbf{Black}}",
                  "\\multicolumn{1}{c}{\\textbf{Light Black}}",
                  "\\multicolumn{1}{c}{\\textbf{Dark Black}}"), 
    align    = c("l", rep("r", 5)),
    escape   = FALSE,
    linesep  = "",
    label = "descriptives_core",
  ) %>%
  # use S columns; if your LaTeX is siunitx v3, you may keep table-column-width; if spacing looks odd, drop it
  column_spec(2:6, latex_column_spec = "S\\[table-format=1.3,table-column-width=1.2cm\\]") %>%
  column_spec(1, width = "7.5cm") %>%
  kable_styling(font_size = 10, latex_options = c("hold_position"), 
                full_width = FALSE) %>% 
  row_spec(c(4, 8), extra_latex_after = "\\midrule")


# 7) Optional footnote (no need to gsub multicolumn widths)
tab_core_vars_latex <- tab_core_vars_latex %>%
  footnote(
    general = "{Survey-weighted descriptive statistics present three key proportions: (1) high-skilled occupation rates by college completion status (completed a four-year degree by age 25), (2) COVID-19 job loss rates by the same college completion status, and (3) COVID-19 job loss rates jointly stratified by college completion status and occupation skill level.}",    
    general_title = "{Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

 
# --- Write out ---

dir.create(file.path(DIR_RESULTS, "table"), showWarnings = FALSE, recursive = TRUE)

# now write out
out_tex <- file.path(DIR_RESULTS, "table", "table_descriptive_core_vars.tex")
writeLines(tab_core_vars_latex, out_tex)
message("LaTeX table written: ", out_tex)

