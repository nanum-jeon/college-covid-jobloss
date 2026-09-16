# scripts/77_table_sensitivity.R

# Load data 
covid_jobloss_sens_results <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_sens_results.rds"))

# Create latex tables
covid_jobloss_sens_results

covid_jobloss_sens_results_ate <- covid_jobloss_sens_results %>% 
  select(gamma, beta, bias, ate_White, ate_Black, ate_Light_Black, ate_Dark_Black)

covid_jobloss_sens_results_nde <- covid_jobloss_sens_results %>% 
  select(gamma, beta, bias, nde_White, nde_Black, nde_Light_Black, nde_Dark_Black)

covid_jobloss_sens_results_nie <- covid_jobloss_sens_results %>% 
  select(gamma, beta, bias, nie_White, nie_Black, nie_Light_Black, nie_Dark_Black)


exclude_cols <- c("gamma", "beta", "bias")
colnames(covid_jobloss_sens_results_ate) <- gsub("_", " ", colnames(covid_jobloss_sens_results_ate))
colnames(covid_jobloss_sens_results_nde) <- gsub("_", " ", colnames(covid_jobloss_sens_results_nde))
colnames(covid_jobloss_sens_results_nie) <- gsub("_", " ", colnames(covid_jobloss_sens_results_nie))


sens_ate_latex_table <- covid_jobloss_sens_results_ate  %>%
  mutate(across(!all_of(exclude_cols), ~ paste0(sprintf("%.3f", .)))) %>%
  kable(format = "latex", 
        booktabs = TRUE, 
        col.names = c("\\multicolumn{1}{c}{$\\boldsymbol{\\gamma}$}", 
                      "\\multicolumn{1}{c}{$\\boldsymbol{\\beta}$}", 
                      "\\multicolumn{1}{c}{\\textbf{bias}}",
                      "\\multicolumn{1}{c}{\\textbf{White}}",
                      "\\multicolumn{1}{c}{\\textbf{Black}}",
                      "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                      "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
        align = "lccclcc", 
        escape = FALSE, 
        linesep = "", 
        caption = paste("Sensitivity Analysis: Total Effect ($\\overline{\\tau}_{|R}$) of College on COVID-19 Job Loss by by Race and Skin Color")) %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(1:3, latex_column_spec = "S\\[table-format=1.3,table-column-width=1.5cm\\]") %>%
  column_spec(4:8, latex_column_spec = "S\\[table-format=1.3,table-column-width=2cm\\]") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")



sens_nde_latex_table <- covid_jobloss_sens_results_nde  %>%
  mutate(across(!all_of(exclude_cols), ~ paste0(sprintf("%.3f", .)))) %>%
  kable(format = "latex", 
        booktabs = TRUE, 
        col.names = c("\\multicolumn{1}{c}{$\\boldsymbol{\\gamma}$}", 
                      "\\multicolumn{1}{c}{$\\boldsymbol{\\beta}$}", 
                      "\\multicolumn{1}{c}{\\textbf{bias}}",
                      "\\multicolumn{1}{c}{\\textbf{White}}",
                      "\\multicolumn{1}{c}{\\textbf{Black}}",
                      "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                      "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
        align = "lccclcc", 
        escape = FALSE, 
        linesep = "", 
        caption = paste("Sensitivity Analysis: Direct Effect ($\\overline{\\zeta}_{|R}$) of College on COVID-19 Job Loss by by Race and Skin Color")) %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(1:3, latex_column_spec = "S\\[table-format=1.3,table-column-width=1.5cm\\]") %>%
  column_spec(4:8, latex_column_spec = "S\\[table-format=1.3,table-column-width=2cm\\]") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

 

sens_nie_latex_table <- covid_jobloss_sens_results_nie  %>%
  mutate(across(!all_of(exclude_cols), ~ paste0(sprintf("%.3f", .)))) %>%
  kable(format = "latex", 
        booktabs = TRUE, 
        col.names = c("\\multicolumn{1}{c}{$\\boldsymbol{\\gamma}$}", 
                      "\\multicolumn{1}{c}{$\\boldsymbol{\\beta}$}", 
                      "\\multicolumn{1}{c}{\\textbf{bias}}",
                      "\\multicolumn{1}{c}{\\textbf{White}}",
                      "\\multicolumn{1}{c}{\\textbf{Black}}",
                      "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                      "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
        align = "lccclcc", 
        escape = FALSE, 
        linesep = "", 
        caption = paste("Sensitivity Analysis: Indirect Effect ($\\overline{\\delta}_{|R}$) of College on COVID-19 Job Loss by by Race and Skin Color")) %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(1:3, latex_column_spec = "S\\[table-format=1.3,table-column-width=1.5cm\\]") %>%
  column_spec(4:8, latex_column_spec = "S\\[table-format=1.3,table-column-width=2cm\\]") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")



# --- Write out ---

# ATE
out_sens_ate_tex <- file.path(DIR_RESULTS, "table", "table_sens_ate.tex")
writeLines(sens_ate_latex_table, out_sens_ate_tex)
message("LaTeX table written: ", out_sens_ate_tex)

# NDE
out_sens_nde_tex <- file.path(DIR_RESULTS, "table", "table_sens_nde.tex")
writeLines(sens_nde_latex_table, out_sens_nde_tex)
message("LaTeX table written: ", out_sens_nde_tex)

# NIE
out_sens_nie_tex <- file.path(DIR_RESULTS, "table", "table_sens_nie.tex")
writeLines(sens_nie_latex_table, out_sens_nie_tex)
message("LaTeX table written: ", out_sens_nie_tex)