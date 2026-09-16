# scripts/70_table_mediation_effects_covid_jobloss_intersectional.R

# Load data 
covid_jobloss_intersectional_results <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_intersectional_results.rds"))
merged_list <- readRDS(file.path(DIR_INTERIM, "merged_list.rds"))
 
## --------- Among Women --------- ## 

# Create regression table (main section)
covid_jobloss_female1_regression_table <- create_regression_table(covid_jobloss_intersectional_results$female1_results)

# Calculate proportion mediated by direct and indirect effects
covid_jobloss_female1_effect_proportion <- calculate_proportion(covid_jobloss_intersectional_results$female1_results)

# Calculate N for women by race and skin color 
covid_jobloss_female1_n <- make_intersectional_N(merged_list, female = 1)

# Combine sections of table 
covid_jobloss_female1_latex_table <- covid_jobloss_female1_regression_table %>%
  bind_rows(covid_jobloss_female1_effect_proportion) %>%
  bind_rows(covid_jobloss_female1_n) %>% 
  kbl(format = "latex", 
      booktabs = TRUE, 
      col.names = c("", 
                    "\\multicolumn{1}{c}{\\textbf{White}}", 
                    "\\multicolumn{1}{c}{\\textbf{Black}}",
                    "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                    "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
      align = "lcccc", 
      escape = FALSE, 
      label = "covid-jobloss-effects-female1",
      linesep = c(rep("", 5), "\\midrule", "", "\\midrule", rep("", nrow(tbl_f)-8)), 
      caption = "College Effects on COVID-19 Job Loss among Women by Race and Skin Color: Total, Indirect Through High-Skilled Occupations, and Direct Effects") %>%
  column_spec(2:5, latex_column_spec = "S\\[table-format=1.3,table-column-width=2.5cm\\]") %>%
  column_spec(1, width = "4.5cm") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")

 
covid_jobloss_female1_latex_table <- covid_jobloss_female1_latex_table %>%
  footnote(
    general = "{\\\\footnotesize $^{*}$p$<$0.05; $^{**}$p$<$0.01;  $^{***}$p$<$0.001 (two-tailed tests). The negative signs indicate that completing college by age 25 reduces the probability of experiencing COVID-19 job loss. 
    `Total' denotes the average treatment effect, decomposed into the natural indirect effect (NIE) through employment in high-skilled occupations and the natural direct effect (NDE), which captures the residual protective effect of college through all other pathways. `Prop. NIE' and `Prop. NDE' show the proportion of total effects transmitted through each causal pathway.}",
    general_title = "{\\\\footnotesize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

covid_jobloss_female1_latex_table

# --- Write out ---
 
# now write out
out_tex <- file.path(DIR_RESULTS, "table", "table_mediation_results_covid_jobloss_female1.tex")
writeLines(covid_jobloss_female1_latex_table, out_tex)
message("LaTeX table written: ", out_tex)


## --------- Among Men --------- ## 
 

# Create regression table (main section)
covid_jobloss_female0_regression_table <- create_regression_table(covid_jobloss_intersectional_results$female0_results)

# Calculate proportion mediated by direct and indirect effects
covid_jobloss_female0_effect_proportion <- calculate_proportion(covid_jobloss_intersectional_results$female0_results)

# Calculate N for women by race and skin color 
covid_jobloss_female0_n <- make_intersectional_N(merged_list, female = 0)

# Combine sections of table 
covid_jobloss_female0_latex_table <- covid_jobloss_female0_regression_table %>%
  bind_rows(covid_jobloss_female0_effect_proportion) %>%
  bind_rows(covid_jobloss_female0_n) %>% 
  kbl(format = "latex", 
      booktabs = TRUE, 
      col.names = c("", 
                    "\\multicolumn{1}{c}{\\textbf{White}}", 
                    "\\multicolumn{1}{c}{\\textbf{Black}}",
                    "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                    "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
      align = "lcccc", 
      escape = FALSE, 
      label = "covid-jobloss-effects-female0",
      linesep = c(rep("", 5), "\\midrule", "", "\\midrule", rep("", nrow(tbl_f)-8)), 
      caption = "College Effects on COVID-19 Job Loss among Men by Race and Skin Color: Total, Indirect Through High-Skilled Occupations, and Direct Effects") %>%
  column_spec(2:5, latex_column_spec = "S\\[table-format=1.3,table-column-width=2.5cm\\]") %>%
  column_spec(1, width = "4.5cm") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")


covid_jobloss_female0_latex_table <- covid_jobloss_female0_latex_table %>%
  footnote(
    general = "{\\\\footnotesize $^{*}$p$<$0.05; $^{**}$p$<$0.01;  $^{***}$p$<$0.001 (two-tailed tests). 
    The negative signs indicate that completing college by age 25 reduces the probability of experiencing COVID-19 job loss. 
    `Total' denotes the average treatment effect, decomposed into the natural indirect effect (NIE) through employment in high-skilled occupations and the natural direct effect (NDE), which captures the residual protective effect of college through all other pathways. `Prop. NIE' and `Prop. NDE' show the proportion of total effects transmitted through each causal pathway.}",
    general_title = "{\\\\footnotesize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

covid_jobloss_female0_latex_table

 
# --- Write out ---

# now write out
out_tex <- file.path(DIR_RESULTS, "table", "table_mediation_results_covid_jobloss_female0.tex")
writeLines(covid_jobloss_female0_latex_table, out_tex)
message("LaTeX table written: ", out_tex)