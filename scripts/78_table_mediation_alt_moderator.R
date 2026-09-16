# scripts/65_table_mediation_effects.R

 
# Load data 
covid_jobloss_alt_moderator_results <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_alt_moderator_results.rds"))
 

# Calculate proportion mediated by direct and indirect effects
covid_jobloss_alt_moderator_effect_proportion <- calculate_proportion(covid_jobloss_alt_moderator_results$main_results)

# Combine tables and generate LaTeX table 
covid_jobloss_alt_moderator_regression_table <- create_regression_table(covid_jobloss_alt_moderator_results$main_result)


covid_jobloss_alt_moderator_latex_table <- covid_jobloss_alt_moderator_regression_table %>%
  bind_rows(covid_jobloss_alt_moderator_effect_proportion) %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      col.names = c("", 
                    "\\multicolumn{1}{c}{\\textbf{White}}", 
                    "\\multicolumn{1}{c}{\\textbf{Black}}",
                    "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                    "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
      align = "lcccc", 
      escape = FALSE, 
      linesep = "", 
      label = "covid-jobloss-effects-alt-moderator",
      caption = "College Effects on COVID-19 Job Loss by Race and Skin Color (Adjusted for Interviewer): Total, Indirect Through High-Skilled Occupations, and Direct Effects") %>%
  row_spec(6, extra_latex_after = "\\midrule") %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(2:5, latex_column_spec = "S\\[table-format=1.3,table-column-width=2.5cm\\]") %>%
  # first (label) column width
  column_spec(1, width = "4.5cm") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")



covid_jobloss_alt_moderator_latex_table <- covid_jobloss_alt_moderator_latex_table %>%
  footnote(
    general = "{\\\\footnotesize $^{*}$p$<$0.05; $^{**}$p$<$0.01;  $^{***}$p$<$0.001 (two-tailed tests). 
    The negative signs indicate that completing college by age 25 reduces the probability of experiencing COVID-19 job loss. 
    `Total' denotes the average treatment effect, decomposed into the natural indirect effect (NIE) through employment in high-skilled occupations and the natural direct effect (NDE), which captures the residual protective effect of college through all other pathways. `Prop. NIE' and `Prop. NDE' show the proportion of total effects transmitted through each causal pathway.}",
    general_title = "{\\\\footnotesize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

covid_jobloss_alt_moderator_latex_table

# --- Write out ---

out_tex <- file.path(DIR_RESULTS, "table", "table_mediation_alt_moderator_results.tex")
writeLines(covid_jobloss_alt_moderator_latex_table, out_tex)
message("LaTeX table written: ", out_tex)


