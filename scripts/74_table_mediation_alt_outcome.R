# scripts/69_table_mediation_effects_covid_displacement.R

## ---------- Covid Displacement ---------- ## 
covid_displacement_results$main_result

# Calculate proportion mediated by direct and indirect effects
covid_displacement_effect_proportion <- calculate_proportion(covid_displacement_results$main_results)

# Combine tables and generate LaTeX table 
covid_displacement_regression_table <- create_regression_table(covid_displacement_results$main_result)
 

covid_displacement_latex_table <- covid_displacement_regression_table %>%
  bind_rows(covid_displacement_effect_proportion) %>%
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
        label = "covid-displacement-effects", 
        caption = "College Effects on COVID-19 Job Displacement by Race and Skin Color: Total, Indirect Through High-Skilled Occupations, and Direct Effects") %>%
  row_spec(6, extra_latex_after = "\\midrule") %>%
  # S columns, 1.8 cm each — brackets escaped
  column_spec(2:5, latex_column_spec = "S\\[table-format=1.3,table-column-width=2.5cm\\]") %>%
  # first (label) column width
  column_spec(1, width = "4.5cm") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")



covid_displacement_latex_table <- covid_displacement_latex_table %>%
  footnote(
    general = "{\\\\footnotesize $^{*}$p$<$0.05; $^{**}$p$<$0.01;  $^{***}$p$<$0.001 (two-tailed tests). The negative signs indicate that completing college by age 25 reduces the probability of job displacement during COVID-19. `Total' denotes the average treatment effect, decomposed into the natural indirect effect (NIE) through employment in high-skilled occupations and the natural direct effect (NDE), which captures the residual protective effect of college through all other pathways. `Prop. NIE' and `Prop. NDE' show the proportion of total effects transmitted through each causal pathway.}",
    general_title = "{\\\\footnotesize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

covid_displacement_latex_table

# --- Write out --- 
 
out_tex <- file.path(DIR_RESULTS, "table", "table_mediation_results_covid_displacement.tex")
writeLines(covid_displacement_latex_table, out_tex)
message("LaTeX table written: ", out_tex)


