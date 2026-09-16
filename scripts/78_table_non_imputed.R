# scripts/78_table_non_imputed.R

# load data 

rds_in <- file.path(DIR_RESULTS, "covid_jobloss_non_imputed_results.rds")
covid_jobloss_non_imputed_results <- readRDS(rds_in) 

covid_jobloss_non_imputed_results
# Effect Table (vs Zero) 

formatted_non_imputed_results <- format_all_results(covid_jobloss_non_imputed_results)
 

non_imputed_summary <- create_group_means_summary(formatted_non_imputed_results)
  
non_imputed_table <- non_imputed_summary %>% 
  transmute(Effect = case_when(Outcome == "ate" ~ "Total (ATE)",
                               Outcome == "nde" ~ "Direct (NDE)", 
                               Outcome == "nie" ~ "Indirect (NIE)"), 
            Group = case_when(Group == "white" ~ "White",
                              Group == "black" ~ "Black", 
                              Group == "light_black" ~ "Light_Black", 
                              Group == "dark_black" ~ "Dark_Black"), 
            Effect = factor(Effect, levels = c("Total (ATE)", "Indirect (NIE)", "Direct (NDE)")),
            Group = factor(Group, levels = c("White", "Black", "Light_Black", "Dark_Black")), 
            Estimate = Estimate,
            SE = SE, 
            P_Value = P_Value, 
            Sig = Sig) %>% 
  as.tibble() %>% arrange(Effect, Group)
 
non_imputed_table
  
# Create regression table (main section)
non_imputed_regression_table <- create_regression_table(non_imputed_table)

 
# Calculate proportion mediated by direct and indirect effects
non_imputed_effect_proportion <- calculate_proportion(non_imputed_table)

# Calculate N by race and skin color for complete data 
non_imputed_N <- non_imputed_summary %>% 
  transmute(Effect = case_when(Outcome == "nde" ~ "\\textit{N}",
                               TRUE ~ NA),
            Group = case_when(Group == "white" ~ "White",
                              Group == "black" ~ "Black", 
                              Group == "light_black" ~ "Light_Black", 
                              Group == "dark_black" ~ "Dark_Black"), 
            Group = factor(Group, levels = c("White", "Black", "Light_Black", "Dark_Black")), 
            N = N_obs)%>% 
  arrange(Group) %>% 
  drop_na() %>%
  pivot_wider(
    names_from  = Group,
    values_from = N
  ) %>%
  mutate(across(where(is.numeric), ~ format(.x, big.mark = ",", scientific = FALSE))) %>%
  mutate(across(everything(), ~ paste0("{", .x, "}")))



# Combine sections of table 
covid_jobloss_non_imputed_latex_table <- non_imputed_regression_table %>%
  bind_rows(non_imputed_effect_proportion) %>%
  bind_rows(non_imputed_N) %>% 
  kbl(format = "latex", 
      booktabs = TRUE, 
      col.names = c("", 
                    "\\multicolumn{1}{c}{\\textbf{White}}", 
                    "\\multicolumn{1}{c}{\\textbf{Black}}",
                    "\\multicolumn{1}{c}{\\textbf{Light Black}}", 
                    "\\multicolumn{1}{c}{\\textbf{Dark Black}}"),
      align = "lcccc", 
      escape = FALSE, 
      linesep = c(rep("", 5), "\\midrule", "", "\\midrule", rep("", nrow(tbl_f)-8)), 
      label = "covid-jobloss-effects-non-imputed",
      caption = "College Effects on COVID-19 Job Loss by Race and Skin Color (Non-Imputed Sample): Total, Indirect Through High-Skilled Occupations, and Direct Effects") %>%
  column_spec(2:5, latex_column_spec = "S\\[table-format=1.3,table-column-width=2.5cm\\]") %>%
  column_spec(1, width = "4.5cm") %>%
  kable_styling(font_size = 10, latex_options = "hold_position")


covid_jobloss_non_imputed_latex_table <- covid_jobloss_non_imputed_latex_table %>%
  footnote(
    general = "{\\\\footnotesize $^{.}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01;  $^{***}$p$<$0.001 (two-tailed tests). The negative signs indicate that completing college by age 25 reduces the probability of experiencing COVID-19 job loss. 
    `Total' denotes the average treatment effect, decomposed into the natural indirect effect (NIE) through employment in high-skilled occupations and the natural direct effect (NDE), which captures the residual protective effect of college through all other pathways. `Prop. NIE' and `Prop. NDE' show the proportion of total effects transmitted through each causal pathway.}",
    general_title = "{\\\\footnotesize Note:}",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE,
    escape = FALSE
  )

covid_jobloss_non_imputed_latex_table
 

# --- Write out ---

out_tex <- file.path(DIR_RESULTS, "table", "table_mediation_non_imputed_results.tex")
writeLines(covid_jobloss_non_imputed_latex_table, out_tex)
message("LaTeX table written: ", out_tex)



  