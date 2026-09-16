# scripts/60_figure_alt_outcome.R


# load results  
combined_displacement_results <- readRDS(file.path(DIR_RESULTS, "pooled_means_college_skill_covid_displacement.rds"))

 
# reorganize the data for plot 
displacement_figure_data <- combined_displacement_results %>%
  filter(group != "all") %>%
  filter(analysis == "outcome_conditional") %>%
  mutate(group = case_when(
    group == "white" ~ "White", 
    group == "black" ~ "Black", 
    group == "light_black" ~ "Light Black", 
    group == "dark_black" ~ "Dark Black"
  )) %>%
  mutate(group = factor(group, levels = c("Dark Black", "Light Black", "Black", "White"))) %>%
  mutate(mediator = case_when(
    hiskil19 == 1 ~ "High Skilled", 
    hiskil19 == 0 ~ "Non High Skilled"
  )) %>%
  mutate(mediator = factor(mediator, levels = c("Non High Skilled", "High Skilled"))) %>% 
  mutate(treatment = case_when(
    compcoll25 == 1 ~ "College by age 25", 
    compcoll25 == 0 ~ "No College by age 25"
  )) %>% 
  mutate(treatment = factor(treatment, levels = c("No College by age 25", "College by age 25"))) %>% 
  filter(group != "Black")


# Plot 
displacement_figure_plot <- ggplot(displacement_figure_data, aes(x = treatment, y = mean, fill = mediator)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), 
                width = 0.25, alpha= 0.5) +
  labs(x = "", y = "COVID-19 Job Displacement", fill = "mediator",
       title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") + 
  facet_wrap(~group, ncol = 4) + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, by = 0.05),
    labels = scales::number_format(accuracy = 0.05)
  ) +
  guides(fill = guide_legend(title = NULL))



# 2) Save safely (lets ggsave handle the path)
displacement_figure_plot
 
ggsave(
  filename = "figure_alt_outcome.png",
  plot     = displacement_figure_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_alt_outcome.png"), mustWork = FALSE),
  "\n"
)
