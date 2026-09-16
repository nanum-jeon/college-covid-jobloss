# scripts/60_figures.R


# load results  
combined_results <- readRDS(file.path(DIR_RESULTS, "pooled_means_college_skill_covid.rds"))

 
# reorganize the data for plot 
figure_data <- combined_results %>%
  filter(group != "all") %>%
  filter(analysis == "outcome_conditional") %>%
  mutate(group = case_when(
    group == "white" ~ "White", 
    group == "black" ~ "Black", 
    group == "light_black" ~ "Light Black", 
    group == "dark_black" ~ "Dark Black"
  )) %>%
  mutate(group = factor(group, levels = c("White", "Black", "Light Black", "Dark Black"))) %>%
  mutate(mediator = case_when(
    hiskil19 == 1 ~ "High Skilled", 
    hiskil19 == 0 ~ "Non High Skilled"
  )) %>%
  mutate(mediator = factor(mediator, levels = c("Non High Skilled", "High Skilled"))) %>% 
  mutate(treatment = case_when(
    compcoll25 == 1 ~ "College degree", 
    compcoll25 == 0 ~ "No college degree"
  )) %>% 
  mutate(treatment = factor(treatment, levels = c("No college degree", "College degree"))) %>% 
  filter(group != "Black")


# Plot 
figure_plot <- ggplot(figure_data, aes(x = treatment, y = mean, fill = mediator)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), 
                size = 0.5, 
                width = 0.3) +
  labs(x = "", y = "COVID-19 Job Loss", fill = "mediator",
       title = "") +
  theme_bw(base_size = 12) +
  scale_fill_brewer(palette = "Set2") + 
  facet_wrap(~group, ncol = 4) + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),
    breaks = seq(0, 0.3, by = 0.05),
    labels = scales::number_format(accuracy = 0.05)
  ) +
  guides(fill = guide_legend(title = NULL))

 

# 2) Save safely (lets ggsave handle the path)
# make the subfolder if needed 
ggsave(
  filename = "figure_jobloss.png",
  plot     = figure_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_jobloss.png"), mustWork = FALSE),
  "\n"
)



# including black  

# reorganize the data for plot 
black_figure_data <- combined_results %>%
  filter(group != "all") %>%
  filter(analysis == "outcome_conditional") %>%
  mutate(group = case_when(
    group == "white" ~ "White", 
    group == "black" ~ "Black", 
    group == "light_black" ~ "Light Black", 
    group == "dark_black" ~ "Dark Black"
  )) %>%
  mutate(group = factor(group, levels = c("White", "Black", "Light Black", "Dark Black"))) %>%
  mutate(mediator = case_when(
    hiskil19 == 1 ~ "High-skilled occupation", 
    hiskil19 == 0 ~ "Non-high-skilled occupation"
  )) %>%
  mutate(mediator = factor(mediator, levels = c("Non-high-skilled occupation", "High-skilled occupation"))) %>% 
  mutate(treatment = case_when(
    compcoll25 == 1 ~ "College degree", 
    compcoll25 == 0 ~ "No college degree"
  )) %>% 
  mutate(treatment = factor(treatment, levels = c("No college degree", "College degree")))   


# Plot 
black_figure_plot <- ggplot(black_figure_data, aes(x = treatment, y = mean, fill = mediator)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), 
                width = 0.3, alpha= 0.5) +
  labs(x = "", y = "COVID-19 Job Loss", fill = "mediator",
       title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") + 
  facet_wrap(~group, ncol = 4) + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),
    breaks = seq(0, 0.3, by = 0.05),
    labels = scales::number_format(accuracy = 0.05)
  ) +
  guides(fill = guide_legend(title = NULL))



##--------- save --------- 

ggsave(
  filename = "figure_jobloss_black.png",
  plot     = black_figure_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)


# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_jobloss_black.png"), mustWork = FALSE),
  "\n"
)

## 
# By college only 

combined_results %>%
  print(n = 100)


# reorganize the data for plot 
black_figure_college_data <- combined_results %>%
  filter(group != "all") %>%
  filter(analysis == "outcome_marginal") %>%
  mutate(group = case_when(
    group == "white" ~ "White", 
    group == "black" ~ "Black", 
    group == "light_black" ~ "Light Black", 
    group == "dark_black" ~ "Dark Black"
  )) %>%
  mutate(group = factor(group, levels = c("White", "Black", "Light Black", "Dark Black"))) %>%
  mutate(treatment = case_when(
    compcoll25 == 1 ~ "College degree", 
    compcoll25 == 0 ~ "No college degree"
  )) %>% 
  mutate(treatment = factor(treatment, levels = c("No college degree", "College degree")))  


# Plot 
black_figure_college_plot <- ggplot(black_figure_college_data, aes(x = treatment, y = mean, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(width = 0.9), 
                width = 0.3, alpha= 0.5) +
  labs(x = "", y = "COVID-19 Job Loss",
       title = "") +
  theme_bw() + 
  facet_wrap(~group, ncol = 4) + 
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),
    breaks = seq(0, 0.3, by = 0.05),
    labels = scales::number_format(accuracy = 0.05)
  ) +
  guides(fill = guide_legend(title = NULL))  +
  scale_fill_brewer(palette = "Paired")

black_figure_college_plot


##--------- save --------- 

ggsave(
  filename = "figure_jobloss_black_college.png",
  plot     = black_figure_college_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)


# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_jobloss_black_college.png"), mustWork = FALSE),
  "\n"
)

