# scripts/60_figures.R


# load results  
combined_results <- readRDS(file.path(DIR_RESULTS, "pooled_means_college_skill_covid.rds"))

# Define a color-blind friendly palette (e.g., from RColorBrewer's "Dark2" or "Set2")
cb_palette <- brewer.pal(n = 4, name = "Dark2") # Or "Set2" for lighter colors

dodge <- position_dodge(width = 0.35)

# Colors to match the example figure
common_cols <- c(
  "White"       = cb_palette[1], 
  "Black"       = cb_palette[2],
  "Light Black" = cb_palette[3],
  "Dark Black"  = cb_palette[4]
)
common_breaks  <- names(common_cols)
common_labels  <- c("White", "Black", "Light Black", "Dark Black")

 
## ----- plot data ----- ## 
library(dplyr)
library(ggplot2)
library(scales)

dodge <- position_dodge(width = 0.35)

# Step 1. Prepare your data
outcome_moderator_figure_data <- combined_results %>%
  filter(analysis == "outcome_moderator") %>%
  mutate(
    group = recode(group,
                   "white" = "White",
                   "black" = "Black",
                   "light_black" = "Light Black",
                   "dark_black" = "Dark Black",
                   .default = NA_character_),
    category = case_when(
      group %in% c("White", "Black") ~ "Race",
      group %in% c("Light Black", "Dark Black") ~ "Skin Color (Black Workers Only)",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(group = factor(group, levels = c("White", "Black", "Light Black", "Dark Black")))

outcome_moderator_figure_data 

# Step 3. Plot
race_figure_plot <- ggplot(
  outcome_moderator_figure_data,
  aes(x = group, y = mean, color = group)
) +
  geom_point(position = dodge) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = dodge, size = 0.5, width = 0.1
  ) +
  labs(x = NULL, y = "COVID-19 Job Loss", color = NULL) +
  facet_wrap(~ category, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, by = 0.05),
    labels = scales::number_format(accuracy = 0.05)
  ) +
  scale_color_manual(
    values = common_cols,
    breaks = common_breaks,
    labels = common_labels,
    drop   = FALSE
  )

race_figure_plot


# 2) Save safely (lets ggsave handle the path)
# make the subfolder if needed 
ggsave(
  filename = "figure_race_jobloss.png",
  plot     = race_figure_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 6,
  height   = 4,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_race_jobloss.png"), mustWork = FALSE),
  "\n"
)



###-----------------####
college_figure_data <- combined_results %>%
  filter(group != "all", analysis == "outcome_marginal") %>%
  mutate(
    # Normalize group labels
    group = recode(group,
                   "white" = "White",
                   "black" = "Black",
                   "light_black" = "Light Black",
                   "dark_black" = "Dark Black",
                   .default = NA_character_),
    group = factor(group, levels = c("White", "Black", "Light Black", "Dark Black")),
    
    # Treatment label
    treatment = case_when(
      compcoll25 == 1 ~ "College by age 25",
      compcoll25 == 0 ~ "No College by age 25",
      TRUE ~ NA_character_
    ),
    treatment = factor(treatment, levels = c("No College by age 25", "College by age 25")),
    
    # Category (race vs skin color)
    category = case_when(
      group %in% c("White", "Black") ~ "Race",
      group %in% c("Light Black", "Dark Black") ~ "Skin Color (Black only)",
      TRUE ~ NA_character_
    ),
    category = factor(category, levels = c("Race", "Skin Color (Black only)"))
  ) 

# Plot 


#### not sure if I am using this ####  
race_figure_plot_by_college <- college_figure_data %>% 
  ggplot(aes(x = treatment, y = mean)) +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = dodge, size = 0.5, width = 0.1) +
  labs(x = NULL, y = "COVID-19 Job Loss", title = NULL, color = NULL) +
  facet_wrap(~category) + 
  theme_bw(base_size = 12) +
  scale_color_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, by = 0.05),
    labels = number_format(accuracy = 0.05)
  )+
  # colors (yours)
  scale_color_manual(values = common_cols,
                     breaks = common_breaks,
                     labels = common_labels,
                     drop   = FALSE) 
  

race_figure_plot_by_college

# 2) Save safely (lets ggsave handle the path)
# make the subfolder if needed 
ggsave(
  filename = "figure_race_jobloss_by_college.png",
  plot     = race_figure_plot_by_college,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_race_jobloss_by_college.png"), mustWork = FALSE),
  "\n"
)

 