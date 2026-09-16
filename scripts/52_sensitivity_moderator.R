# scripts/52_sensitivity_moderator.R
# -----------------------------------------
# De-bias interviewers' measurements on skin color vars
# -----------------------------------------

# Load data 
source(here::here("scripts", "00_setup.R"))  # sets DIR_* and loads packages
# 00_setup.R should source your src/ files that define the helpers above.
 

# interviewer  data 
moderator_path <- here::here(DIR_INTERIM, "moderator_vars.rds")
moderator_vars <- readRDS(moderator_path)



# --- A. Check the distribution of your outcome variable ---
# A histogram is great for seeing the spread of the skin color ratings.
ggplot(moderator_vars, aes(x = skin_tone_scale)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Skin Tone Scale", x = "Skin Tone Scale", y = "Count") +
  theme_minimal()

 
# Create the density plot
sensitivity_moderator_plot <- ggplot(moderator_vars, aes(x = skin_tone_scale)) +
  
  # Add the density layer for the original ratings (in blue)
  geom_density(aes(fill = "Original Scale"), alpha = 0.6) +
  
  # Add the density layer for the predicted ratings (in red)
  geom_density(aes(x = skin_tone_debiased, fill = "Adjusted Scale (Debiased)"), alpha = 0.6) +
  
  # Improve the labels and appearance
  labs(
    #title = "Distribution of Original vs. Adjusted Skin Color Scale",
    x = "Skin Color Scale",
    y = NULL,  # Fixed: use NULL instead of "none"
    #fill = "Variable Type"
  ) +
  scale_fill_manual(values = c("Original Scale" = "skyblue", "Adjusted Scale (Debiased)" = "salmon")) +
  theme_bw() +
  scale_x_continuous(
    limits = c(-3, 12),
    breaks = seq(-3, 12, by = 1),
    labels = scales::number_format(accuracy = 1)
  ) +
  guides(fill = guide_legend(title = NULL)) + 
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank()  # Fixed: use element_blank() instead of "none"
  )



##--------- save --------- 

ggsave(
  filename = "figure_sensitivity_moderator.png",
  plot     = sensitivity_moderator_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)


# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_sensitivity_moderator.png"), mustWork = FALSE),
  "\n"
)


  