              ## 1. Create Hypothetical Data
# We create a "long" format data frame, which is the ideal input for ggplot2.

# Define the x-axis: a sequence of propensity scores from 0 to 1
sorting_score <- seq(0.1, 0.9, by = 0.01)

# Create the data frame
plot_data <- tibble(
  # Repeat the propensity score for each of our three scenarios
  propensity = rep(sorting_score, 2),
  
  # Calculate the "effect" for each scenario
  effect = c(
    -0.3 + 0.3* sorting_score,   # Negative Sorting: effect decreases with propensity 
    -0.0 - 0.3 * sorting_score    # Positive Sorting: effect increases with propensity
  ),
  
  # Create a grouping variable to distinguish the lines
  selection_type = factor(
    rep(c("Negative Selection", "Positive Selection"), each = length(sorting_score)),
    levels = c("Negative Selection", "Positive Selection") # Set factor order
  )
)


## 2. Create a new data frame specifically for the labels
# This gives us full control over the text and its placement.
label_data <- tibble(
  # Define the x-axis position for each label
  propensity = c(0.65, 0.05), # x-position for Positive, Negative
  
  # Define the y-axis position for each label
  effect = c(-0.35, -0.35), # y-position for Positive, Negative
  
  # Define the label text
  label_text = c(
    "Positive Selection Hypothesis (H2):\nThose more likely to complete college\nsee the greatest protective benefit.",
    "Negative Selection Hypothesis (H1):\nThose less likely to complete college\nsee the greatest protective benefit."
  ),
  
  # Add the grouping variable to match the line colors
  selection_type = factor(c("Positive Selection", "Negative Selection"),
                          levels = c("Negative Selection", "Positive Selection"))
)

## 2. Generate the Plot
hypothesis_plot <- ggplot(plot_data, aes(x = propensity, y = effect, color = selection_type)) +
  
  # Add the lines, making them slightly thicker for clarity
  geom_line(linewidth = 1.2) +
  
  # Manually set the colors for better communication
  # Green for positive, red/orange for negative, blue for the baseline
  scale_color_manual(values = c(
    "Positive Selection" = "forestgreen",
    "Negative Selection" = "firebrick"
  )) +
  # Add clear labels and a title
  labs(
#    title = "Relationship Between Sorting and Treatment Effect",
#    subtitle = "Visualizing Positive and Negative Sorting",
    x = "Propensity Score of College Completion",
    y = "Treatment Effect",
    color = "" # This renames the legend title
  ) +
  
  # Use a clean, minimal theme
  theme_classic() +
  # Additional theme adjustments for a polished look
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"), 
    axis.ticks = element_blank(), #<-- REMOVE AXIS TICKS
    axis.text = element_blank()   #<-- REMOVE AXIS TEXT LABELS
  ) + 
  scale_x_continuous(
    limits = c(-0.05, 1),
    breaks = seq(-0.05, 1, by = 0.1),
    labels = scales::number_format(accuracy = 0.1) 
  ) +
  scale_y_continuous(
    limits = c(-0.35, 0),
    breaks = seq(-0.35, 0, by = 0.1) 
  ) + 
  # Add the text labels using our new label_data
  geom_text(
    data = label_data, 
    aes(label = label_text), 
   #fontface = "bold", 
    vjust = "bottom", # Adjusts vertical position
    hjust = "left", # Adjusts horizontal position
    lineheight = 1,  # Space between lines of text
    size = 3,
    show.legend = FALSE # We don't need a legend for the text itself
  ) + 
  annotate(
    "text",
    x = -0.01, y = -0.1,   # Position of the text
    label = "Greater protection against\nCOVID-19 job loss",
    color = "gray20",
    size = 3,
    angle = 90,        # Rotate the text to be vertical
    vjust = -0.3       # Adjust justification to move text away from the arrow
  ) +
  annotate(
    "segment",
    x = -0.01, y = 0, xend = -0.01, yend = -0.2, # Position of the vertical arrow
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "gray20"
  )


hypothesis_plot
# Save safely (lets ggsave handle the path)
 
ggsave(
  filename = "figure_hypothesis_plot.png",
  plot     = hypothesis_plot,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_hypothesis_plot.png"), mustWork = FALSE),
  "\n"
)