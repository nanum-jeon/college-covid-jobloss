library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# ----------------------------
# 0) Data prep
# ----------------------------

race_df <- covid_jobloss_results$main_results %>%
  filter(Group == "White" | Group == "Black") %>% 
  mutate(
    Group = dplyr::recode(Group,
                          "Light_Black" = "Light Black",
                          "Dark_Black"  = "Dark Black",
                          .default = Group),
    Group = factor(Group, levels = c("Black", "White")),
    ci_lo = Estimate - 1.96 * SE,
    ci_hi = Estimate + 1.96 * SE
  ) %>% 
  arrange(Group)
# Color palette (match your figure)
col_ATE <- "#7F7F7F"
col_NDE <- "#F39C12"  # orange
col_NIE <- "#2E86C1"  # blue

# -----------------------------------
# 1) Forest plot for ATE & NIE & NDE
# -----------------------------------

forest_df <- race_df %>%  
  mutate(
    Effect_short = recode(Effect,
                          "Total (ATE)" = "ATE", 
                          "Direct (NDE)"  = "NDE",
                          "Indirect (NIE)" = "NIE"),
    Effect_short = factor(Effect_short, levels = c("ATE", "NIE", "NDE"))  # adjust levels as needed
  )  

forest_p <-
  ggplot(forest_df, aes(x = Estimate, y = Group, color = Effect_short)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey10", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), 
                 height = 0.3, size = 1, 
                 position = position_dodge2(width = 0.3, reverse = TRUE)) +
  geom_point(aes(shape = Effect_short), size = 3, 
             position = position_dodge2(width = 0.3, reverse = TRUE)) +
  scale_color_manual(values = c("ATE" = col_ATE, "NDE" = col_NDE, "NIE" = col_NIE)) +
  scale_shape_manual(values = c("ATE" = 15, "NIE" = 16, "NDE" = 17)) +
  labs(
    #title = "Forest: NDE & NIE (95% CIs, rough)",
    x = "Effect on COVID-19 Job Loss",
    y = NULL, color = NULL, shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 24, hjust = 0),
    legend.position = "bottom",
    plot.margin = margin(t = 0, r = 20, b = 20, l = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) + 
  scale_x_continuous(
    limits = c(-0.30, 0.05), 
    breaks = seq(-0.30, 0.05, by = 0.05),
    labels = scales::label_number(accuracy = 0.01)  # Forces decimal format
  )  

forest_p

# ----------------------------
# 1) Forest plot for NDE & NIE
# ----------------------------
forest_df <- race_df %>% 
  filter(Effect %in% c("Direct (NDE)", "Indirect (NIE)")) %>%
  mutate(
    Effect_short = recode(Effect,
                          "Direct (NDE)"  = "NDE",
                          "Indirect (NIE)" = "NIE"),
    Effect_short = factor(Effect_short, levels = c("NDE", "NIE"))  # adjust levels as needed
  )  

forest_p <-
  ggplot(forest_df, aes(x = Estimate, y = Group, color = Effect_short)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey10", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), 
                 height = 0.3, size = 1, 
                 position = position_dodge2(width = 0.3, reverse = TRUE)) +
  geom_point(aes(shape = Effect_short), size = 3, 
             position = position_dodge2(width = 0.3, reverse = TRUE)) +
  scale_color_manual(values = c("NDE" = col_NDE, "NIE" = col_NIE)) +
  scale_shape_manual(values = c("NDE" = 16, "NIE" = 17)) +
  labs(
    #title = "Forest: NDE & NIE (95% CIs, rough)",
    x = "Effect on COVID-19 Job Loss",
    y = NULL, color = NULL, shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 24, hjust = 0),
    legend.position = "bottom",
    plot.margin = margin(t = 0, r = 20, b = 20, l = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) + 
  scale_x_continuous(
    limits = c(-0.30, 0.05), 
    breaks = seq(-0.30, 0.05, by = 0.05),
    labels = scales::label_number(accuracy = 0.01)  # Forces decimal format
  )  

forest_p
# ----------------------------
# 2) Stacked ATE decomposition + ATE CI
# ----------------------------
# Bars: take NDE and NIE; ATE line: from ATE rows
stack_df <- race_df %>%
  filter(Effect %in% c("Direct (NDE)", "Indirect (NIE)")) %>%
  mutate(Effect_short = recode(Effect, "Direct (NDE)" = "NDE", "Indirect (NIE)" = "NIE"))  

ate_df <- race_df %>%
  filter(Effect == "Total (ATE)") %>%
  select(Group, ATE = Estimate, ATE_lo = ci_lo, ATE_hi = ci_hi)  
ate_df$Group <- factor(ate_df$Group, levels = levels(stack_df$Group))


stacked_p <-
  ggplot(stack_df, aes(x = Estimate, y = Group, fill = Effect_short)) +
  # stacked horizontal bars
  geom_col(width = 0.7) +
  # overlay the ATE 95% CI as a horizontal line with caps
  geom_errorbarh(
    data = ate_df,
    aes(y = Group, xmin = ATE_lo, xmax = ATE_hi),
    height = 0.1, size = 0.5, color = "black",
    inherit.aes = FALSE, 
    alpha = 1
  ) +  
  #geom_point(data = ate_df, aes(x = ATE, y = Group),
  #                         size = 1.8, color = "black", inherit.aes = FALSE) +
  # Add horizontal bracket for each racial group
  geom_segment(
    data = ate_df,
    aes(x = ATE-0.005, xend = 0.005, 
        y = as.numeric(Group) + 0.4, yend = as.numeric(Group) + 0.4),
    color = "black", size = 0.5, inherit.aes = FALSE
  ) +
  # Left vertical line of bracket for each group
  geom_segment(
    data = ate_df,
    aes(x = ATE-0.005, xend = ATE-0.005,
        y = as.numeric(Group) + 0.35, yend = as.numeric(Group) + 0.4),
    color = "black", size = 0.5, inherit.aes = FALSE
  ) +
  # Right vertical line of bracket for each group
  geom_segment(
    data = ate_df,
    aes(x = +0.005, xend = +0.005,
        y = as.numeric(Group) + 0.35, yend = as.numeric(Group) + 0.4),
    color = "black", size = 0.5, inherit.aes = FALSE
  ) +
  # ATE label for each group
  geom_text(
    data = ate_df,
    aes(x = (ATE) / 2, y = as.numeric(Group) + 0.45, label = "ATE"),
    hjust = 0.5, vjust = 0, size = 3, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("NDE" = col_NDE, "NIE" = col_NIE)) +
  labs(
    title = "",
    x = "Effect on COVID-19 Job Loss",
    y = NULL, fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 24, hjust = 0),
    legend.position = "bottom",
    plot.margin = margin(t = 0, r = 20, b = 20, l = 20), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) + 
  scale_x_continuous(
    limits = c(-0.30, 0.05), 
    breaks = seq(-0.30, 0.05, by = 0.05),
    labels = scales::label_number(accuracy = 0.01)  # Forces decimal format
  )  

stacked_p


# ----------------------------
# 3) Arrange side-by-side with a single legend
# ----------------------------
# Extract a shared legend (from forest; either is fine)
 

# Create your plots without legends
forest_plot_clean <- forest_p + theme(legend.position = "none", axis.title.x = element_blank())
stacked_plot_clean <- stacked_p + theme(legend.position = "none", axis.title.x = element_blank())

# Get legend
leg <- cowplot::get_legend(
  stacked_p + theme(legend.position = "top")
)

# Combine the two plots
row_plots <- cowplot::plot_grid(
  forest_plot_clean,
  stacked_plot_clean,
  ncol = 2, align = "h"
)

# Add shared x-axis label
shared_x_label <- cowplot::ggdraw() + 
  cowplot::draw_label("Effect on COVID-19 Job Loss", 
                      x = 0.5, y = 0.5, 
                      #fontface = "bold", 
                      size = 12)

# Final arrangement with shared x-label
final_plot_by_race <- cowplot::plot_grid(
  row_plots, 
  #shared_x_label,
  leg,
  ncol = 1, 
  rel_heights = c(1, 
                  #0.05, 
                  0.08)
)

final_plot_by_race
 
# ----------------------------
# 4) Save (keeps margins)
# ----------------------------
 
ggsave(
  filename = "figure_mediation_effect_by_race.png",
  plot     = final_plot_by_race,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 8.5,
  height   = 5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_mediation_effect_by_race.png"), mustWork = FALSE),
  "\n"
)