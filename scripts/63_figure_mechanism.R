# scripts/66_figure_mechanism.R


# load results 
covid_jobloss_results <- readRDS(file.path(DIR_RESULTS, "covid_jobloss_results.rds"))



# mechanism plot data (1): negative direct 
df_ps  <- covid_jobloss_results$mechanism_results %>% filter(Effect == "PS") %>%
  transmute(Group, 
         PS = Estimate, 
         PS_lo = Estimate - 1.96 * SE,
         PS_hi = Estimate + 1.96 * SE)

df_ate <- covid_jobloss_results$main_results %>% filter(Effect == "Total (ATE)") %>%
  transmute(Group,
            ATE = Estimate,
            ATE_lo = Estimate - 1.96 * SE,
            ATE_hi = Estimate + 1.96 * SE)
 

ate_plot_df <- df_ate %>%
  inner_join(df_ps, by = "Group") %>%
  mutate(
    Group_label = dplyr::recode(Group,
                                "Light_Black" = "Light Black",
                                "Dark_Black"  = "Dark Black",
                                .default = Group)
  ) %>% 
  mutate(
    Group_label = factor(Group_label, levels = c("White", "Black", "Light Black", "Dark Black"))
  ) %>%
  mutate(
    facet = case_when(
      Group %in% c("White", "Black") ~ "Race",
      Group %in% c("Light_Black", "Dark_Black") ~ "Skin Color (Black Workers Only)"
    ),
    x = recode(Group,
               "White"       = "White",
               "Black"       = "Black",
               "Light_Black" = "Light",
               "Dark_Black"  = "Dark"),
    x = factor(x, levels = c("White","Black","Light","Dark")),
    facet = factor(facet, levels = c("Race","Skin Color (Black Workers Only)"))
  )


ate_plot_df
# Define a color-blind friendly palette (e.g., from RColorBrewer's "Dark2" or "Set2")
cb_palette <- brewer.pal(n = 4, name = "Dark2") # Or "Set2" for lighter colors


# Colors to match the example figure
common_cols <- c(
  "White"       = cb_palette[1], 
  "Black"       = cb_palette[2],
  "Light_Black" = cb_palette[3],
  "Dark_Black"  = cb_palette[4]
)
common_breaks  <- names(common_cols)
common_labels  <- c("White", "Black", "Light Black", "Dark Black")
 
# library(ggrepel) # uncomment if you want labels

total_p <- ggplot(ate_plot_df, aes(x = PS, y = ATE, color = Group)) +
  # reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
  
  # 95% CI for PS (horizontal) — use geom_segment for broad compatibility
  geom_errorbarh(aes(xmin = PS_lo, xmax = PS_hi), height = 0.003, linewidth = 0.6, alpha = 0.8) + 

  
  # 95% CI for ATE (vertical)
  geom_errorbar(aes(ymin = ATE_lo, ymax = ATE_hi), width = 0.006, linewidth = 0.7, alpha = 0.9) + 

  # point
  geom_point(size = 1.6) +
  
  # optional: add clean labels next to points
  ggrepel::geom_text_repel(aes(label = Group_label), size = 3, max.overlaps = 20,
                           show.legend = FALSE, seed = 123, box.padding = 0.2, point.padding = 0.15, 
                           nudge_x = -0.06, nudge_y = -0.005) +
  
  labs(
    x = "Propensity Score of College Completion (PS, 95% CI)",
    y = "Total Effects (ATE, 95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    panel.spacing.x = unit(12, "pt")
  ) +
  
  # y scale
  scale_y_continuous(limits = c(-0.30, 0),
                     breaks = seq(-0.30, 0, by = 0.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # x scale (PS as percent for readability)
  scale_x_continuous(limits = c(0.0, 0.40),
                     breaks = seq(0.0, 0.40, by = 0.10),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # colors (yours)
  scale_color_manual(values = common_cols,
                     breaks = common_breaks,
                     labels = common_labels,
                     drop   = FALSE) +
  
  # clean up unused guides
  guides(fill = "none", shape = "none", linetype = "none", alpha = "none") +
  
  # facets: Race | Skin tone (Black only)
  facet_wrap(~ facet, scales = "free_x", nrow = 1)  

 
# mechanism plot (2): direct

# mechanism plot data (3): indirect



df_nde <- covid_jobloss_results$main_results %>% filter(Effect == "Direct (NDE)") %>%
  transmute(Group,
            NDE = Estimate,
            NDE_lo = Estimate - 1.96 * SE,
            NDE_hi = Estimate + 1.96 * SE)



nde_plot_df <- df_nde %>%
  inner_join(df_ps, by = "Group") %>%
  mutate(
    Group_label = dplyr::recode(Group,
                                "Light_Black" = "Light Black",
                                "Dark_Black"  = "Dark Black",
                                .default = Group)
  ) %>% 
  mutate(
    Group_label = factor(Group_label, levels = c("White", "Black", "Light Black", "Dark Black"))
  ) %>%
  mutate(
    facet = case_when(
      Group %in% c("White", "Black") ~ "Race",
      Group %in% c("Light_Black", "Dark_Black") ~ "Skin Color (Black Workers Only)"
    ),
    x = recode(Group,
               "White"       = "White",
               "Black"       = "Black",
               "Light_Black" = "Light",
               "Dark_Black"  = "Dark"),
    x = factor(x, levels = c("White","Black","Light","Dark")),
    facet = factor(facet, levels = c("Race","Skin Color (Black Workers Only)"))
  )


 
# library(ggrepel) # uncomment if you want labels

direct_p <- ggplot(nde_plot_df, aes(x = PS, y = NDE, color = Group)) +
  # reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
   
  geom_errorbarh(aes(xmin = PS_lo, xmax = PS_hi), height = 0.0025, linewidth = 0.6, alpha = 0.8) + 
  
  # 95% CI for ATE (vertical)
  geom_errorbar(aes(ymin = NDE_lo, ymax = NDE_hi), width = 0.006, linewidth = 0.7, alpha = 0.9) + 
  
  # point
  geom_point(size = 1.6) +
  
  # optional: add clean labels next to points
  ggrepel::geom_text_repel(aes(label = Group_label), size = 3, max.overlaps = 20,
                           show.legend = FALSE, seed = 123, box.padding = 0.2, point.padding = 0.15, 
                           nudge_x = -0.06, nudge_y = -0.005) +
  
  labs(
    x = "Propensity Score of College Completion (PS, 95% CI)",
    y = "Direct Effects (NDE, 95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    panel.spacing.x = unit(12, "pt")
  ) +
  
  # y scale
  scale_y_continuous(limits = c(-0.2, 0.0),
                     breaks = seq(-0.2, 0.0, by = 0.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # x scale (PS as percent for readability)
  scale_x_continuous(limits = c(0.0, 0.40),
                     breaks = seq(0.0, 0.40, by = 0.10),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # colors (yours)
  scale_color_manual(values = common_cols,
                     breaks = common_breaks,
                     labels = common_labels,
                     drop   = FALSE) +
  
  # clean up unused guides
  guides(fill = "none", shape = "none", linetype = "none", alpha = "none") +
  
  # facets: Race | Skin tone (Black only)
  facet_wrap(~ facet, scales = "free_x", nrow = 1)

direct_p


# mechanism plot data (3): indirect



df_nie <- covid_jobloss_results$main_results %>% filter(Effect == "Indirect (NIE)") %>%
  transmute(Group,
            NIE = Estimate,
            NIE_lo = Estimate - 1.96 * SE,
            NIE_hi = Estimate + 1.96 * SE)
 


nie_plot_df <- df_nie %>%
  inner_join(df_ps, by = "Group") %>%
  mutate(
    Group_label = dplyr::recode(Group,
                                "Light_Black" = "Light Black",
                                "Dark_Black"  = "Dark Black",
                                .default = Group)
  ) %>% 
  mutate(
    Group_label = factor(Group_label, levels = c("White", "Black", "Light Black", "Dark Black"))
  ) %>%
  mutate(
    facet = case_when(
      Group %in% c("White", "Black") ~ "Race",
      Group %in% c("Light_Black", "Dark_Black") ~ "Skin Color (Black Workers Only)"
    ),
    x = recode(Group,
               "White"       = "White",
               "Black"       = "Black",
               "Light_Black" = "Light",
               "Dark_Black"  = "Dark"),
    x = factor(x, levels = c("White","Black","Light","Dark")),
    facet = factor(facet, levels = c("Race","Skin Color (Black Workers Only)"))
  )


 

# library(ggrepel) # uncomment if you want labels

indirect_p <- ggplot(nie_plot_df, aes(x = PS, y = NIE, color = Group)) +
  # reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.6) +
   
  geom_errorbarh(aes(xmin = PS_lo, xmax = PS_hi), height = 0.002, linewidth = 0.6, alpha = 0.8) + 
  
  # 95% CI for ATE (vertical)
  geom_errorbar(aes(ymin = NIE_lo, ymax = NIE_hi), width = 0.006, linewidth = 0.7, alpha = 0.9) + 
  
  # point
  geom_point(size = 1.6) +
  
  # optional: add clean labels next to points
  ggrepel::geom_text_repel(aes(label = Group_label), size = 3, max.overlaps = 20,
                           show.legend = FALSE, seed = 123, box.padding = 0.2, point.padding = 0.15, 
                           nudge_x = -0.06, nudge_y = -0.005) +
  
  labs(
    x = "Propensity Score of College Completion (PS, 95% CI)",
    y = "Indirect Effects (NIE, 95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
    panel.spacing.x = unit(12, "pt")
  ) +
  
  # y scale
  scale_y_continuous(limits = c(-0.15, 0.05),
                     breaks = seq(-0.15, 0.05, by = 0.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # x scale (PS as percent for readability)
  scale_x_continuous(limits = c(0.0, 0.40),
                     breaks = seq(0.0, 0.40, by = 0.10),
                     labels = scales::number_format(accuracy = 0.01)) +
  
  # colors (yours)
  scale_color_manual(values = common_cols,
                     breaks = common_breaks,
                     labels = common_labels,
                     drop   = FALSE) +
  
  # clean up unused guides
  guides(fill = "none", shape = "none", linetype = "none", alpha = "none") +
  
  # facets: Race | Skin tone (Black only)
  facet_wrap(~ facet, scales = "free_x", nrow = 1)

indirect_p


### save figures 

ggsave(
  filename = "figure_total_mechanism.png",
  plot     = total_p,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_total_mechanism.png"), mustWork = FALSE),
  "\n"
)

ggsave(
  filename = "figure_direct_mechanism.png",
  plot     = direct_p,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_direct_mechanism.png"), mustWork = FALSE),
  "\n"
)

ggsave(
  filename = "figure_indirect_mechanism.png",
  plot     = indirect_p,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 7,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_indirect_mechanism.png"), mustWork = FALSE),
  "\n"
)