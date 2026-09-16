library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# ----------------------------
# 0) Data prep
# ----------------------------
 
forest_df <- covid_jobloss_results$main_results %>% 
  mutate(
    Group = dplyr::recode(Group,
                          "Light_Black" = "Light Black",
                          "Dark_Black"  = "Dark Black",
                          .default = Group),
    Group = factor(Group, levels = c("White", "Black", "Light Black",  "Dark Black")),
    ci_lo = Estimate - 1.96 * SE,
    ci_hi = Estimate + 1.96 * SE
  ) %>% 
  arrange(Group)
 
# Color palette (match your figure)
col_ATE <- "black"
col_NDE <- "#F39C12"  # orange
col_NIE <- "#2E86C1"  # blue

# -----------------------------------
# 1) Forest plot for ATE & NIE & NDE
# -----------------------------------

forest_df

forest_df <- forest_df %>%  
  mutate(
    Effect_short = recode(Effect,
                          "Total (ATE)" = "ATE", 
                          "Direct (NDE)"  = "NDE",
                          "Indirect (NIE)" = "NIE"),
    Effect_short = factor(Effect_short, levels = c("NDE", "NIE", "ATE"))  # adjust levels as needed
  )  

 
forest_p <-
  ggplot(forest_df, aes(x = Estimate, y = Effect_short, color = Effect_short)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey10", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), 
                 height = 0.3, size = 1, 
                 position = position_dodge2(width = 0.3, reverse = TRUE)) +
  geom_point(aes(shape = Effect_short), size = 3, 
             position = position_dodge2(width = 0.3, reverse = TRUE)) +
  scale_color_manual(
    values = c("ATE" = col_ATE, "NDE" = col_NDE, "NIE" = col_NIE),
    breaks = c("ATE", "NIE", "NDE")
  ) +
  scale_shape_manual(
    values = c("ATE" = 15, "NIE" = 16, "NDE" = 17),
    breaks = c("ATE", "NIE", "NDE")
  ) + 
  labs(
    #title = "Forest: NDE & NIE (95% CIs, rough)",
    x = "Effect on COVID-19 Job Loss",
    y = NULL, color = NULL, shape = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 24, hjust = 0),
    legend.position = "none",
    plot.margin = margin(t = 0, r = 20, b = 20, l = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12) 
  ) + 
  scale_x_continuous(
    limits = c(-0.30, 0.05), 
    breaks = seq(-0.30, 0.05, by = 0.1),
    labels = scales::label_number(accuracy = 0.01)  # Forces decimal format
  ) +
  facet_wrap(~ Group, nrow = 1)  


forest_p

# ----------------------------
# 4) Save (keeps margins)
# ----------------------------

ggsave(
  filename = "figure_mediation_effect.png",
  plot     = forest_p,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 9,
  height   = 5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_mediation_effect.png"), mustWork = FALSE),
  "\n"
)




# ----------------------------
# 2) Stacked ATE decomposition + ATE CI
# ----------------------------
# Bars: take NDE and NIE; ATE line: from ATE rows
stack_df <- forest_df %>%
  filter(Effect %in% c("Direct (NDE)", "Indirect (NIE)")) %>%
  mutate(Effect_short = recode(Effect, "Direct (NDE)" = "Proportion of Natural Direct Effect", "Indirect (NIE)" = "Proportion of Natural Indirect Effect")) %>%  
  mutate(
    Effect_short = recode(Effect, 
                          "Direct (NDE)"  = "Proportion of Natural Direct Effect",
                          "Indirect (NIE)" = "Proportion of Natural Indirect Effect"),
    Effect_short = factor(Effect_short, levels = c("Proportion of Natural Direct Effect", "Proportion of Natural Indirect Effect"))  # adjust levels as needed
  )  

ate_df <- forest_df %>%
  filter(Effect == "Total (ATE)") %>%
  select(Group, ATE = Estimate, ATE_lo = ci_lo, ATE_hi = ci_hi)

ate_df$Group <- factor(ate_df$Group, levels = levels(stack_df$Group))

 

stacked_p <-
  ggplot(stack_df, aes(x = Group, y = Estimate, fill = Effect_short)) +
  geom_col(width = 0.7)  + 
  scale_fill_manual(values = c("Proportion of Natural Direct Effect" = col_NDE, 
                               "Proportion of Natural Indirect Effect" = col_NIE), 
                    breaks = c("Proportion of Natural Indirect Effect", "Proportion of Natural Direct Effect")) +
  labs(x = NULL, y = "Total Effect of College on COVID-19 Job Loss", fill = NULL) +
  theme_bw(base_size = 12) +
  theme( 
    legend.position = "bottom",
    axis.text.x = element_text(hjust = 0.5, size = 12)
  ) +
  scale_y_continuous(
    limits = c(-0.20, 0.05),
    breaks = seq(-0.20, 0.05, by = 0.05),
    labels = scales::label_number(accuracy = 0.01)
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") 


stacked_p
# ----------------------------
# 4) Save (keeps margins)
# ----------------------------

ggsave(
  filename = "figure_mediation_effect_prop.png",
  plot     = stacked_p,
  path     = file.path(DIR_RESULTS, "figure"),  
  width    = 8,
  height   = 5,
  units    = "in",
  dpi      = 300
)

# Optional: confirm where it went
cat(
  "Saved to:",
  normalizePath(file.path(DIR_RESULTS, "figure", "figure_mediation_effect_prop.png"), mustWork = FALSE),
  "\n"
)
