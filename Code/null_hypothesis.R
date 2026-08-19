library(ggplot2)
library(dplyr)
library(patchwork)

katyal_item_analysis <- read.csv("./Data/katyal_item_analysis.csv")

# ggplot aes 
questionnaire_shape_map <- c(
  "alcohol" = 0, "social_anxiety" = 1, "ocd" = 11, "eat" = 5,
  "depression" = 18, "impulsivity" = 20, "apathy" = 15,
  "anxiety" = 17, "gad_anxiety" = 2)
reversed_colors <- c("#1B9E77", "#D95F02")

shared_theme <- theme(
  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 18),
  axis.text = element_text(size = 14),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16),
  aspect.ratio = 1)

# CIT correlation + annotation label
katyal_inattention_CIT_cor <-
  cor.test(katyal_item_analysis$inattention_d,
           katyal_item_analysis$CIT, method = "spearman")

katyal_inattention_CIT_label <- ifelse(
  katyal_inattention_CIT_cor$p.value < 0.001,
  paste0("italic(r) == ", signif(katyal_inattention_CIT_cor$estimate, 2),
         "*','~italic(p) < 0.001"),
  paste0("italic(r) == ", signif(katyal_inattention_CIT_cor$estimate, 2),
         "*','~italic(p) == ", signif(katyal_inattention_CIT_cor$p.value, 3)))

# CIT scatter 
CIT_katyal_inattention_weights <-
  katyal_item_analysis %>%
  mutate(
    questionnaire_mapped = recode(questionnaire,
                                  "AUDIT" = "alcohol", "LSAS" = "social_anxiety", "OCI" = "ocd",
                                  "EAT" = "eat", "SDS" = "depression", "BIS" = "impulsivity",
                                  "AES" = "apathy", "STAI" = "anxiety"),
    questionnaire_mapped = factor(questionnaire_mapped,
                                  levels = names(questionnaire_shape_map))) %>%
  ggplot(aes(x = inattention_d, y = CIT,
             shape = questionnaire_mapped, color = reversed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  coord_cartesian(ylim = c(-0.03, 0.07)) +
  annotate("text", x = -0.15, y = 0.065,
           label = katyal_inattention_CIT_label, parse = TRUE, size = 6) +
  scale_shape_manual(values = questionnaire_shape_map, name = "Questionnaire") +
  scale_color_manual(values = reversed_colors,
                     labels = c("Standard", "Reversed"), name = "") +
  labs(x = "Inattention Sensitivity (Cohen's d)",
       y = "Item-weight", title = "Katyal et al., 2025: CIT", tag = "B") +
  theme_minimal() + shared_theme

# Mean-weight panel (narrow)
katyal_CIT_mean_fig <-
  katyal_item_analysis %>%
  ggplot(aes(x = 1, y = CIT, color = reversed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, size = 1.2, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", size = 6, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = reversed_colors, labels = c("Standard", "Reversed"), name = "") +
  theme_minimal() +
  coord_cartesian(ylim = c(-0.03, 0.07)) +
  labs(title = "Mean\nweight") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 0),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Katyal composite (scatter + narrow mean + legend)
katyal_CIT_panel <-
  (CIT_katyal_inattention_weights | katyal_CIT_mean_fig) +
  plot_layout(widths = c(1, 0.2), guides = "collect") &
  theme(legend.position = "right")

# PIP plot 
pip_df <- read.csv("./Data/pip_df.csv")
summary_cbt <-
  ggplot(pip_df, aes(x = Freq, y = mean, group = measure)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_text(aes(y = mean + se, label = paste0("n=", n)),
            vjust = -0.8, size = 5) +
  facet_wrap(~measure, scales = "free_y",
             labeller = labeller(measure = function(x) paste0("PIP: ", x))) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  theme_minimal() +
  labs(x = "Number of infrequency item failures (Freq)",
       y = "Score (± SE)", tag = "A") +
  shared_theme +
  theme(strip.text = element_text(size = 18, face = "bold"))

# Two-panel figure: A = PIP (top), B = Katyal (bottom)
figure <-
  (summary_cbt / katyal_CIT_panel) +
  plot_layout(heights = c(0.9, 1)) &
  theme(plot.tag = element_text(size = 20, face = "bold"))

#ggsave("./figures/pip_katyal_panel.png", figure,
#      width = 12, height = 12, dpi = 300)
