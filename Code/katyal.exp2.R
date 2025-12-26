# ==============================================================================
# Analysis of inattention effects in Katyal et al. (2025) Experiment 2
# ==============================================================================

# Clear all
rm(list = ls())

# Load data --------------------------------------------------------------------
katyal_item_level_data <- read.csv("./Data/Katyal2025/katyal_item_level_data.csv")
item_weights <- read.csv("./Data/Katyal2025/Gillan_2016_item_weights.csv")

# Prepare data with individual questionnaire items
questionnaire_prefixes <- c("SDS", "LSAS", "STAI", "OCI", "BIS", "AUDIT", "EAT", "AES")

# Define reversed items from from Hopkins et al. (2022) 
reversed_items <- c(
  "SDS_11", "SDS_12", "SDS_14", "SDS_16", "SDS_17", "SDS_18", "SDS_20",
  "STAI_1", "STAI_3", "STAI_4", "STAI_5", "STAI_7", "STAI_10", 
  "STAI_13", "STAI_16", "STAI_19",
  "BIS_9", "BIS_13", "BIS_20",
  "AES_1", "AES_2", "AES_7", "AES_8", "AES_16", "AES_17", "AES_18"
)


# Compute skewness and confidence correlations ---------------------------------
compute_item_statistics <- function(questionnaire_prefix, data, conf_column) {
  # create a regex pattern to identify questionnaire item columns
  pattern <- paste0("^", questionnaire_prefix, "(_|\\.)\\d+$")
  # finds all matching column names in the dataset
  item_columns <- grep(pattern, names(data), value = TRUE)
  # loops over each item column and combine results into df 
  map_dfr(item_columns, function(col) {
    # extracts the values from the current item column
    item_values <- data[[col]]
    tibble(
      questionnaire = questionnaire_prefix,
      item = col,
      # compute skewness 
      skewness = moments::skewness(item_values, na.rm = TRUE),
      # compute correlation of item rating with confidence 
      conf_correlation = cor(item_values, data[[conf_column]], 
                             use = "complete.obs", method = "pearson")
    )
  })
}

# run fucntion for all questionnaires in questionnaire_prefixes
item_statistics <- map_dfr(
  questionnaire_prefixes, 
  compute_item_statistics, 
  data = katyal_item_level_data, 
  conf_column = "mean_conf"
) %>%
  mutate(
    reversed = item %in% reversed_items,
    questionnaire = factor(questionnaire)
  )

# Compute inattention sensitivity ----------------------------------------------
# make sure inattentive is the reference level 
katyal_item_level_data$attention_fail <- relevel(factor(katyal_item_level_data$attention_fail),
                                          ref = "inattentive")

compute_inattention_effect <- function(data, prefixes, attention_column) {
  pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")_\\d+$")
  item_columns <- grep(pattern, names(data), value = TRUE)
  
  map_dfr(item_columns, function(col) {
    effect <- effectsize::cohens_d(
      data[[col]] ~ data[[attention_column]],
      pooled_sd = TRUE,
      hedges.correction = FALSE,
      ci = 0.95
    )
    
    tibble(
      item = col,
      inattention_d = effect$Cohens_d,
      ci_low = effect$CI_low,
      ci_high = effect$CI_high
    )
  }) %>%
    arrange(desc(inattention_d))
}

inattention_effects <- compute_inattention_effect(
  katyal_item_level_data, 
  questionnaire_prefixes, 
  attention_column = "attention_fail"
)

# Combine all item-level metrics -----------------------------------------------

item_analysis <- item_statistics %>%
  left_join(inattention_effects, by = "item") %>%
  # change item names for merging with Gillan item weights 
  mutate(
    prefix = str_extract(item, "^[^_]+"),
    number = str_extract(item, "(?<=_)\\d+"),
    prefix = recode(prefix,
                    "OCI"   = "ocir",
                    "EAT"   = "eat",
                    "AES"   = "apathy",
                    "AUDIT" = "alcohol",
                    "SDS"   = "zung",
                    "STAI"  = "anxiety",
                    "BIS"   = "bis",
                    "LSAS"  = "leb",
                    .default = prefix
    ),
    item = paste0(prefix, ".", number)
  ) %>%
  select(-prefix, -number) %>%
  left_join(item_weights, by = "item")

# Visualizations ---------------------------------------------------------------
# Define plot aesthetics matching Sarna et al., 2025 conventions 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Katyal et al., 2025 Figures

# Define shared theme for all plots
shared_theme <- theme(
  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 18),
  axis.text = element_text(size = 14),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16),
  aspect.ratio = 1
)

# Define consistent shape mapping for questionnaires
questionnaire_shape_map <- c(
  "AUDIT" = 0,
  "LSAS" = 1,   
  "OCI" = 11,              
  "EAT" = 5,
  "SDS" = 18,        
  "BIS" = 20,    
  "AES" = 15,
  "STAI" = 17
)

# Define reversed colors
reversed_colors <- c("#1B9E77", "#D95F02")

### compute correlations --------------------------------------------------------

katyal_inattention_CIT_cor <- 
  cor.test(item_analysis$inattention_d,
           item_analysis$CIT,
           method = "spearman")

katyal_inattention_AD_cor <- 
  cor.test(item_analysis$inattention_d,
           item_analysis$AD,
           method = "spearman")

katyal_inattention_skewness_cor <- 
  cor.test(item_analysis$inattention_d,
           item_analysis$skewness,
           method = "spearman")


### create p-value labels 
katyal_inattention_CIT_label <- ifelse(
  katyal_inattention_CIT_cor$p.value < 0.001,
  paste0("italic(r) == ", signif(katyal_inattention_CIT_cor$estimate, 2),
         "*','~italic(p) < 0.001"),
  paste0("italic(r) == ", signif(katyal_inattention_CIT_cor$estimate, 2),
         "*','~italic(p) == ", signif(katyal_inattention_CIT_cor$p.value, 3))
)

katyal_inattention_AD_label <- ifelse(
  katyal_inattention_AD_cor$p.value < 0.001,
  paste0("italic(r) == ", signif(katyal_inattention_AD_cor$estimate, 2),
         "*','~italic(p) < 0.001"),
  paste0("italic(r) == ", signif(katyal_inattention_AD_cor$estimate, 2),
         "*','~italic(p) == ", signif(katyal_inattention_AD_cor$p.value, 3))
)

katyal_inattention_skewness_label <- ifelse(
  katyal_inattention_skewness_cor$p.value < 0.001,
  paste0("italic(r) == ", signif(katyal_inattention_skewness_cor$estimate, 2),
         "*','~italic(p) < 0.001"),
  paste0("italic(r) == ", signif(katyal_inattention_skewness_cor$estimate, 2),
         "*','~italic(p) == ", signif(katyal_inattention_skewness_cor$p.value, 3))
)


### PLOTS -----------------------------------------------------------------------
CIT_katyal_inattention_weights <- 
  item_analysis %>%
  mutate(
    questionnaire = factor(questionnaire, levels = names(questionnaire_shape_map))) %>%
  ggplot(aes(x = inattention_d, y = CIT,
             shape = questionnaire, color = reversed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  coord_cartesian(ylim = c(-0.03, 0.07)) +
  annotate(
    "text",
    x = -0.15,
    y = 0.065,
    label = katyal_inattention_CIT_label,
    parse = TRUE, size = 6
  ) +
  scale_shape_manual(values = questionnaire_shape_map, guide = "none") +
  scale_color_manual(values = reversed_colors, guide = "none") +
  labs(x = "Inattention Sensitivity (Cohen's d)",
       y = "Item-weight",
       title = "Katyal et al., 2025: CIT") +
  theme_minimal() + shared_theme +
  theme(legend.position = "none")


### AD plot --------------------------------------------------------------------

AD_katyal_inattention_weights <- 
  item_analysis %>%
  mutate(
    questionnaire = factor(questionnaire, levels = names(questionnaire_shape_map))) %>%
  ggplot(aes(x = inattention_d, y = AD,
             shape = questionnaire, color = reversed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  coord_cartesian(ylim = c(-0.03, 0.07)) +
  annotate(
    "text",
    x = -0.15,
    y = 0.065,
    label = katyal_inattention_AD_label,
    parse = TRUE, size = 6
  ) +
  scale_shape_manual(values = questionnaire_shape_map, guide = "none") +
  scale_color_manual(values = reversed_colors, guide = "none") +
  labs(x = "Inattention Sensitivity (Cohen's d)",
       y = "Item-weight",
       title = "Katyal et al., 2025: AD") +
  theme_minimal() + shared_theme +
  theme(legend.position = "none")


### Skewness plot --------------------------------------------------------------

katyal_inattention_skewness <- 
  item_analysis %>%
  mutate(
    questionnaire = factor(questionnaire, levels = names(questionnaire_shape_map))) %>%
  ggplot(aes(x = inattention_d, y = skewness,
             shape = questionnaire, color = reversed)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black") +
  annotate(
    "text",
    x = -0.15,
    y = max(item_analysis$skewness, na.rm = TRUE) *0.915,
    label = katyal_inattention_skewness_label,
    parse = TRUE, size = 6
  ) +
  scale_shape_manual(values = questionnaire_shape_map, name = "Questionnaire") +
  scale_color_manual(values = reversed_colors, name = NULL, labels = c("Standard", "Reversed")) +
  labs(x = "Inattention Sensitivity (Cohen's d)",
       y = "Item-skewness",
       title = "Katyal et al., 2025") +
  theme_minimal() + shared_theme

### SAVE FIGURE -----------------------------------------------------------------
katyal_inattention_figure <- CIT_katyal_inattention_weights | AD_katyal_inattention_weights | katyal_inattention_skewness

ggsave(filename = "./figures/katyal_inattention_figure.png", plot = katyal_inattention_figure, 
      device = "png", width = 16, height = 6, dpi = 300)
