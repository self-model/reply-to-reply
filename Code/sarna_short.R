
# Clear all
rm(list = ls())

# Function to reverse the scores #########################################################
reverse_scores <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  return(max_val + min_val - x)
}

##################################################
# Load in processed data ---------------------------------------------------------------
##################################################
# Task data
raw_df <- read.csv("Data/Sarna2025/raw_df_cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(subj_id = as.character(subj_id))

tidy_df <- raw_df %>%
  filter(trial_type == "countDots") %>%
  select(subj_id, trial_index, block, RT, correct, confidence, confidence_RT, increment, right_array, left_array, response)

# Qualtrics data
qualtrics_df_raw <- read.csv("Data/Sarna2025/qualtrics_df_cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(subj_id = as.character(subj_id))

qualtrics_df <- qualtrics_df_raw %>% filter(subj_id != "test")

# Mean accuracy and attention exclusion -------------------------------------------------
acc_df <- tidy_df %>%
  filter(!block %in% c("practice_with_feedback", "practice_without_feedback")) %>%
  group_by(subj_id) %>%
  mutate(accuracy = ifelse(correct == "true", 1, 0)) %>%
  summarise(mean_acc = mean(as.numeric(accuracy)))

excluded <- acc_df %>% filter(mean_acc < 0.6) %>% pull(subj_id)

# Filter out excluded participants ------------------------------------------------------
tidy_df <- tidy_df %>% filter(!subj_id %in% excluded)
qualtrics_df <- qualtrics_df %>% filter(!subj_id %in% excluded)

##################################################
# Process Qualtrics data ---------------------------------------------------------------
##################################################

item_instructions <- data.frame(
  item_content = sub(".* - ", "", as.character(qualtrics_df_raw[1, ])),
  item_name = colnames(qualtrics_df_raw)
) %>%
  filter(str_detect(item_name, "OCI.R_") |
           str_detect(item_name, "DASS_anx_") |
           str_detect(item_name, "zung_") |
           str_detect(item_name, "neutral_")) %>%
  filter(!str_detect(item_name, "Click|DO|Page.Submit"))

self_long <- qualtrics_df %>%
  select(subj_id, matches("OCI.R_"), matches("DASS_anx_"), matches("zung_"), matches("neutral_")) %>%
  select(-contains("Click")) %>%
  select(subj_id, all_of(item_instructions$item_name)) %>%
  pivot_longer(cols = -subj_id, names_to = "item_name", values_to = "item_value") %>%
  left_join(item_instructions, by = "item_name") %>%
  filter(!subj_id %in% excluded)

##################################################
# Attention checks ---------------------------------------------------------------------
##################################################

inf_df <- self_long %>%
  filter(str_detect(item_name, "inf")) %>%
  mutate(attention_check_failed = case_when(
    item_name == "OCI.R_inf_1" & item_value != "0" ~ TRUE,
    item_name == "OCI.R_inf_2" & item_value != "0" ~ TRUE,
    item_name == "zung_inf_1" & !(item_value %in% c("4", "3")) ~ TRUE,
    item_name == "zung_inf_2" & item_value != "1" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(fail_indicator = as.integer(attention_check_failed))

# create df with failure indicators for each infrequency item
item_failures <- inf_df %>%
  select(subj_id, item_name, fail_indicator) %>%
  pivot_wider(names_from = item_name, values_from = fail_indicator, values_fill = 0) %>%
  rowwise() %>%
  mutate(total_fail = sum(c_across(-subj_id)))

# get IDs of participants who failed any attention check
failed_attention_id <- item_failures %>%
  filter(total_fail > 0) %>%
  pull(subj_id)

##################################################
# Acquiescence scores ------------------------------------------------------------------
##################################################
neutral_items <- paste0("neutral_", 1:14)

#copmute acquiescence scores as the mean across all neutral items 
rating_bias_df <- self_long %>%
  filter(item_name %in% neutral_items) %>%
  group_by(subj_id) %>%
  summarise(mean_rating = mean(as.numeric(item_value), na.rm = TRUE))

##################################################
# Participant-level data ------------------------------------------------------------
##################################################
participants_level_df <- tidy_df %>%
  #remove practice blocks 
  filter(!block %in% c("practice_with_feedback", "practice_without_feedback")) %>%
  group_by(subj_id) %>%
  summarise(mean_conf = mean(as.numeric(confidence))) %>%
  mutate(
    subj_id = as.character(subj_id),
    failed_attention = ifelse(subj_id %in% failed_attention_id, "Inattentive", "Attentive")
  ) %>%
  left_join(acc_df, by = "subj_id") %>%
  mutate(failed_attention = factor(failed_attention, levels = c("Attentive", "Inattentive"))) %>%
  left_join(rating_bias_df, by = "subj_id")

##################################################
# OCI-R processing ---------------------------------------------------------------------
# Reproduce error: 
#1.inclusion of all participants (attentive and inattentive)  
##################################################
oci_items_df <- self_long %>%
  filter(str_starts(item_name, "OCI.R")) %>%
  filter(!item_name %in% c("OCI.R_inf_1", "OCI.R_inf_2")) %>%
  mutate(item_value = as.numeric(item_value)) %>%
  left_join(rating_bias_df, by = "subj_id")

# Error #1: inclusion of all participants (attentive and inattentive)
oci_corrected <- list()
for (oci_item in unique(oci_items_df$item_name)) {
  item_data <- oci_items_df %>% filter(item_name == oci_item)
  model <- lm(item_value ~ mean_rating, data = item_data)
  item_data$item_corrected <- residuals(model)
  oci_corrected[[oci_item]] <- item_data
}

oci_items_df <- bind_rows(oci_corrected)

oci_df_corrected <- oci_items_df %>%
  group_by(subj_id) %>%
  summarise(total_oci = sum(item_value), total_oci_corrected = sum(item_corrected)) %>%
  mutate(total_oci_corrected_scaled = (total_oci_corrected - min(total_oci_corrected)) /
           (max(total_oci_corrected) - min(total_oci_corrected)) * max(total_oci))

##################################################
# SDS processing -----------------------------------------------------------------------
## Reproduce errors: 
# 1.inclusion of all participants (attentive and inattentive)  
# 2.positional mismatch  
##################################################
sds_items_df <- self_long %>%
  filter(str_starts(item_name, "zung_")) %>%
  filter(!item_name %in% c("zung_inf_1", "zung_inf_2")) %>%
  mutate(item_value = as.numeric(item_value))

sds_reversed_items <- c("zung_2","zung_5","zung_6","zung_11","zung_12","zung_14","zung_16","zung_17","zung_18","zung_20")

# reverse score 
sds_items_df <- sds_items_df %>%
  mutate(item_original_value = item_value,
         item_value = ifelse(item_name %in% sds_reversed_items,
                             reverse_scores(item_value),
                             item_value),
         reversed = ifelse(item_name %in% sds_reversed_items, "reversed", "standard")) %>%
  left_join(rating_bias_df, by = "subj_id")

# SDS correction 
# Error #1: inclusion of all participants (attentive and inattentive)
sds_corrected <- list()
for (sds_item in unique(sds_items_df$item_name)) {
  item_data <- sds_items_df %>% filter(item_name == sds_item)
  model <- lm(item_value ~ mean_rating, data = item_data)
  item_data$item_corrected <- residuals(model)
  sds_corrected[[sds_item]] <- item_data
}
# Error #2: positional mismatch
sds_items_df$item_corrected <- bind_rows(sds_corrected)$item_corrected  

sds_df_corrected <- sds_items_df %>%
  group_by(subj_id) %>%
  summarise(total_sds = sum(item_value),
            total_sds_corrected = sum(item_corrected),
            total_sds_rating_space = sum(item_original_value)) %>%
  mutate(total_sds_corrected_scaled = (total_sds_corrected - min(total_sds_corrected)) /
           (max(total_sds_corrected) - min(total_sds_corrected)) * max(total_sds))

##################################################
# Combine data and correlations --------------------------------------------------------
##################################################
participants_level_df_repro_errors <- participants_level_df %>%
  left_join(oci_df_corrected, by = "subj_id") %>%
  left_join(sds_df_corrected, by = "subj_id")

# add sanity checks to make sure we get the same values as Gillan et al. 2025 
###################################################
# Corrected version ###############################
##################################################

#OCI-R: residualize by item (Attentive-only) --------
oci_items_df_att <- self_long %>%
  filter(!(subj_id %in% failed_attention_id), str_starts(item_name, "OCI.R"),
         !item_name %in% c("OCI.R_inf_1", "OCI.R_inf_2")) %>%
  mutate(item_value = as.numeric(item_value)) %>%
  left_join(rating_bias_df, by = "subj_id")

oci_items_df_att <- oci_items_df_att %>%
  group_by(item_name) %>%
  mutate(item_corrected = resid(lm(item_value ~ mean_rating))) %>%
  ungroup()

oci_correct_totals_att <- oci_items_df_att %>%
  group_by(subj_id) %>%
  summarise(total_oci = sum(item_value, na.rm = TRUE),
            total_oci_corrected = sum(item_corrected, na.rm = TRUE)) %>%
  mutate(total_oci_corrected_scaled = (total_oci_corrected - min(total_oci_corrected, na.rm = TRUE)) /
           (max(total_oci_corrected, na.rm = TRUE) - min(total_oci_corrected, na.rm = TRUE)) *
           max(total_oci, na.rm = TRUE))

# SDS: reverse-score, residualize by item (Attentive-only; no positional mismatch) ---------
sds_items_df_att <- self_long %>%
  filter(!(subj_id %in% failed_attention_id), str_starts(item_name, "zung_"),
         !item_name %in% c("zung_inf_1", "zung_inf_2")) %>%
  mutate(item_value = as.numeric(item_value),
         item_value = ifelse(item_name %in% sds_reversed_items, reverse_scores(item_value), item_value)) %>%
  left_join(rating_bias_df, by = "subj_id")

sds_items_df_att <- sds_items_df_att %>%
  group_by(item_name) %>%
  mutate(item_corrected = resid(lm(item_value ~ mean_rating))) %>%
  ungroup()

sds_correct_totals_att <- sds_items_df_att %>%
  group_by(subj_id) %>%
  summarise(total_sds = sum(item_value, na.rm = TRUE),
            total_sds_corrected = sum(item_corrected, na.rm = TRUE)) %>%
  mutate(total_sds_corrected_scaled = (total_sds_corrected - min(total_sds_corrected, na.rm = TRUE)) /
           (max(total_sds_corrected, na.rm = TRUE) - min(total_sds_corrected, na.rm = TRUE)) *
           max(total_sds, na.rm = TRUE))

# Combine data (Attentive-only) ---------------------------
participants_level_df_att <- participants_level_df %>%
  filter(!(subj_id %in% failed_attention_id)) %>%
  select(subj_id, mean_conf) %>%
  left_join(oci_correct_totals_att, by = "subj_id") %>%
  left_join(sds_correct_totals_att, by = "subj_id")

# sanity check compare to value of Gillan et al. 2025 