# re-analysis of Sarna et al., 
# we use the foundational code provided by Sarna et al but we
# 1. correct an error with the SDS scale assignment to participants
# 3. reorder the analysis so that acquiescence is controlled for AFTER inattentive participants have been removed.
# show the impact of adding covariates for OCD/depression, sex and age

# update from Sarna et al., 2025 reply-to-reply 
# I'm making the following changes to the code:
# 1. Comment out this package library(Hmisc) as it causes conflicts when running on an Apple machine. 
#The removal of this package does not affect the output. 
# 2. Changing files path to "Data/.. to match the folder structure used in this repository.
# 3. I deleted all lines following line 408 (in the original script) as they are not relevant for the purpose of the analysis performed here.  
##################################################
# Load ggplot2 package
library(ggplot2)
# library(Hmisc) 
library(dplyr)
library(gridExtra)
library(tidyverse)
library(effsize)

# Clear all
rm(list = ls())


# Function to reverse the scores #########################################################
reverse_scores <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  return(max_val + min_val - x)
}

# Normalize scores to a scale ranging from 0 to 1 ########################################
# normalize_scores <- function(x) {
#   (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }


##################################################
# Load in their processed data ---------------------------------------------------------

# Task data
raw_df <- read.csv("Data/Sarna2025/raw_df_cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(subj_id = as.character(subj_id))

## pick out subset cols of task data
tidy_df <- raw_df %>% filter(trial_type=='countDots') %>% select(subj_id, trial_index, block, RT, correct, confidence, confidence_RT, increment, right_array, left_array, response)

# Qualtrics data
qualtrics_df_raw <- read.csv("Data/sarna2025/qualtrics_df_cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(subj_id = as.character(subj_id))

qualtrics_df <- qualtrics_df_raw %>% filter(subj_id != 'test')
length(unique(qualtrics_df$subj_id))


# Prolific demographic data
demographic_df <- read.csv("Data/Sarna2025/demographic_df_cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(subj_id = as.character(subj_id))
demo_df<- demographic_df[,c("subj_id","Age","Sex")]

# mean accuracy and confidence for correct/incorrect responses 
acc_df <-
  tidy_df %>%
  filter(block != 'practice_with_feedback' & block != 'practice_without_feedback') %>%
  group_by(subj_id) %>%
  mutate(accuracy = ifelse(correct=='true', 1,0)) %>%
  summarise(mean_acc = mean(as.numeric(accuracy)))

excluded_for_accuracy <- acc_df %>% filter(mean_acc<0.6) %>% dplyr::pull(subj_id)
excluded <- c(excluded_for_accuracy)

##################################################
# Data left after accuracy exclusions ---------------------------------------------------------

tidy_df <- tidy_df %>% filter(!(subj_id %in% excluded))
qualtrics_df <- qualtrics_df %>% filter(!(subj_id %in% excluded))
demo_df <- demo_df %>% filter(!(subj_id %in% excluded))

# N after exclusion - should be N=190
length(unique(tidy_df$subj_id))
length(unique(qualtrics_df$subj_id))
length(unique(demo_df$subj_id))

##################################################
# construct mean confidence per participant df ---------------------------------------------------------

mean_conf_df <- tidy_df %>% 
  filter(block != 'practice_with_feedback' & block != 'practice_without_feedback') %>% 
  group_by(subj_id) %>% 
  summarise(mean_conf = mean(as.numeric(confidence)))

##################################################
# Process qualtrics data  ---------------------------------------------------------

# Extract the first row as content descriptions and clean the content descriptions by removing the instruction part
#cleaned_content_descriptions <- sub(".* - ", "", as.character(raw_prolific_data[1, ]))

# Item names and instructions - requires raw qualtric file
item_instructions <- data.frame(
  item_content = sub(".* - ", "", as.character(qualtrics_df_raw[1, ])), # Extract the first row as content descriptions and clean the content descriptions by removing the instruction part
  item_name = colnames(qualtrics_df_raw)
) %>%
  filter(str_detect(item_name, "OCI.R_") | 
           str_detect(item_name, "DASS_anx_") | 
           str_detect(item_name, "zung_") | 
           str_detect(item_name, "neutral_")) %>%
  filter(!str_detect(item_name, "Click")) %>%
  filter(!str_detect(item_name, "DO")) %>%
  filter(!str_detect(item_name, "Page.Submit"))

self_long <- 
  qualtrics_df %>% 
  select(subj_id, matches("OCI.R_"), matches("DASS_anx_"), matches("zung_"), matches("neutral_")) %>%
  select(-contains("Click")) %>% # Self report columns with randomization and RT for block 
  select(subj_id, all_of(item_instructions$item_name)) %>%
  pivot_longer(cols = -subj_id, names_to = "item_name", values_to = "item_value")%>% 
  left_join(., item_instructions, by='item_name')  %>%# add item content 
  filter(!(subj_id %in% excluded))
length(unique(self_long$subj_id)) # should be N=190 subjects

# Infrequncy df 
inf_df <- self_long %>%
  filter(str_detect(item_name, "inf")) %>% # Add a new column to indicate whether the answer failed the attention check
  mutate(attention_check_failed = case_when(
    item_name == "OCI.R_inf_1" & item_value != "0" ~ TRUE,
    item_name == "OCI.R_inf_2" & item_value != "0" ~ TRUE,
    item_name == "zung_inf_1" & !(item_value %in% c("4", "3")) ~ TRUE,
    item_name == "zung_inf_2" & item_value != "1" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(fail_indicator = as.integer(attention_check_failed))

item_failures <- inf_df %>%
  select(subj_id, item_name, fail_indicator) %>%
  pivot_wider(names_from = item_name, values_from = fail_indicator, values_fill = list(fail_indicator = 0)) %>% 
  rowwise() %>%
  mutate(total_fail = sum(c_across(-subj_id)))

# get subj id for failed attention
failed_attention_id <- item_failures %>% filter(total_fail>0) %>% dplyr::pull(subj_id)

length(unique(failed_attention_id))

##################################################
# Data left after failed atten exclusions ---------------------------------------------------------

# tidy_atten_df <- tidy_df %>% filter(!(subj_id %in% failed_attention_id))
# qualtrics_atten_df <- qualtrics_df %>% filter(!(subj_id %in% failed_attention_id))
# demo_atten_df <- demo_df %>% filter(!(subj_id %in% failed_attention_id))
# 
# # N after exclusion - should be N=144
# length(unique(tidy_atten_df$subj_id))
# length(unique(qualtrics_atten_df$subj_id))
# length(unique(demo_atten_df$subj_id))

##################################################
# Process acqui scores ---------------------------------------------------------

neutral_items <- c('neutral_1', 'neutral_2', 'neutral_3', 'neutral_4', 'neutral_5', 'neutral_6', 'neutral_7', 'neutral_8',
                   'neutral_9', 'neutral_10', 'neutral_11', 'neutral_12', 'neutral_13', 'neutral_14')

# This is for N=190
rating_bias_df <- self_long %>% filter(item_name %in% neutral_items) %>% group_by(subj_id) %>%
  summarise(mean_rating = mean(as.numeric(item_value)))

##################################################
# Process oci scores ---------------------------------------------------------

oci_items_df <- self_long %>% filter(str_starts(item_name, 'OCI.R')) %>%
  filter(item_name != 'OCI.R_inf_1' & item_name != 'OCI.R_inf_2')

# Convert item_value to numeric
oci_items_df$item_value <- as.numeric(oci_items_df$item_value)

# total oci score 
oci_df_total <- oci_items_df %>% 
  group_by(subj_id) %>% 
  summarise(
    total_oci = sum(item_value)
  ) 

##################################################
# Process sds scores ---------------------------------------------------------

sds_items_df <- self_long %>% filter(str_starts(item_name, 'zung_')) %>% 
  filter(item_name != 'zung_inf_1' & item_name != 'zung_inf_2') 

# reverse coding items 
sds_reversed_items <- c("zung_2", "zung_5", "zung_6", "zung_11", "zung_12", "zung_14", "zung_16", "zung_17", "zung_18", "zung_20")

# Convert item_value to numeric
sds_items_df$item_value <- as.numeric(sds_items_df$item_value)

# Apply reverse scoring
sds_items_df <- sds_items_df %>%
  mutate(item_original_value = item_value, 
         item_value = ifelse(item_name %in% sds_reversed_items, 
                             reverse_scores(item_value), 
                             item_value))

#note if reverse or standard scoring 
sds_items_df <- sds_items_df %>%
  mutate(reversed = ifelse(item_name %in% sds_reversed_items, "reversed", "standard"))

# total sds score 
sds_df_total <- sds_items_df %>% 
  group_by(subj_id) %>% 
  summarise(
    total_sds = sum(item_value)
  ) 

##################################################
# Confidence, accuracy and rating bias data into one df ---------------------------------------------------------
mean_conf_accu_df <- mean_conf_df %>%
  mutate(
    subj_id = as.character(subj_id),  # Ensure subj_id is character
    failed_attention = ifelse(subj_id %in% as.character(failed_attention_id), 'Inattentive', 'Attentive')
  ) %>%
  left_join(acc_df, by = "subj_id")


mean_conf_accu_df$failed_attention <- factor(mean_conf_accu_df$failed_attention, 
                                        levels = c("Attentive", "Inattentive"))


# add rating bias to mean conf data
dat_df <- mean_conf_accu_df %>% left_join(., rating_bias_df)


##################################################
# Add task data to questionnaire data into one df ---------------------------------------------------------

dat_df_sds= merge(dat_df,sds_df_total,by="subj_id")
dat_df_sds_oci= merge(dat_df_sds,oci_df_total,by="subj_id")

# final dataset of N=190, with all variables
df_with_demos= merge(dat_df_sds_oci,demo_df,by="subj_id")

# N=190 sample analysis
cor.test(df_with_demos$mean_conf, df_with_demos$total_sds) # r=-0.11, same as Sarna et al.
cor.test(df_with_demos$mean_conf, df_with_demos$total_oci) # r=0.28, same as Sarna et al.

# However, N=2 do not have Sex data
nosex_id<-demo_df[ (demo_df$Sex!="Male" & demo_df$Sex!="Female"),]$subj_id
df_complete= subset(df_with_demos, df_with_demos$Sex %in% c("Male", "Female")) # complete cases only
df_complete$Sex = as.factor(df_complete$Sex)
df_complete$Age = as.numeric(df_complete$Age)

# N=188 left
length(unique(df_complete$subj_id))

# full sample analysis Table 1 Full sample N=188
summary(lm(mean_conf~scale(total_sds), data=df_complete)) # Model 1
summary(lm(mean_conf~scale(total_oci), data=df_complete)) # Model 2
summary(lm(mean_conf~scale(total_sds)+scale(total_oci) + scale(as.numeric(Age))+(Sex), data=df_complete)) # Model 3

# removing inattentive responders Table 1 N=142
sub_df = subset(df_complete, df_complete$failed_attention=="Attentive")
length(unique(sub_df$subj_id))

summary(lm(mean_conf~scale(total_sds), data=sub_df)) # Model 1
summary(lm(mean_conf~scale(total_oci), data=sub_df))  # Model 2
summary(lm(mean_conf~scale(total_sds)+scale(total_oci) + scale(as.numeric(Age))+(Sex), data=sub_df)) # Model 3


##################################################
# Correct acqui from questionnaire data ---------------------------------------------------------

# This should be performed after exclusions aka on N=142!

# Correcting OCI scores ---------------------------------------------------------

# add bias rating to oci tiems
oci_items_acqui_df <- oci_items_df %>%
   left_join(., df_with_demos %>% select(subj_id, mean_rating), by = c("subj_id" = "subj_id"))

# only subset out attentive subjects with sex data
exclud_id<-c(nosex_id,failed_attention_id )

oci_items_acqui_atten_df<-oci_items_acqui_df[!oci_items_acqui_df$subj_id %in% exclud_id,]
length(unique(oci_items_acqui_atten_df$subj_id)) #N=142

# Initialize a list to store results
oci_corrected <- list()

# get unique item names
oci_items <- unique(oci_items_acqui_atten_df$item_name)

# Loop over each item and fit a linear model
for (oci_item in oci_items) {
  # Subset data for the current item
  item_data <- oci_items_acqui_atten_df%>% filter(item_name == oci_item)
  
  # Fit  linear model
  model <- lm(item_value ~ mean_rating, data = item_data)
  
  # Extract residuals
  item_data$item_corrected <- residuals(model)
  
  # Store the modified data
  oci_corrected[[oci_item]] <- item_data
}

# Combine the results into a single dataframe
oci_items_corrected_df<- bind_rows(oci_corrected)

# create new total corrected oci df 
# attentive_oci_df_corrected <- oci_items_corrected_df %>% 
#   group_by(subj_id) %>% 
#   summarise(total_oci = sum(item_value), total_oci_corrected = sum(item_corrected)) %>% # scaling the corrected scale to the same scale as the original oci scale
#   mutate(total_oci_corrected_scaled = (total_oci_corrected - min(total_oci_corrected)) / 
#            (max(total_oci_corrected) - min(total_oci_corrected)) *max(total_oci)  )

attentive_oci_df_corrected <- oci_items_corrected_df %>% 
  group_by(subj_id) %>% 
  summarise(total_oci = sum(item_value), total_oci_corrected = sum(item_corrected), .groups = "drop") %>%
  mutate(total_oci_corrected_scaled = (total_oci_corrected - min(total_oci_corrected)) / 
           (max(total_oci_corrected) - min(total_oci_corrected)) * max(total_oci))

length(unique(attentive_oci_df_corrected$subj_id))

# Join scaled scores to sub_df
sub_df_oci_acq <- sub_df %>%
  left_join(attentive_oci_df_corrected %>%
              rename(subj_id = subj_id) %>% 
              select(subj_id, total_oci_corrected,total_oci_corrected_scaled),
            by = 'subj_id')


# Correcting SDS scores ---------------------------------------------------------

# apply correction to SDS
sds_items_acqui_df <- sds_items_df %>%
  left_join(., df_with_demos %>% select(subj_id, mean_rating), by = c("subj_id" = "subj_id"))

sds_items_acqui_atten_df<-sds_items_acqui_df[!sds_items_acqui_df$subj_id %in% exclud_id,]
length(unique(sds_items_acqui_atten_df$subj_id)) #N=142

# Initialize a list to store results
sds_corrected <- list()


# get unique item names
sds_items <- unique(sds_items_acqui_atten_df$item_name)

# Loop over each item and fit a linear model
for (sds_item in sds_items) {
  # Subset data for the current item
  item_data <- sds_items_acqui_atten_df%>% filter(item_name == sds_item)
  
  # Fit  linear model
  model <- lm(item_value ~ mean_rating, data = item_data)
  
  # Extract residuals
  item_data$item_corrected <- residuals(model)
  
  # Store the modified data
  sds_corrected[[sds_item]] <- item_data
}

# Combine the results into a single dataframe
sds_items_corrected_df<- bind_rows(sds_corrected)

# create new total corrected oci df 
# attentive_sds_df_corrected <- sds_items_corrected_df %>% 
#   group_by(subj_id) %>% 
#   summarise(total_sds = sum(item_value), 
#             total_sds_corrected = sum(item_corrected),
#             total_sds_rating_space = sum(item_original_value),
#             total_sds_positive = sum(item_value[reversed == "standard"]),
#             total_sds_negative = sum(item_value[reversed == "reversed"]),
#             total_sds_positive_corrected = sum(item_corrected[reversed == "standard"]),
#             total_sds_negative_corrected = sum(item_corrected[reversed == "reversed"])
#             ) %>% # scaling the corrected scale to the same scale as the original sds scale
#   mutate(total_sds_corrected_scaled = (total_sds_corrected - min(total_sds_corrected)) / 
#            (max(total_sds_corrected) - min(total_sds_corrected)) *  max(total_sds) )

attentive_sds_df_corrected <- sds_items_corrected_df %>%
  group_by(subj_id) %>%
  summarise(
    total_sds = sum(item_value),
    total_sds_corrected = sum(item_corrected),
    total_sds_rating_space = sum(item_original_value),
    total_sds_positive = sum(item_value[reversed == "standard"]),
    total_sds_negative = sum(item_value[reversed == "reversed"]),
    total_sds_positive_corrected = sum(item_corrected[reversed == "standard"]),
    total_sds_negative_corrected = sum(item_corrected[reversed == "reversed"]),
    .groups = "drop"
  ) %>%
  mutate(total_sds_corrected_scaled = (total_sds_corrected - min(total_sds_corrected)) /
           (max(total_sds_corrected) - min(total_sds_corrected)) * max(total_sds))


length(unique(attentive_sds_df_corrected$subj_id))

# Join scaled scores to sub_df
sub_df_oci_sds_acq <- sub_df_oci_acq %>%
  left_join(attentive_sds_df_corrected %>%
              rename(subj_id = subj_id) %>% 
              select(subj_id, total_sds_corrected,total_sds_corrected_scaled),
            by = 'subj_id')


length(unique(sub_df_oci_sds_acq$subj_id))


# with N=144, attentive only (duplicate of attentive only analyses above)
# summary(lm(mean_conf~scale(total_sds), data=sub_df_oci_sds_acq )) 
# summary(lm(mean_conf~scale(total_oci), data=sub_df_oci_sds_acq )) 
# summary(lm(mean_conf~scale(total_sds)+scale(total_oci) + scale(as.numeric(Age))+Sex, data=sub_df_oci_sds_acq ))
