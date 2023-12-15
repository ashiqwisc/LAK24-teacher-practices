library(tidyverse)

# Read in the cumulative dataframe
df <- read_csv("./datasets/final-sample-cb-lak24-extended.csv")

# Replace the NA values in screenalignment_binary with 0 
df <- df %>%
  mutate(screenalignment_binary = ifelse(is.na(screenalignment_binary), 0, screenalignment_binary))

# Define the columns that each category consists of 
tutor_log_columns <- c("Correct_attempt", "first_correct_attempt", "Incorrect_attempt", "hint_request")
detector_prediction_columns <- c("Struggle_State", "Idle_State", "Misuse_State")
out_of_tutor_columns <- c("Raising_hand", "Talking", "screenalignment_binary")

# Count the number of rows in each category 
total_tutor_log <- df %>%
  filter(modality == "tutor") %>%
  nrow()

total_detector_prediction <- df %>%
  filter(modality == "detector") %>%
  nrow() 

total_out_of_tutor <- df %>%
  filter(modality == "observation" | modality == "position") %>%
  nrow()

# Count the total occurrence # for each column in each category 
count_ones_tutor_log <- df %>%
  filter(modality == "tutor") %>%
  summarise(across(all_of(tutor_log_columns), sum)) %>%
  gather(key = "column", value = "count") %>%
  mutate(category = "Tutor Log")

count_ones_detector_prediction <- df %>%
  filter(modality == "detector") %>%
  summarise(across(all_of(detector_prediction_columns), sum)) %>%
  gather(key = "column", value = "count") %>%
  mutate(category = "Detector Prediction")

count_ones_out_of_tutor <- df %>%
  filter(modality == "observation" | modality == "position") %>%
  summarise(across(all_of(out_of_tutor_columns), sum)) %>%
  gather(key = "column", value = "count") %>%
  mutate(category = "Out-of-Tutor Interactions")

# Combine the results into one dataframe 
count_ones_combined <- bind_rows(count_ones_tutor_log, count_ones_detector_prediction, count_ones_out_of_tutor)

# Calculate the base rates for each column within categories
proportion_ones <- count_ones_combined %>%
  group_by(category, column) %>%
  mutate(category_count = 
           case_when((category == "Tutor Log") ~ total_tutor_log, 
                     (category == "Detector Prediction") ~ total_detector_prediction,
                     (category == "Out-of-Tutor Interactions") ~ total_out_of_tutor)) %>%
  mutate(within_category_proportion = count / category_count)

