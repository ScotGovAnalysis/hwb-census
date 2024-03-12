#########################################################################
# Name of file - 12d_mental_health_and_wellbeing_sdq.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for mental health and wellbeing SDQ
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))



### 1 - Read in raw data ----

hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)



### 2 - Set row order of response categories ----

cat_order_1 <- c("Normal",
                 "Borderline or Abnormal")

cat_order_2 <- c("Close to average",
                 "Slightly raised, High, or Very high")



### 3 - Create derived variable for SDQ scores ----

# Identify columns that start with "sdq"
sdq_columns <- grep("^sdq", names(hwb_analysis), value = TRUE)

# Identify columns that are recoded with 0 to "Certainly true" and 2 to "Not true" (Qs 7,11,14,21,25)
sdq_columns_a <- c("sdq_7_do_as_told", "sdq_11_good_friend", "sdq_14_people_like_me", "sdq_21_think_before_doing", "sdq_25_good_attention")

# Identify columns that are recoded with 2 to "Certainly true" and 0 to "Not true" (All other Qs)
sdq_columns_b <- setdiff(sdq_columns, sdq_columns_a)

# Replace values in SDQ columns with numeric scores based on conditions
hwb_analysis <- hwb_analysis %>%
  mutate(across(all_of(sdq_columns_a), ~case_when(
    . == "Certainly true" ~ 0,
    . == "Somewhat true" ~ 1,
    . == "Not true" ~ 2,
    TRUE ~ NA_real_
  )))

hwb_analysis <- hwb_analysis %>%
  mutate(across(all_of(sdq_columns_b), ~case_when(
    . == "Certainly true" ~ 2,
    . == "Somewhat true" ~ 1,
    . == "Not true" ~ 0,
    TRUE ~ NA_real_
  )))

# Convert columns to numeric
hwb_analysis[sdq_columns] <- lapply(hwb_analysis[sdq_columns], as.numeric)

# Calculate individual scales
hwb_analysis <- hwb_analysis %>% 
  mutate("emotional" = sdq_3_pains + sdq_8_worry_a_lot + sdq_13_often_unhappy + sdq_16_nervous_in_situations + sdq_24_fears) %>% 
  mutate("conduct" = sdq_5_lose_temper + sdq_7_do_as_told + sdq_12_fight_a_lot + sdq_18_accused_of_lying + sdq_22_taking_things) %>% 
  mutate("hyperactivity" = sdq_2_restless + sdq_10_fidgeting + sdq_15_easily_distracted + sdq_21_think_before_doing + sdq_25_good_attention) %>%
  mutate("peer" = sdq_6_usually_alone + sdq_11_good_friend + sdq_14_people_like_me + sdq_19_bullied + sdq_23_get_on_adults_more) %>%
  mutate("prosocial" = sdq_1_nice_to_others + sdq_4_share_with_others + sdq_9_helping_others + sdq_17_kind_to_younger_children + sdq_20_volunteering)

# Calculate total difficulties score
hwb_analysis <- hwb_analysis %>% 
  mutate("total_difficulties" = emotional + conduct + hyperactivity + peer)

# Create bandings for percentage calculations
# Total Difficulties score
# Original 3-band categorisation
hwb_analysis$'Total difficulties score % - three band scale' <- ifelse(
  hwb_analysis$total_difficulties <= 15, 
  'Normal',
  ifelse(
    hwb_analysis$total_difficulties >= 16 & hwb_analysis$total_difficulties <= 40,
    'Borderline or Abnormal',
    NA
  )
)

# Total Difficulties score
# Newer 4-band categorisation
hwb_analysis$'Total difficulties score % - four band scale' <- ifelse(
  hwb_analysis$total_difficulties <= 14, 
  'Close to average',
  ifelse(
    hwb_analysis$total_difficulties >= 15 & hwb_analysis$total_difficulties <= 40,
    'Slightly raised, High, or Very high',
    NA
  )
)

# Emotional Problems score 
# Newer 4-band categorisation
hwb_analysis$'Emotional problems score %' <- ifelse(
  hwb_analysis$emotional <= 4, 
  'Close to average',
  ifelse(
    hwb_analysis$emotional >= 5 & hwb_analysis$emotional <= 10,
    'Slightly raised, High, or Very high',
    NA
  )
)

# Conduct Problems score
# Newer 4-band categorisation
hwb_analysis$'Conduct problems score %' <- ifelse(
  hwb_analysis$conduct <= 3, 
  'Close to average',
  ifelse(
    hwb_analysis$conduct >= 4 & hwb_analysis$conduct <= 10,
    'Slightly raised, High, or Very high',
    NA
  )
)

# Hyperactivity score 
# Newer 4-band categorisation
hwb_analysis$'Hyperactivity score %' <- ifelse(
  hwb_analysis$hyperactivity <= 5, 
  'Close to average',
  ifelse(
    hwb_analysis$hyperactivity >= 6 & hwb_analysis$hyperactivity <= 10,
    'Slightly raised, High, or Very high',
    NA
  )
)

# Peer Problems score
# Newer 4-band categorisation
hwb_analysis$'Peer problems score %' <- ifelse(
  hwb_analysis$peer <= 2, 
  'Close to average',
  ifelse(
    hwb_analysis$peer >= 3 & hwb_analysis$peer <= 10,
    'Slightly raised, High, or Very high',
    NA
  )
)

# Prosocial score  
# Newer 4-band categorisation
hwb_analysis$'Prosocial score %' <- ifelse(
  hwb_analysis$prosocial <= 6, 
  'Slightly raised, High, or Very high',
  ifelse(
    hwb_analysis$prosocial >= 7 & hwb_analysis$prosocial <= 10,
    'Close to average',
    NA
  )
)



### 4 - Define variables for analysis ----

variables <- data.frame(
  variable = c("Total difficulties score % - three band scale", 
               "Total difficulties score % - four band scale", 
               "Emotional problems score %",
               "Conduct problems score %",
               "Hyperactivity score %",
               "Peer problems score %",
               "Prosocial score %"),
  cat_order = c("cat_order_1", 
                "cat_order_2", 
                "cat_order_2", 
                "cat_order_2", 
                "cat_order_2",
                "cat_order_2",
                "cat_order_2")
)



### 5 - Perform analysis on selected variables ----

# For national
national_sdq <- perform_analysis_national(hwb_analysis, variables)

# For each local authority
local_authority_list <- list()

for (value in all_las) {
  cat("Processing local authority:", value, "\n")
  filtered_data <- hwb_analysis[hwb_analysis$pc_la == value, ]
  result <- perform_analysis_local_authority(filtered_data, variables)
  # Store the result with a dynamic name, e.g. Angus_sdq
  list_name <- paste0(value, "_sdq")
  local_authority_list[[list_name]] <- result
}



### 6 - Save outputs as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_sdq,
  here("output", year, "National", "Output", paste0(year, "_mental_health_and_wellbeing_sdq.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_mental_health_and_wellbeing_sdq.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)


### END OF SCRIPT ###

