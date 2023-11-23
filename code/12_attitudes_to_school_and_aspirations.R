#########################################################################
# Name of file - 12_attitudes_to_school_and_aspirations.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses date for attitudes to school and aspirations
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx")

# Read in dataframe
hwb_analysis <- readxl::read_xlsx(file_path, sheet = 1)



### 2 - Set row order of response categories ---

cat_order_1 <- c("Strongly agree or Agree",
                 "Neither agree nor disagree",
                 "Strongly disagree or Disagree",
                 "Prefer not to say")

cat_order_2 <- c("Not at all", 
                 "A little", 
                 "Some",
                 "A lot",
                 "Prefer not to say")



### 3 - Replace response values as per Measures for Inclusion in publication document ---
hwb_analysis[hwb_analysis == "Strongly agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Agree"] <- "Strongly agree or Agree"
hwb_analysis[hwb_analysis == "Strongly disagree"] <- "Strongly disagree or Disagree"
hwb_analysis[hwb_analysis == "Disagree"] <- "Strongly disagree or Disagree"



### 4 - Perform analysis ---

df <- data.frame(
  variable = c("enjoy_learning_new_things", 
               "choice_of_learning", 
               "happy_at_school",
               "teachers_treat_me_fairly",
               "positive_about_future",
               "pressured_by_schoolwork",
               "have_adult_to_talk_to_school"),
  cat_order = c("cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_1", 
                "cat_order_2", 
                "cat_order_1")
)

df2 <- data.frame(
  var = c("Gender", "EthnicBackground")
)






sex_all <- 1:nrow(df) %>%
  lapply(function(i) {
    var_name <- df$variable[i]
    cat_order <- df$cat_order[i]
    
      analysis_one_characteristic(Gender, !!as.name(var_name), var_name, cat_order)
      
  }) %>%
  do.call(rbind, .)



## WORKING BUT NO NAME
result_list <- lapply(df2$var, function(var) {
  var_results <- lapply(1:nrow(df), function(i) {
    var_name <- df$variable[i]
    cat_order <- df$cat_order[i]
    analysis_one_characteristic(!!as.name(var), !!as.name(var_name), var_name, cat_order)
  }) %>%
    bind_rows()
  
  var_results
})


result_list <- lapply(df2$var, function(var) {
  var_result <- lapply(1:nrow(df), function(i) {
    var_name <- df$variable[i]
    cat_order <- df$cat_order[i]
    analysis_one_characteristic(!!as.name(var), !!as.name(var_name), var_name, cat_order)
  }) %>%
    bind_rows()
  
  as_tibble(var_result)
})

names(result_list) <- df2$var

test <- result_list$Gender
test2 <- result_list$EthnicBackground










# stage_q1 <- one_characteristic(pc_stage, enjoy_learning_new_things, "enjoy_learning_new_things", cat_order_1)
# stage_q2 <- one_characteristic(pc_stage, choice_of_learning, "choice_of_learning", cat_order_1)
# stage_q3 <- one_characteristic(pc_stage, happy_at_school, "happy_at_school", cat_order_1)
# stage_q4 <- one_characteristic(pc_stage, teachers_treat_me_fairly, "teachers_treat_me_fairly", cat_order_1)
# stage_q5 <- one_characteristic(pc_stage, positive_about_future, "positive_about_future", cat_order_1)
# stage_q6 <- one_characteristic(pc_stage, pressured_by_schoolwork, "pressured_by_schoolwork", cat_order_2)
# stage_q7 <- one_characteristic(pc_stage, have_adult_to_talk_to_school, "have_adult_to_talk_to_school", cat_order_1)
# stage_all <- rbind(stage_q1, stage_q2, stage_q3, stage_q4, stage_q5, stage_q6, stage_q7)

stage_q1 <- one_characteristic(Gender, enjoy_learning_new_things, "enjoy_learning_new_things", cat_order_1)
stage_q2 <- one_characteristic(Gender, choice_of_learning, "choice_of_learning", cat_order_1)
stage_q3 <- one_characteristic(Gender, happy_at_school, "happy_at_school", cat_order_1)
stage_q4 <- one_characteristic(Gender, teachers_treat_me_fairly, "teachers_treat_me_fairly", cat_order_1)
stage_q5 <- one_characteristic(Gender, positive_about_future, "positive_about_future", cat_order_1)
stage_q6 <- one_characteristic(Gender, pressured_by_schoolwork, "pressured_by_schoolwork", cat_order_2)
stage_q7 <- one_characteristic(Gender, have_adult_to_talk_to_school, "have_adult_to_talk_to_school", cat_order_1)















sex_and_stage_q1 <- analysis_stage_and_characteristic(Gender, enjoy_learning_new_things, "enjoy_learning_new_things", cat_order_1)

sex_q1 <- one_characteristic(Gender, enjoy_learning_new_things, "enjoy_learning_new_things", cat_order_1)






