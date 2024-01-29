#########################################################################
# Name of file - 12k_carers_analysis.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for carers analysis
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx")

# Read in dataframe
hwb_analysis <- readxl::read_xlsx(file_path, sheet = 1)



### 2 - Set row order of response categories ---

cat_order_1 <- c("Yes",
                 "No",
                 "Not known")



### 3 - Replace response values as per Measures for Inclusion in publication document ---

hwb_analysis$care_for_someone[hwb_analysis$care_for_someone == "Caring responsibilities"] <- "Yes"
hwb_analysis$care_for_someone[hwb_analysis$care_for_someone == "No caring responsibilities"] <- "No"
hwb_analysis$care_for_someone[hwb_analysis$care_for_someone == "Prefer not to say"] <- "Not known"



### 4 - Define variables for analysis ---

variables <- data.frame(
  variable = c("care_for_someone"),
  cat_order = c("cat_order_1")
)



### 5 - Perform analysis on selected variables ---

# For national
stage <- analysis_one_characteristic(hwb_analysis, pc_stage, care_for_someone, "care_for_someone", "cat_order_1")
sex <- analysis_one_characteristic(hwb_analysis, Gender, care_for_someone, "care_for_someone", "cat_order_1")
ethnic_group <- analysis_one_characteristic(hwb_analysis, EthnicBackground, care_for_someone, "care_for_someone", "cat_order_1")
simd <- analysis_one_characteristic(hwb_analysis, SIMD2020v2_Quintile, care_for_someone, "care_for_someone", "cat_order_1")

national_carers_analysis <- list(stage, sex, ethnic_group, simd)
names(national_carers_analysis) <- c("stage", "sex", "ethnic_group", "simd")

# For each local authority
# Initialize empty list
local_authority_list <- list()

for (la in all_las) {
  hwb_la <- hwb_analysis %>%
    filter(pc_la == la)
  
  stage <- analysis_one_characteristic(hwb_la, pc_stage, care_for_someone, "care_for_someone", "cat_order_1")
  sex <- analysis_one_characteristic(hwb_la, Gender, care_for_someone, "care_for_someone", "cat_order_1")
  ethnic_group <- analysis_one_characteristic(hwb_la, EthnicBackground, care_for_someone, "care_for_someone", "cat_order_1")
  simd <- analysis_one_characteristic(hwb_la, SIMD2020v2_Quintile, care_for_someone, "care_for_someone", "cat_order_1")
  
  la_carers_analysis <- list(stage, sex, ethnic_group, simd)
  names(la_carers_analysis) <- c("stage", "sex", "ethnic_group", "simd")
  
  local_authority_list[[paste0(la, "_carers_analysis")]] <- la_carers_analysis
}



### 6 - Save combined_list as an excel file to Merged folder ----

# Save national
write_xlsx(
  national_carers_analysis,
  here("output", year, "National", "Output", paste0(year, "_carers_analysis.xlsx"))
)

# Save local authorities
# Function to save tibbles in respective folders
save_tibbles_as_sheets <- function(tibble_list, folder_name) {
  file_path <- here::here("output", year, folder_name, "Output", paste0(year, "_carers_analysis.xlsx"))
  write_xlsx(
    tibble_list,
    path = file_path,
    col_names = TRUE
  )
}

# Save each list of tibbles as a single Excel file with multiple sheets
map2(local_authority_list, all_las, save_tibbles_as_sheets)



### END OF SCRIPT ###

