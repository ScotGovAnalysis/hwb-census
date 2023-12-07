#########################################################################
# Name of file - 12j_national_summary_statistics.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Analyses data for national summary statistics
#########################################################################

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "analysis_one_characteristic.R"))
source(here::here("functions", "analysis_stage_and_characteristic.R"))
source(here::here("functions", "perform_analysis_national.R"))
source(here::here("functions", "perform_analysis_local_authority.R"))
source(here::here("lookups", "lut_la_code.R"))
source(here::here("lookups", "lut_sex.R"))
source(here::here("lookups", "lut_ethnic_background.R"))
source(here::here("lookups", "lut_urbrur6.R"))
source(here::here("lookups", "lut_asn.R"))
source(here::here("lookups", "lut_simd.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx")

# Read in dataframe
hwb_analysis <- readxl::read_xlsx(file_path, sheet = 1)



### 2 - Read in pupil census ----

# Load and select required variables from latest pupil census table 
myconn <- dbConnect(odbc::odbc(), "dbXed", UID="", PWD= "")

# Construct the placeholders for the IN clause
placeholders <- paste(rep("?", length(all_stages)), collapse = ",")

# Construct the SQL query with parameterized IN clause
query <- glue::glue("SELECT ScottishCandidateNumberN,
                     StudentId,
                     LaCode, 
                     OnRoll,
                     StudentStage,
                     Gender,
                     EthnicBackground,
                     UrbRur6,
                     Datazone2011
                     FROM sch.student_{pupil_census_year}
                     WHERE OnRoll = '1' AND
                     StudentStage IN ({placeholders}) AND
                     SchoolFundingType IN ('2', '3')")

# Execute the query using parameterized inputs
pupil_census <- dbGetQuery(myconn, query, params = all_stages)



### 3 - Read in ASN and SIMD data and join to pupil census ----

# Load and select required variables from the latest studentnaturesupport table (for ASN)
asn_data <- dbGetQuery(myconn, paste(paste0("SELECT StudentId, Type FROM sch.studentneed_", studentnaturesupport_year),
                                     "WHERE Type NOT IN ('05')"))

# Remove duplicates from asn_data
asn_data <- asn_data %>%
  distinct(StudentId) %>% 
  mutate(asn = "Yes")

# Join asn_data onto pupil_census
pupil_census <- left_join(pupil_census, asn_data, by = "StudentId")

# Replace asn NA values with "No"
pupil_census$asn[is.na(pupil_census$asn)] <- "No"

# Load Data Zone - SIMD quintile lookup table (based on 2011 Data Zones - future version based on 2022 Census will become available soon)
# (https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-data-zone-look-up/)
# Only the "SIMD 2020v2 DZ lookup data" sheet from the workbook will be required
dz_simd_filename <- "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey/R/HWB Analysis/Documents & files required for analysis/data_zone_simd_for_r.csv"
dz_simd <- read.csv(dz_simd_filename, header = TRUE) %>% 
  select(DZ, SIMD2020v2_Quintile) # keep only Data Zone & SIMD quintile columns

# Join dz_simd onto pupil_census
pupil_census <- left_join(pupil_census, dz_simd, by = c("Datazone2011" = "DZ"))

# Remove columns that aren't needed
pupil_census <- pupil_census %>%
  select(-StudentId, -OnRoll, -Datazone2011)

# Remove local authorities that didn't participate in HWB survey
pupil_census <- pupil_census %>%
  filter(LaCode %in% all_las)

# Rename columns "LaCode" to "pc_la" and "StudentStage" to "pc_stage" for comparison to hwb data
pupil_census <- pupil_census %>%
  rename(pc_la = LaCode, pc_stage = StudentStage)



### 4 - Apply lookup tables to pupil_census ----

# Apply lookup table to pupil_census to rename variables in column "LaCode"
pupil_census$pc_la <- lut_la_code[pupil_census$pc_la]

# Apply lookup table to pupil_census to rename variables in column "Gender"
pupil_census$Gender <- lut_sex[pupil_census$Gender]

# Apply lookup table to pupil_census to rename variables in column "EthnicBackground"
pupil_census$EthnicBackground <- lut_ethnic_background[pupil_census$EthnicBackground]

# Apply lookup table to pupil_census to rename variables in column "UrbRur6"
pupil_census$UrbRur6 <- lut_urbrur6[pupil_census$UrbRur6]

# Apply lookup table to pupil_census to rename variables in column "asn"
pupil_census$asn <- lut_asn[pupil_census$asn]

# Apply lookup table to pupil_census to rename variables in column "SIMD2020v2_Quintile"
pupil_census$SIMD2020v2_Quintile <- lut_simd[pupil_census$SIMD2020v2_Quintile]




### 5 - Create table of number of responses by local authority ----

# Function to perform analysis of counts
perform_summary <- function(data, var) {
  result <- data %>%
    filter({{ var }} != "Not known") %>%
    count({{ var }}, pc_stage) %>%
    pivot_wider(names_from = pc_stage, values_from = n, values_fill = 0) %>%
    adorn_totals(c("row")) %>%
    mutate(Total = rowSums(across(-1)))
  
  return(result)
}

# List of variables
variables <- c("pc_la", "Gender", "SIMD2020v2_Quintile", "EthnicBackground", "UrbRur6", "asn")

# Applying perform_summary function to each variable and storing output as a named list for hwb
hwb_output_list <- map(set_names(variables), ~ perform_summary(hwb_analysis, !!as.name(.x)))

# Applying perform_summary function to each variable and storing output as a named list for pupil_census
pc_output_list <- map(set_names(variables), ~ perform_summary(pupil_census, !!as.name(.x)))



### 6 - Create table of percentage of respondents by stage and local authority ----

# Initialize final_list as an empty list to store the results
final_list <- list()

# Loop through each common name and perform the division operation
for (name in variables) {
  # Extract the tibbles with the same name from each list
  summary_table <- hwb_output_list[[name]]
  summary_table_pc <- pc_output_list[[name]]
  
  # Create an empty dataframe with the same dimensions as summary_table to store results
  test <- data.frame(matrix(ncol = ncol(summary_table), nrow = nrow(summary_table)))
  colnames(test) <- colnames(summary_table)
  rownames(test) <- rownames(summary_table)
  
  # Keep the first column of summary_table in the test dataframe
  test[, 1] <- summary_table[, 1]
  
  # Loop through each row and column (excluding the first column)
  for (i in 1:nrow(summary_table)) {
    for (j in 2:ncol(summary_table)) { # Start from the second column as the first one is not numeric
      # For the columns other than the first one, perform the division and store the result in the test dataframe
      test[i, j] <- round(summary_table[i, j] / summary_table_pc[i, j] * 100, 2)
    }
  }
  
  # Assign the last row as counts, not percentages
  last_row <- nrow(summary_table)
  test[last_row, -1] <- round(summary_table[last_row, -1], 0)
  
  # Assign the resulting dataframe to final_list with the name as the list element
  final_list[[name]] <- test
}



### 7 - Add count of stage and local authority to final_list ----

# Adding pc_la from hwb_output_list to final_list with the new name pc_la_count in the first position in the list
final_list <- c(list(pc_la_count = hwb_output_list$pc_la), final_list)



### 8 - Save final_list as an excel file to Merged folder for national only ----

write_xlsx(
  final_list,
  here("output", year, "National", paste0(year, "_national_summary_statistics.xlsx"))
)


### END OF SCRIPT ###


