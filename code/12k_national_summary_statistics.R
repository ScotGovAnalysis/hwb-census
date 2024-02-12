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

hwb_analysis <- read_xlsx(file.path(raw_data_folder, year, "Merged", "09_joined_stages.xlsx"), sheet = 1)



### 2 - Read in pupil census ----

# Construct the stages list for the IN clause
sql_stages <- paste0(shQuote(all_stages, type = "sh"), collapse = ",")

# Construct the SQL query
query <- glue("SELECT ScottishCandidateNumber, 
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
               StudentStage IN ({sql_stages}) AND
               SchoolFundingType IN ('2', '3')")

# Execute the query
pupil_census <- execute_sql(
  server = "s0855a\\dbXed",
  database = "char_and_ref",
  sql = query,
  output = TRUE
)



### 3 - Read in ASN and SIMD data and join to pupil census ----

# Construct the SQL query to get asn data
query <- glue("SELECT StudentId, 
               Type
               FROM sch.studentneed_{studentnaturesupport_year}
               WHERE Type NOT IN ('05')")

# Execute the query
asn_data <- execute_sql(
  server = "s0855a\\dbXed",
  database = "char_and_ref",
  sql = query,
  output = TRUE
)

# Remove duplicates from asn_data
asn_data <- asn_data %>%
  distinct(StudentId) %>% 
  mutate(asn = "Yes")

# Join asn_data onto pupil_census
pupil_census <- left_join(pupil_census, asn_data, by = "StudentId")

# Replace asn NA values with "No"
pupil_census$asn[is.na(pupil_census$asn)] <- "No"


# Load Data Zone - SIMD quintile lookup table (based on 2011 Data Zones - future version based on 2022 Census will become available soon)
# THIS WILL NEED TO BE CHECKED FOR UPDATES EACH YEAR
# (https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-data-zone-look-up/)
# Only the "SIMD 2020v2 DZ lookup data" sheet from the workbook will be required
dz_simd <-
  read.csv(here("external_datasets_required",
                year,
                "data_zone_simd_for_r.csv")) %>%
  select(DZ, SIMD2020v2_Quintile) # keep only Data Zone & SIMD quintile columns

# Join dz_simd onto pupil_census
pupil_census <- left_join(pupil_census, dz_simd, by = c("Datazone2011" = "DZ"))

# Remove columns that aren't needed
pupil_census <- pupil_census %>%
  select(-StudentId, -OnRoll, -Datazone2011)

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

# Remove local authorities that didn't participate in HWB survey
pupil_census <- pupil_census %>%
  filter(pc_la %in% all_las)



### 5 - Create first two tables of responses by local authority ----

# Perform analysis of counts for hwb
hwb_count_la <- hwb_analysis %>%
    count(pc_la, pc_stage) %>%
    pivot_wider(names_from = pc_stage, values_from = n, values_fill = 0) %>%
    adorn_totals(c("row")) %>%
    mutate(Total = rowSums(across(-1)))

# Perform analysis of counts for pupil census
pc_count_la <- pupil_census %>%
  count(pc_la, pc_stage) %>%
  pivot_wider(names_from = pc_stage, values_from = n, values_fill = 0) %>%
  adorn_totals(c("row")) %>%
  mutate(Total = rowSums(across(-1)))

# Create a new dataframe to store the results
hwb_pct_la <- data.frame(hwb_count_la[, 1])  # Keep the first column from summary_table

# Loop through each row and column (excluding the first column) to perform the division operation
for (i in 1:nrow(hwb_count_la)) {
  for (j in 2:ncol(hwb_count_la)) { # Start from the second column as the first one is not numeric
    # For the columns other than the first one, perform the division and store the result in the test dataframe
    hwb_pct_la[i, j] <- round(hwb_count_la[i, j] / pc_count_la[i, j] * 100, 2)
  }
}



### 6 - Create last five tables of percentage of respondents by stage and characteristic ----
  
# For Gender and asn we don't want "Not known" as a response option, we do for the other characteristics  

cat_order_1 <- c("Female",
                 "Male")

hwb_analysis_filtered_gender <- hwb_analysis %>%
  filter(Gender != "Not known")

hwb_analysis_filtered_asn <- hwb_analysis %>%
  filter(asn != "Not known")

hwb_Gender <- analysis_one_characteristic(hwb_analysis_filtered_gender, pc_stage, Gender, "Gender", "cat_order_1")
hwb_SIMD2020v2_Quintile <- analysis_one_characteristic(hwb_analysis, pc_stage, SIMD2020v2_Quintile, "SIMD2020v2_Quintile", "cat_order_1")
hwb_EthnicBackground <- analysis_one_characteristic(hwb_analysis, pc_stage, EthnicBackground, "EthnicBackground", "cat_order_1")
hwb_UrbRur6 <- analysis_one_characteristic(hwb_analysis, pc_stage, UrbRur6, "UrbRur6", "cat_order_1")
hwb_asn <- analysis_one_characteristic(hwb_analysis_filtered_asn, pc_stage, asn, "asn", "cat_order_1")



### 7 - Create list of all seven dataframes ----

final_list <- list(hwb_count_la, hwb_pct_la, hwb_Gender, hwb_SIMD2020v2_Quintile, hwb_EthnicBackground, hwb_UrbRur6, hwb_asn)
names(final_list) <- c("hwb_count_la", "hwb_pct_la", "hwb_Gender", "hwb_SIMD2020v2_Quintile", "hwb_EthnicBackground", "hwb_UrbRur6", "hwb_asn")



### 8 - Save final_list as an excel file to Merged folder for national only ----

write_xlsx(
  final_list,
  here("output", year, "National", paste0(year, "_national_summary_statistics.xlsx"))
)


### END OF SCRIPT ###


