#########################################################################
# Name of file - 11_join_to_asn_and_simd.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Joins data onto ASN and SIMD data and applies lookups ready for analysis.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("lookups", "lookup_sex.R"))
source(here::here("lookups", "lookup_ethnic_background.R"))
source(here::here("lookups", "lookup_urbrur6.R"))
source(here::here("lookups", "lookup_asn.R"))
source(here::here("lookups", "lookup_simd.R"))
source(here::here("lookups", "lookup_care_for_someone.R"))
source(here::here("lookups", "lookup_long_term_condition.R"))



### 1 - Read in raw data ----

raw_data <- read_xlsx(file.path(raw_data_folder, year, "Merged", "08_removed_records.xlsx"), sheet = 1)



### 2 - Read in ASN and SIMD data and join to raw_data ----

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

# Join asn_data onto raw_data
joined_data <- left_join(raw_data, asn_data, by = "StudentId")

# Replace asn NA values with "No"
joined_data$asn[is.na(joined_data$asn)] <- "No"


# Load Data Zone - SIMD quintile lookup table (based on 2011 Data Zones - future version based on 2022 Census will become available soon)
# THIS WILL NEED TO BE CHECKED FOR UPDATES EACH YEAR
# (https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-data-zone-look-up/)
# Only the "SIMD 2020v2 DZ lookup data" sheet from the workbook will be required
dz_simd <-
  read.csv(here("external_datasets_required",
                 year,
                 "data_zone_simd_for_r.csv")) %>%
  select(DZ, SIMD2020v2_Quintile) # keep only Data Zone & SIMD quintile columns

# Join dz_simd onto joined_data
joined_data <- left_join(joined_data, dz_simd, by = c("Datazone2011" = "DZ"))



### 3 - Modify joined_data ----

# Remove columns we don't need for analysis (StudentId, DataZone2011)
joined_data <- joined_data %>%
  select(-StudentId, -Datazone2011)

# Reorder columns
joined_data <- joined_data %>%
  select(pc_stage, pc_la, hwb_scn, school_name, seed_code, Gender, EthnicBackground, UrbRur6, 
         asn, SIMD2020v2_Quintile, hwb_stage, hwb_la, take_part, everything())

# Replace NAs in columns Gender, EthnicBackground,UrbRur6, asn, SIMD2020v2_Quintile, care_for_someone, long_term_condition with 
# "Unknown" (for ease of applying lookups)
# Define which columns to replace NA values with "Unknown"
cols_to_replace <- c("Gender", "EthnicBackground", "UrbRur6", "asn", "SIMD2020v2_Quintile", "care_for_someone", "long_term_condition")

# Loop through columns and replace NA with "Unknown"
for (col in cols_to_replace) {
  joined_data[[col]][is.na(joined_data[[col]])] <- "Unknown"
}

# Apply lookup table to joined_data to rename variables in column "Gender"
joined_data$Gender <- lookup_sex[joined_data$Gender]

# Apply lookup table to joined_data to rename variables in column "EthnicBackground"
joined_data$EthnicBackground <- lookup_ethnic_background[joined_data$EthnicBackground]

# Apply lookup table to joined_data to rename variables in column "UrbRur6"
joined_data$UrbRur6 <- lookup_urbrur6[joined_data$UrbRur6]

# Apply lookup table to joined_data to rename variables in column "asn"
joined_data$asn <- lookup_asn[joined_data$asn]

# Apply lookup table to joined_data to rename variables in column "SIMD2020v2_Quintile"
joined_data$SIMD2020v2_Quintile <- lookup_simd[joined_data$SIMD2020v2_Quintile]

# Apply lookup table to joined_data to rename variables in column "care_for_someone"
joined_data$care_for_someone <- lookup_care_for_someone[joined_data$care_for_someone]

# Apply lookup table to joined_data to rename variables in column "long_term_condition"
joined_data$long_term_condition <- lookup_long_term_condition[joined_data$long_term_condition]



### 4 - Remove characteristics from East Renfrewshire records ----

# If a respondent completed the survey in a local authority that wasn't East Renfrewshire, but that respondent's scn was East Renfrewshire 
# in the pupil census, then for analysis they will be counted as East Renfrewshire. As such, their characteristics must be removed.

# Replace characteristics hwb_scn, school_name and seed_code with "Data not collected" when pc_la = "East Renfrewshire"
joined_data$hwb_scn[joined_data$pc_la == "East Renfrewshire"] <- "Data not collected"
joined_data$school_name[joined_data$pc_la == "East Renfrewshire"] <- "Data not collected"
joined_data$seed_code[joined_data$pc_la == "East Renfrewshire"] <- "Data not collected"

# Replace characteristics Gender, EthnicBackground, UrbRur6, asn, SIMD2020v2_Quintile with "Not known" when pc_la = "East Renfrewshire"
joined_data$Gender[joined_data$pc_la == "East Renfrewshire"] <- "Not known"
joined_data$EthnicBackground[joined_data$pc_la == "East Renfrewshire"] <- "Not known"
joined_data$UrbRur6[joined_data$pc_la == "East Renfrewshire"] <- "Not known"
joined_data$asn[joined_data$pc_la == "East Renfrewshire"] <- "Not known"
joined_data$SIMD2020v2_Quintile[joined_data$pc_la == "East Renfrewshire"] <- "Not known"



### 5 - Save joined_data as an excel file to Merged folder ----

write_xlsx(
  joined_data,
  file.path(raw_data_folder, year, "Merged", paste0("09_joined_stages.xlsx"))
)


### END OF SCRIPT ###
