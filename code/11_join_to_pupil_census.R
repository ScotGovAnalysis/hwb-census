#########################################################################
# Name of file - 11_join_to_pupil_census.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Joins data onto pupil census and other data sources,
# merges into one dataframe and applies lookups ready for analysis.
#########################################################################


### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("lookups", "lut_sex.R"))
source(here::here("lookups", "lut_ethnic_background.R"))
source(here::here("lookups", "lut_urbrur6.R"))
source(here::here("lookups", "lut_asn.R"))
source(here::here("lookups", "lut_simd.R"))
source(here::here("lookups", "lut_care_for_someone.R"))
source(here::here("lookups", "lut_long_term_condition.R"))



### 1 - Read in raw data ----

# Define the path to Excel file
file_path <- file.path(raw_data_folder, year, "Merged", "08_removed_records.xlsx")

# Read in all sheets into a list of tibbles with names
raw_data <- set_names(
  map(
    all_stages,
    ~ read_xlsx(file_path, sheet = .x)
  ),
  all_stages
)



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

# COME BACK TO
# Load Data Zone - SIMD quintile lookup table (based on 2011 Data Zones - future version based on 2022 Census will become available soon)
# (https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-data-zone-look-up/)
# Only the "SIMD 2020v2 DZ lookup data" sheet from the workbook will be required
dz_simd_filename <- "//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey/R/HWB Analysis/Documents & files required for analysis/data_zone_simd_for_r.csv"
dz_simd <- read.csv(dz_simd_filename, header = TRUE) %>% 
  select(DZ, SIMD2020v2_Quintile) # keep only Data Zone & SIMD quintile columns

# Join dz_simd onto pupil_census
pupil_census <- left_join(pupil_census, dz_simd, by = c("Datazone2011" = "DZ"))



### 4 - Join pupil_census onto raw_data ----

# Define a function to perform left join of pupil_census onto raw_data by SCN
left_join_census <- function(tibble) {
  
  tibble_filtered <- tibble %>%
    mutate(hwb_scn_cleaned = ifelse(hwb_scn == "Data not collected", hwb_scn, as.character(as.numeric(hwb_scn)))) # Ensure hwb_scn_cleaned remains character
  
  pupil_census_cleaned <- pupil_census %>%
    mutate(pc_scn_cleaned = as.character(ScottishCandidateNumberN)) # Ensure pc_scn_cleaned remains character
  
  result <- left_join(tibble_filtered, pupil_census_cleaned, by = c("hwb_scn_cleaned" = "pc_scn_cleaned"))
  
  return(result)
}

# Use map function to apply the join_data function to each tibble in raw_data
joined_data <- map(raw_data, left_join_census)


# Remove columns we don't need for analysis (hwb_scn_cleaned, ScottishCandidateNumberN, StudentId, LaCode, OnRoll, StudentStage, DataZone2011)
# Function to remove columns
removeColumns <- function(tibble) {
  tibble <- tibble %>%
    select(-hwb_scn_cleaned, -ScottishCandidateNumberN, -StudentId, -LaCode, -OnRoll, -StudentStage, -Datazone2011)
  return(tibble)
}

# Applying the function to every tibble in joined_data
joined_data <- map(joined_data, ~ removeColumns(.))


# Reorder columns
# Function to reorder columns in each tibble
reorder_columns <- function(tibble) {
  tibble <- tibble %>%
    select(pc_stage, pc_la, hwb_scn, school_name, seed_code, Gender, EthnicBackground, UrbRur6, 
           asn, SIMD2020v2_Quintile, hwb_stage, hwb_la, take_part, everything())
  return(tibble)
}

# Apply the function to each tibble joined_data
joined_data <- lapply(joined_data, reorder_columns)



### 5 - Join all stages into one dataframe ----

# Joins all stages into one dataframe, whilst maintaining a logical ordering of the order in which questions were asked.
# The ordering is determined by all cols in the first tibble (in order), then all cols in the second tibble that aren't in the first tibble (in
# order), then all cols in the third tibble that aren't in the first two tibbles (in order) etc.

# Function to fill missing columns with "Question not asked of stage"
fill_missing_cols <- function(df, all_cols) {
  missing_cols <- setdiff(all_cols, colnames(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- "Question not asked of stage"
  }
  return(df)
}

# Get all unique column names across all tibbles
all_cols <- unique(unlist(lapply(joined_data, names)))

# Initialize an empty dataframe
joined_dataframe <- tibble()

# Iterate through joined_data to create the final dataframe
for (tbl in joined_data) {
  tbl <- fill_missing_cols(tbl, all_cols)
  joined_dataframe <- bind_rows(joined_dataframe, tbl)
}

# Arrange columns in the desired order
joined_dataframe <- joined_dataframe[, c(names(joined_dataframe)[order(match(names(joined_dataframe), all_cols))])]



### 6 - Apply lookup tables ----

# Replace NAs in columns Gender, EthnicBackground,UrbRur6, asn, SIMD2020v2_Quintile, care_for_someone, long_term_condition with 
# "Unknown" (for ease of applying lookups)
# Replace NA values in specified columns with "Unknown"
cols_to_replace <- c("Gender", "EthnicBackground", "UrbRur6", "asn", "SIMD2020v2_Quintile", "care_for_someone", "long_term_condition")

# Loop through columns and replace NA with "Unknown"
for (col in cols_to_replace) {
  joined_dataframe[[col]][is.na(joined_dataframe[[col]])] <- "Unknown"
}

# Apply lookup table to joined_dataframe to rename variables in column "Gender"
joined_dataframe$Gender <- lut_sex[joined_dataframe$Gender]

# Apply lookup table to joined_dataframe to rename variables in column "EthnicBackground"
joined_dataframe$EthnicBackground <- lut_ethnic_background[joined_dataframe$EthnicBackground]

# Apply lookup table to joined_dataframe to rename variables in column "UrbRur6"
joined_dataframe$UrbRur6 <- lut_urbrur6[joined_dataframe$UrbRur6]

# Apply lookup table to joined_dataframe to rename variables in column "asn"
joined_dataframe$asn <- lut_asn[joined_dataframe$asn]

# Apply lookup table to joined_dataframe to rename variables in column "SIMD2020v2_Quintile"
joined_dataframe$SIMD2020v2_Quintile <- lut_simd[joined_dataframe$SIMD2020v2_Quintile]

# Apply lookup table to joined_dataframe to rename variables in column "care_for_someone"
joined_dataframe$care_for_someone <- lut_care_for_someone[joined_dataframe$care_for_someone]

# Apply lookup table to joined_dataframe to rename variables in column "long_term_condition"
joined_dataframe$long_term_condition <- lut_long_term_condition[joined_dataframe$long_term_condition]



### 7 - Remove characteristics from East Renfrewshire records ----

# If a respondent completed the survey in a local authority that wasn't East Renfrewshire, but that respondent's scn was East Renfrewshire 
# in the pupil census, then for analysis they will be counted as East Renfrewshire. As such, their characteristics must be removed.

# Replace characteristics hwb_scn, school_name and seed_code with "Data not collected" when pc_la = "East Renfrewshire"
joined_dataframe$hwb_scn[joined_dataframe$pc_la == "East Renfrewshire"] <- "Data not collected"
joined_dataframe$school_name[joined_dataframe$pc_la == "East Renfrewshire"] <- "Data not collected"
joined_dataframe$seed_code[joined_dataframe$pc_la == "East Renfrewshire"] <- "Data not collected"

# Replace characteristics Gender, EthnicBackground, UrbRur6, asn, SIMD2020v2_Quintile with "Not known" when pc_la = "East Renfrewshire"
joined_dataframe$Gender[joined_dataframe$pc_la == "East Renfrewshire"] <- "Not known"
joined_dataframe$EthnicBackground[joined_dataframe$pc_la == "East Renfrewshire"] <- "Not known"
joined_dataframe$UrbRur6[joined_dataframe$pc_la == "East Renfrewshire"] <- "Not known"
joined_dataframe$asn[joined_dataframe$pc_la == "East Renfrewshire"] <- "Not known"
joined_dataframe$SIMD2020v2_Quintile[joined_dataframe$pc_la == "East Renfrewshire"] <- "Not known"



### 8 - Save joined_dataframe as an excel file to Merged folder ----

write_xlsx(
  joined_dataframe,
  file.path(raw_data_folder, year, "Merged", paste0("09_joined_stages.xlsx"))
)


### END OF SCRIPT ###
