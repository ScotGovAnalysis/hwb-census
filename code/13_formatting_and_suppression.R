#########################################################################
# Name of file - 13_formatting_and_suppression.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication
#########################################################################



### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))
source(here::here("functions", "perform_data_suppression.R"))


### 1 - Read in data ----

# Read in dataframes for shetland attitudes and store as list
file_path <- here("output", year, "Shetland", paste0(year, "_attitudes_to_school_and_aspirations.xlsx"))

shetland_attitudes_to_school_and_aspirations <- setNames(
  lapply(excel_sheets(file_path), function(sheet) readxl::read_xlsx(file_path, sheet = sheet)),
  excel_sheets(file_path)
)

# Convert columns 3 to end to numeric
# Function to convert columns 3 to end in each tibble to numeric
convert_to_numeric <- function(tibble_list) {
  updated_list <- lapply(tibble_list, function(tibble_item) {
    # Extract column names from the third column to the end
    cols_to_convert <- names(tibble_item)[3:ncol(tibble_item)]
    
    # Convert selected columns to numeric
    tibble_item[cols_to_convert] <- lapply(tibble_item[cols_to_convert], as.numeric)
    
    return(tibble_item)
  })
  return(updated_list)
}

# Applying the function to convert columns 3 to end in each tibble to numeric
shetland_attitudes_to_school_and_aspirations <- convert_to_numeric(shetland_attitudes_to_school_and_aspirations)

# Replace all NA with 0
# Function to replace NAs with 0 in a tibble
replace_na_with_zero <- function(df) {
  df[is.na(df)] <- 0
  return(df)
}

# Using map to apply the function to each tibble in the list
shetland_attitudes_to_school_and_aspirations <- map(shetland_attitudes_to_school_and_aspirations, ~ replace_na_with_zero(.))



### 2 - Perform data suppression ---

all_data_suppressed <- perform_data_suppression(shetland_attitudes_to_school_and_aspirations)



### 3 - Format data as dataframes ----

# Round numbers to 1 decimal place (except for Total rows which are integers)
all_data_suppressed <- lapply(all_data_suppressed, function(df) {
  # Identify the columns to round (excluding the first two)
  columns_to_round <- names(df)[-c(1, 2)]
  
  # Loop through each column and apply rounding condition
  for (col in columns_to_round) {
    # Check if the column is not 'Response = Total'
    if (col != "Response" && col != "Total") {
      # Define a function to handle rounding and formatting
      round_values <- function(value) {
        if (grepl("^\\d+\\.?\\d*$", value)) {  # Check for numeric pattern
          rounded_value <- round(as.numeric(value), 1)  # Round numeric values
          if (as.integer(rounded_value) == rounded_value) {
            return(sprintf("%.1f", rounded_value))  # Format whole numbers with decimal point and zero
          } else {
            return(as.character(rounded_value))  # Convert rounded numeric values to character
          }
        } else {
          return(value)  # Keep non-numeric or suppression indicator values unchanged
        }
      }
      
      # Apply the rounding and formatting function to the column
      df[[col]] <- sapply(df[[col]], round_values)
    }
  }
  
  return(df)
})

# Rename rows from "Total" to "Number of respondents" in the Response column
for (i in seq_along(all_data_suppressed)) {
  all_data_suppressed[[i]]$Response[all_data_suppressed[[i]]$Response == "Total"] <- "Number of respondents"
}

# Rename column "Total" to "Total %"
for (i in seq_along(all_data_suppressed)) {
  if ("Total" %in% colnames(all_data_suppressed[[i]])) {
    colnames(all_data_suppressed[[i]])[colnames(all_data_suppressed[[i]]) == "Total"] <- "Total %"
  }
}

# Add a blank row after every "Number of respondents" row (except for the last one)
# Function to add a blank row after "Number of respondents" in each dataframe
add_blank_rows <- function(df) {
  row_indices <- which(df$Response == "Number of respondents")
  
  if (length(row_indices) > 1) {
    for (index in rev(row_indices[-length(row_indices)])) {
      blank_row <- rep(NA, ncol(df))
      df <- rbind(
        df[1:index, , drop = FALSE],
        blank_row,
        df[(index + 1):nrow(df), , drop = FALSE]
      )
    }
  } 
  return(df)
}

# Apply the function to each dataframe in the list
all_data_suppressed <- lapply(all_data_suppressed, add_blank_rows)

# Function to reset row names and make them consecutive (as the ordering was non-consecutive)
reset_row_names <- function(df) {
  rownames(df) <- NULL
  return(df)
}

# Apply the function to each dataframe in the list
all_data_suppressed <- lapply(all_data_suppressed, reset_row_names)



### 4 - Build workbook ----

# Creates a workbook from all_data_suppressed so we can format our data as Excel spreadsheets using the openxlsx package
wb <- buildWorkbook(all_data_suppressed, gridLines = FALSE)

# Create a list of each sheet and it's respective data
sheet_and_data <- lapply(seq_along(all_data_suppressed), function(i) {
  list(sheets = i, data = all_data_suppressed[[i]])
})

# Convert numbers stored as text to numbers
for (x in sheet_and_data){
  for (cn in seq_len(ncol(x$data))) {
    for (rn in seq_len(nrow(x$data))) {
      if (!is.numeric(x$data[rn,cn]) && !is.na(val <- as.numeric(as.character(x$data[rn,cn])))) {
        writeData(wb, x$sheets, val, startCol = cn, startRow = 1L + rn)
      }
    }
  }
}


# Find indexes for question ends
questions <- all_data_suppressed[[1]][, 1, drop = FALSE]

indexes <- list()

for(qq in 1:(nrow(questions))){
  if(is.na(questions[qq,] != questions[qq+1,]) & is.na(questions[qq,])){
    indexes = append(indexes,qq)
  }
}

indexes = append(indexes,nrow(questions) + 1)

# Merges survey question rows in first column into one cell
# Start at j0 = 1 because in excel the first row is 'Survey question' row, although the first row in our df is data
j0 <- 1

for (j1 in indexes){
  current_qj <- questions[j0:(j1-1),1, drop = FALSE]
  for (x in sheet_and_data){
    mergeCells(wb, x$sheets, 1, rows = (j0+1):(j0+nrow(current_qj)))
  }
  j0 = j1 + 1
}

# Changes the font of all cells in the spreadsheet to be Arial and size 12
fontStyle <- createStyle(fontName = "Arial", fontSize = 12)

for (x in sheet_and_data){
  addStyle(wb, x$sheets, fontStyle, cols = 1:ncol(x$data), rows = 1:(nrow(x$data)+1), stack = TRUE, gridExpand = TRUE)
}


# Formats survey questions in column A to be centered (stack = TRUE means that it won't overwrite the formatting in previous createStyles)
centerStyle <- createStyle(halign = "center", valign = "center", wrapText = TRUE)

for (x in sheet_and_data){
  addStyle(wb, x$sheets, centerStyle, cols = 1, rows = 2:nrow(x$data), stack = TRUE, gridExpand = TRUE)
}


# Makes font bold
boldStyle <- createStyle(textDecoration = "bold")

# Makes header row bold
for (x in sheet_and_data){
  addStyle(wb, x$sheets, boldStyle, cols = 1:ncol(x$data), rows = 1, stack = TRUE, gridExpand = TRUE)
}


# Makes "Number of respondents" rows bold
for (x in sheet_and_data){
  k0 <- 1
  for (k1 in indexes){
    current_qk <- x$data[k0:k1, 2:ncol(x$data)]
    for(col in 1:ncol(current_qk)){
      for(row in nrow(current_qk)){
        addStyle(wb, x$sheets, boldStyle, cols = 2:(ncol(current_qk)+1), rows = (k0+nrow(current_qk)-1), gridExpand = TRUE, stack = TRUE)
      }
    }
    k0 = k1+1
  }
}


# Adds thick bottom border to the first row
thickborderStyle <- createStyle(border = "bottom", borderStyle = openxlsx_getOp("borderStyle", "thick"))

for (x in sheet_and_data){
  addStyle(wb, x$sheets, thickborderStyle, cols = 1:ncol(x$data), rows = 1, stack = TRUE, gridExpand = TRUE)
}


# Aligns text to the right
alignrightStyle <- createStyle(halign = "right")

for (x in sheet_and_data){
  addStyle(wb, x$sheets, alignrightStyle, cols = 3:ncol(x$data), rows = 1:(nrow(x$data)+1), stack = TRUE, gridExpand = TRUE)
}


# Displays percentages to one decimal place and puts totals in integer format, with a comma for thousands
decimalStyle <- createStyle(numFmt = '0.0')

for (x in sheet_and_data){
  addStyle(wb, x$sheets, decimalStyle, cols = 3:ncol(x$data), rows = 2:nrow(x$data), stack = TRUE, gridExpand = TRUE)
}


thousandsStyle <- createStyle(numFmt = 'COMMA')

for (x in sheet_and_data){
  l0 <- 1
  for (l1 in indexes){
    current_ql <- x$data[l0:l1,3:ncol(x$data)]
    for(col in 1:ncol(current_ql)){
      for(row in nrow(current_ql)){
        addStyle(wb, x$sheets, thousandsStyle, cols = 3:(ncol(current_ql)+2), rows = (l0+nrow(current_ql)-1), gridExpand = TRUE, stack = TRUE)
      }
    }
    l0 = l1+1
  }
}

# Auto-fit column widths
# Loop through each data frame in the list
for (x in sheet_and_data) {
  
  # Set column widths based on the content
  for (col in 1:ncol(x$data)) {
    max_width <- max(nchar(paste(names(x$data)[col], x$data[, col], sep = "")))  # Find the maximum width in characters
    setColWidths(wb, x$sheets, cols = col, widths = max_width + 5)  # Add some padding
  }
}



### 5 - Save outputs as an excel file to Merged folder ----

# Saves the workbook as an excel sheet 
saveWorkbook(wb,here("output", year, "Shetland", paste0(year, "_attitudes_to_school_and_aspirations_formatted.xlsx")), overwrite = TRUE)



### END OF SCRIPT ###


# To do
# rename survey questions - might be easiest to collapse all column metadata into one file or make a master file?. For now I think read in all metadata and collapse
# into one dataframe of just all the columns we need
# re-order columns. prob easiest to set prefer not to say and not known last rather than list them all
# add in columns if blank e.g. simd 1 in shetland

