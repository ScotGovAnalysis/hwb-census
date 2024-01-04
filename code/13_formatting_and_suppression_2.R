#########################################################################
# Name of file - 13_formatting_and_suppression_2.R
# Health and Wellbeing (HWB) Census 
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.2.2
#
# Description - Formats and suppresses tables for publication
#########################################################################

## CURRENT ISSUES TO BE FIXED
## ".0" not displaying after integers in pct breakdowns
## Total column suppression not working

### 0 - Setup ----

source(here::here("code", "00_setup.R"))
source(here::here("functions", "read_raw_data.R"))



### 1 - Read in data ----

# Read in dataframes for shetland attitudes and store as list
file_path <- here("output", year, "Shetland", paste0(year, "_attitudes_to_school_and_aspirations.xlsx"))

shetland_attitudes_to_school_and_aspirations <- lapply(excel_sheets(file_path), function(sheet) readxl::read_xlsx(file_path, sheet = sheet))
names(shetland_attitudes_to_school_and_aspirations) <- excel_sheets(file_path)

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



### 2 - Format data ----

# Splits data frame into responses only for ease of suppression
# Function to filter numeric columns
filter_numeric_cols <- function(df) {
  df %>% select_if(is.numeric)
}

# Creating shetland_attitudes_to_school_and_aspirations_num list
shetland_attitudes_to_school_and_aspirations_num <- lapply(
  shetland_attitudes_to_school_and_aspirations,
  filter_numeric_cols
)

# Create a list of outputs (which will get overwritten when suppressed) and the original data, which will get 
# used to suppress the outputs
# output_and_data = list(
#   stage = list(outputs = data.matrix(shetland_attitudes_to_school_and_aspirations_num$stage), datas = shetland_attitudes_to_school_and_aspirations$stage),
#   sex = list(outputs = data.matrix(shetland_attitudes_to_school_and_aspirations_num$sex), datas = shetland_attitudes_to_school_and_aspirations$sex)
# )

create_output_and_data <- function(list1, list2) {
  output_and_data <- list()
  
  tibble_names <- names(list1) # Assuming both lists have the same tibble names
  
  for (name in tibble_names) {
    output_and_data[[name]] <- list(
      outputs = data.matrix(list1[[name]]),
      datas = list2[[name]]
    )
  }
  
  return(output_and_data)
}

output_and_data <- create_output_and_data(shetland_attitudes_to_school_and_aspirations_num, shetland_attitudes_to_school_and_aspirations)



### 3 - Perform data suppression ----

# Find indexes for question ends
questions <- shetland_attitudes_to_school_and_aspirations$stage[,1]

indexes <- list()
for(qq in 1:(nrow(questions)-1)){
  if(questions[qq,] != questions[qq+1,]){
    indexes = append(indexes,qq)
  }
}
indexes = append(indexes, nrow(questions))

# Primary and secondary suppression of cells (not including total row)
count <- 0
suppressed_output <- list()
for (x in output_and_data){
  i0 <- 1
  count = count + 1
  for (i1 in indexes){
    
    current_q <- x$datas[i0:i1,3:ncol(x$datas)]
    
    for(col in 1:ncol(current_q)){
      
      if (anyNA(current_q[,col])==FALSE){  #This skips whole columns of current_q if there are any NAs
        
        c_count <- 0
        
        for(row in 1:(nrow(current_q)-1)){
          
          ## primary suppression (suppress values less than x and greater than y)
          if(((current_q[row, col]/100 * current_q[nrow(current_q), col]) < 4.45) & (current_q[row, col]/100 * current_q[nrow(current_q), col] > 0)){
            
            x$outputs[i0+row-1,col] = "[c]"
            
            c_count = c_count + 1
            
          }
        } 
        
        # secondary suppression
        if(c_count == 1 & nrow(current_q) > 2){
          
          min <- Inf 
          
          for(row in 1:(nrow(current_q)-1)){
            
            if (current_q[row,col]<min && x$outputs[i0+row-1,col]!="[c]"){
              min = current_q[row, col]
              next_lowest_row <- row
              
            }
            
          }
          
          x$outputs[i0+next_lowest_row-1,col] = "[c]"
          
        }
        
      }
      
    }
    
    i0 = i1+1
  } 
  suppressed_output <- append(suppressed_output,x)
}


# output_and_data_2 <- list(
#   list(outputs_2 = do.call(rbind, suppressed_output[1]), datas_2 = shetland_attitudes_to_school_and_aspirations$stage),
#   list(outputs_2 = do.call(rbind, suppressed_output[3]), datas_2 = shetland_attitudes_to_school_and_aspirations$sex),
#   list(outputs_2 = do.call(rbind, suppressed_output[5]), datas_2 = shetland_attitudes_to_school_and_aspirations$simd),
#   list(outputs_2 = do.call(rbind, suppressed_output[7]), datas_2 = shetland_attitudes_to_school_and_aspirations$urbrur6),
#   list(outputs_2 = do.call(rbind, suppressed_output[9]), datas_2 = shetland_attitudes_to_school_and_aspirations$ethnic_group),
#   list(outputs_2 = do.call(rbind, suppressed_output[11]), datas_2 = shetland_attitudes_to_school_and_aspirations$care_for_someone),
#   list(outputs_2 = do.call(rbind, suppressed_output[13]), datas_2 = shetland_attitudes_to_school_and_aspirations$long_term_condition)
# )


output_and_data_2 <- list()

# indices_of_interest <- c(1, 3, 5, 7, 9, 11, 13)  # Odd-numbered indices
indices_of_interest <- seq(1, length(suppressed_output), by = 2)  # Generates odd-numbered indices

# tibble_names <- c("stage", "sex", "simd", "urbrur6", "ethnic_group", "care_for_someone", "long_term_condition")
tibble_names <- names(shetland_attitudes_to_school_and_aspirations)


for (i in seq_along(indices_of_interest)) {
  index <- indices_of_interest[i]
  
  if (index <= length(suppressed_output)) {
      outputs_2 <- as.data.frame(suppressed_output[[index]])
      datas_2 <- shetland_attitudes_to_school_and_aspirations[[tibble_names[i]]]
      
      output_and_data_2[[i]] <- list(outputs_2 = outputs_2, datas_2 = datas_2)
  }
}



names(output_and_data_2) <- names(shetland_attitudes_to_school_and_aspirations)



## suppression of total row
count <- 0
suppressed_output_2 <- list()
for (x in output_and_data_2){
  
  ii0 <- 1
  count = count + 1
  for (ii1 in indexes){
    
    current_qq <- x$datas_2[ii0:ii1,3:ncol(x$datas_2)]
    
    for(col in 1:ncol(current_qq)){
      
      if (anyNA(current_qq[,col])==FALSE){  ## skips whole columns of current_qq if there are any NAs
        
        for(row in 1:nrow(current_qq)){
          
          ## suppress totals less than x and greater than y
          
          if((current_qq[nrow(current_qq), col] < 4.45) & (current_qq[nrow(current_qq), col] > 0)){
            
            for(row in 1:(nrow(current_qq) - 1)){ #drop the last row as we don't want to suppress the total
              
              x$outputs_2[ii0+row-1,col] = "[c]"
              
            }
            
          }
          
        }
        
        if(colnames(x$datas_2[,3]) == "P5" & all(current_qq[row, col] == 0)){
          
          x$outputs_2[ii0:ii1,col] = "-"    ## where every cell in a question is a 0, that question wasn't asked, so overwrite each 0 with a "-", only in the stage sheet
        }
        
      }
    }
    
    ii0 = ii1+1
    
  }
  suppressed_output_2 <- append(suppressed_output_2,x)
}




# Joins questions back onto original data
# stage_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$stage[,1:2], suppressed_output_2[1])
# sex_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$sex[,1:2], suppressed_output_2[3])
# simd_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$simd[,1:2], suppressed_output_2[5])
# urbrur6_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$urbrur6[,1:2], suppressed_output_2[7])
# ethnic_group_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$ethnic_group[,1:2], suppressed_output_2[9])
# care_for_someone_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$care_for_someone[,1:2], suppressed_output_2[11])
# long_term_condition_suppressed <- cbind(shetland_attitudes_to_school_and_aspirations$long_term_condition[,1:2], suppressed_output_2[13])
# 
# all_data_suppressed <- list(stage_suppressed, sex_suppressed, simd_suppressed, urbrur6_suppressed,
#                             ethnic_group_suppressed, care_for_someone_suppressed, long_term_condition_suppressed)
# 
# names(all_data_suppressed) <- names(shetland_attitudes_to_school_and_aspirations)
# 


all_data_suppressed <- list()

for (i in seq_along(indices_of_interest)) {
  index <- indices_of_interest[i]
  
  if (index <= length(suppressed_output)) {
      suppressed_data <- suppressed_output[[index]]
      tibble_data <- shetland_attitudes_to_school_and_aspirations[[tibble_names[i]]]
      
      combined_data <- cbind(tibble_data[, 1:2], suppressed_data)
      all_data_suppressed[[i]] <- combined_data
  }
}

names(all_data_suppressed) <- tibble_names




# # Tidy up column names (doesn't seem to be an issue now)
# for ( col in 1:ncol(output_finished)){
#   colnames(output_finished)[col] <-  sub("outputs_2.", "", colnames(output_finished)[col])
# }
# 
# for ( col in 1:ncol(output_finished2)){
#   colnames(output_finished2)[col] <-  sub("outputs_2.", "", colnames(output_finished2)[col])
# }
# 


### 4 - Format data as dataframes ----

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





### 5 - Build workbook ----

# Creates a workbook from all_data_suppressed so we can format our data as Excel spreadsheets
# using the openxlsx package
wb <- buildWorkbook(all_data_suppressed, gridLines = FALSE)

# Create a list of each sheet and it's respective data
sheet_and_data = list(
  list(sheets = 1, data = all_data_suppressed[[1]]),
  list(sheets = 2, data = all_data_suppressed[[2]]),
  list(sheets = 3, data = all_data_suppressed[[3]]),
  list(sheets = 4, data = all_data_suppressed[[4]]),
  list(sheets = 5, data = all_data_suppressed[[5]]),
  list(sheets = 6, data = all_data_suppressed[[6]]),
  list(sheets = 7, data = all_data_suppressed[[7]])
)

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


v <- all_data_suppressed[[1]]


# Merges survey question rows in first column into one cell
# Start at j0 = 1 because in excel the first row is 'Survey question' row, although the first row in out df is data
j0 <- 1

for (j1 in indexes){
  current_qj <- questions[j0:(j1-1),1, drop = FALSE]
  for (x in sheet_and_data){
    mergeCells(wb, x$sheets, 1, rows = (j0+1):(j0+nrow(current_qj)))
  }
  j0 = j1 + 1
}



# 1st j0 = 1, j1 = 6 => rows 2-6 get merged
# 2nd j0 = 7, j1 = 12 => rows 8-12 get merged
# 3rd j0 = 13, j1 = 18 => rows 14-18 get merged
# 4th j0 = 19, j1 = 24 => rows 20-24 get merged
# 5th j0 = 25, j1 = 30 => rows 26-30 get merged
# 6th j0 = 31, j1 = 37 => rows 32 - 37 get merged
# 7th j0 = 38, j1 = 42 => rows 39 - 42 get merged

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




# # Saves the workbook as an excel sheet NOT WORKING
# saveWorkbook(wb,here("output", year, "Shetland", paste0(year, "_attitudes_to_school_and_aspirations_formatted.xlsx")), overwrite = TRUE)
# 

## saves the workbook as an excel sheet
# saveWorkbook(wb,"//s0196a/ADM-Education-NIF Analysis/Health and Wellbeing Survey/R/HWB Analysis/hwb by la/attitudes to school/edited/Shetland - attitudes to school TEST.xlsx", overwrite = TRUE)


saveWorkbook(wb,"C:/Users/u455720/OneDrive - SCOTS Connect/HWB RAP notes/Shetland - attitudes to school TEST.xlsx", overwrite = TRUE)






