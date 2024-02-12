perform_formatting <- function(all_data_suppressed) {

  # Round numbers to 1 decimal place (except for Total rows which are integers)
  all_data_suppressed <- lapply(all_data_suppressed, function(df) {
    # Identify the columns to round (excluding the first three)
    columns_to_round <- names(df)[-c(1, 2, 3)]
    
    # Loop through each column and apply rounding condition
    for (col in columns_to_round) {
      # Define a function to handle rounding and formatting
      round_values <- function(value, row_totals) {
        if (grepl("^\\d+\\.?\\d*$", value) && !any(row_totals)) {  # Check for numeric pattern and not in total row
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
      for (row in 1:nrow(df)) {
        row_totals <- df[row, ] == "Total"  # Check if any cell in the row is "Total"
        df[[col]][row] <- round_values(df[[col]][row], row_totals)
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
  
  # Function to reset row names and make them consecutive (as the ordering was non-consecutive)
  reset_row_names <- function(df) {
    rownames(df) <- NULL
    return(df)
  }
  
  # Apply the function to each dataframe in the list
  all_data_suppressed <- lapply(all_data_suppressed, reset_row_names)
  
  # Replace values in Survey question to values from survey_question column in metadata_survey_questions_for_publication
  # Function to replace values in 'Survey question' column
  # If no match exists, return 'Survey question' as it was
  replace_survey_question <- function(dataframe, metadata) {
    dataframe %>%
      left_join(metadata, by = c("Survey question" = "code")) %>%
      mutate(`Survey question` = coalesce(survey_question, `Survey question`)) %>%
      select(-survey_question)
  }
  
  # Use map to apply the function to each tibble in all_data_suppressed
  all_data_suppressed <- map(all_data_suppressed, ~replace_survey_question(.x, survey_question_metadata))
 
}
