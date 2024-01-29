perform_formatting <- function(all_data_suppressed) {

  # Round numbers to 1 decimal place (except for Total rows which are integers)
  all_data_suppressed <- lapply(all_data_suppressed, function(df) {
    # Identify the columns to round (excluding the first three)
    columns_to_round <- names(df)[-c(1, 2, 3)]
    
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
  
  # Replace values in Survey question to values in question_header2 from metadata_headers
  # Function to replace values in 'Survey question' column
  # If no match exists, return 'Survey question' as it was
  replace_survey_question <- function(dataframe, metadata) {
    dataframe %>%
      left_join(metadata, by = c("Survey question" = "code")) %>%
      mutate(`Survey question` = coalesce(question_header2, `Survey question`)) %>%
      select(-question_header2)
  }
  
  # Use map to apply the function to each tibble in all_data_suppressed
  all_data_suppressed <- map(all_data_suppressed, ~replace_survey_question(.x, metadata_headers))

}
