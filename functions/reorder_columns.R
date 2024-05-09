# Function to reorder columns based on dynamic rules for specific prefixes
reorder_columns <- function(df) {
  # The first and last set of specified columns
  first_cols <- c("Survey topic", "Survey question", "Response")
  last_cols <- c("Prefer not to say", "Not known", "Total %")
  
  # Ensure the specified columns exist in the dataframe
  start_cols <- intersect(first_cols, names(df))
  end_cols <- intersect(last_cols, names(df))
  
  # Find all unique prefixes (e.g., "P5_", "P6_")
  column_prefixes <- unique(sub("(_[^_]+)?$", "_", grep("^[A-Za-z0-9_]+_", names(df), value = TRUE)))
  
  # Function to sort columns for a given prefix
  sort_with_specials_last <- function(cols, prefix) {
    # Identify columns to be moved to the end
    specials <- c(paste0(prefix, "Prefer not to say"), paste0(prefix, "Not known"))
    
    # Separate the special columns and sort the rest alphabetically
    special_cols <- intersect(specials, cols)
    normal_cols <- setdiff(cols, special_cols)
    sorted_cols <- c(sort(normal_cols), special_cols)
    
    return(sorted_cols)
  }
  
  # Create a list to store the reordered columns based on prefixes
  ordered_cols_by_prefix <- list()
  
  # Reorder columns for each unique prefix
  for (prefix in column_prefixes) {
    # Get columns that start with the current prefix
    prefix_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
    # Sort with special rules
    ordered_cols_by_prefix[[prefix]] <- sort_with_specials_last(prefix_cols, prefix)
  }
  
  # Flatten the list of ordered columns
  middle_cols <- unlist(ordered_cols_by_prefix)
  
  # Identify columns without underscores and sort them alphabetically
  other_cols_no_underscores <- sort(setdiff(names(df), c(start_cols, middle_cols, end_cols)))
  
  # Create the final column order
  new_order <- c(start_cols, middle_cols, other_cols_no_underscores, end_cols)
  
  # Reorder the dataframe
  df <- df[, new_order, drop = FALSE]
  
  return(df)
}