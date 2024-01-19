perform_data_suppression <- function(original_data) {
  # Function to filter numeric columns
  filter_numeric_cols <- function(df) {
    df %>% select_if(is.numeric)
  }
  
  # Creating a list of numeric data frames
  numeric_data_list <- lapply(original_data, filter_numeric_cols)
  
  # Function to create output and data
  create_output_and_data <- function(list1, list2) {
    output_and_data <- list()
    
    tibble_names <- names(list1)
    
    for (name in tibble_names) {
      output_and_data[[name]] <- list(
        outputs = data.matrix(list1[[name]]),
        datas = list2[[name]]
      )
    }
    
    return(output_and_data)
  }
  
  # Create output_and_data list
  output_and_data <- create_output_and_data(numeric_data_list, original_data)
  
  # Find indexes for question ends
  questions <- original_data$stage[, 1]
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
        
        if (anyNA(current_q[,col])==FALSE){  # This skips whole columns of current_q if there are any NAs
          
          c_count <- 0
          
          for(row in 1:(nrow(current_q)-1)){
            
            # Primary suppression (suppress values less than x and greater than y)
            if(((current_q[row, col]/100 * current_q[nrow(current_q), col]) < 5) & (current_q[row, col]/100 * current_q[nrow(current_q), col] > 0)){
              
              x$outputs[i0+row-1,col] = "[c]"
              
              c_count = c_count + 1
              
            }
          }
          
          # Secondary suppression
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
  
  output_and_data_2 <- list()
  
  # Generates odd-numbered indices
  indices_of_interest <- seq(1, length(suppressed_output), by = 2)
  
  # Get tibble names
  tibble_names <- names(original_data)
  
  
  for (i in seq_along(indices_of_interest)) {
    index <- indices_of_interest[i]
    
    if (index <= length(suppressed_output)) {
      outputs_2 <- as.data.frame(suppressed_output[[index]])
      datas_2 <- original_data[[tibble_names[i]]]
      
      output_and_data_2[[i]] <- list(outputs_2 = outputs_2, datas_2 = datas_2)
    }
  }
  
  
  
  names(output_and_data_2) <- names(original_data)
  
  
  
  # Suppression of total row
  count <- 0
  suppressed_output_2 <- list()
  for (x in output_and_data_2){
    
    ii0 <- 1
    count = count + 1
    for (ii1 in indexes){
      
      current_qq <- x$datas_2[ii0:ii1,3:ncol(x$datas_2)]
      
      for(col in 1:ncol(current_qq)){
        
        if (anyNA(current_qq[,col])==FALSE){  # Skips whole columns of current_qq if there are any NAs
          
          for(row in 1:nrow(current_qq)){
            
            # Suppress totals less than x and greater than y
            
            if((current_qq[nrow(current_qq), col] < 5) & (current_qq[nrow(current_qq), col] > 0)){
              
              for(row in 1:(nrow(current_qq) - 1)){ # Drop the last row as we don't want to suppress the total
                
                x$outputs_2[ii0+row-1,col] = "[c]"
                
              }
              
            }
            
          }
          
        }
      }
      
      ii0 = ii1+1
      
    }
    suppressed_output_2 <- append(suppressed_output_2,x)
  }
  
  # Joins questions back onto original data
  all_data_suppressed <- list()
  
  for (i in seq_along(indices_of_interest)) {
    index <- indices_of_interest[i]
    
    if (index <= length(suppressed_output_2)) {
      suppressed_data <- suppressed_output_2[[index]]
      tibble_data <- original_data[[tibble_names[i]]]
      
      combined_data <- cbind(tibble_data[, 1:2], suppressed_data)
      all_data_suppressed[[i]] <- combined_data
    }
  }
  
  names(all_data_suppressed) <- tibble_names
  
  return(all_data_suppressed)
}