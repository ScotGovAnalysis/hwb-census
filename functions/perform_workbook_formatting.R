# perform_workbook_formatting <- function(wb, formatted_data) {
#   
#   # Create a list of each sheet and it's respective data
#   sheet_and_data <- lapply(seq_along(formatted_data), function(i) {
#     list(sheets = i, data = formatted_data[[i]])
#   })
#   
#   # Convert numbers stored as text to numbers
#   for (x in sheet_and_data){
#     for (cn in seq_len(ncol(x$data))) {
#       for (rn in seq_len(nrow(x$data))) {
#         if (!is.numeric(x$data[rn,cn]) && !is.na(val <- as.numeric(as.character(x$data[rn,cn])))) {
#           writeData(wb, x$sheets, val, startCol = cn, startRow = 1L + rn)
#         }
#       }
#     }
#   }
#   
#   
#   # Find indexes for question ends
#   questions <- formatted_data[[1]][, 1, drop = FALSE]
#   
#   indexes <- list()
#   
#   for(qq in 1:(nrow(questions))){
#     if(is.na(questions[qq,] != questions[qq+1,]) & is.na(questions[qq,])){
#       indexes = append(indexes,qq)
#     }
#   }
#   
#   indexes = append(indexes,nrow(questions) + 1)
#   
#   # Merges survey question rows in first column into one cell
#   # Start at j0 = 1 because in excel the first row is 'Survey question' row, although the first row in our df is data
#   j0 <- 1
#   
#   for (j1 in indexes){
#     current_qj <- questions[j0:(j1-1),1, drop = FALSE]
#     for (x in sheet_and_data){
#       mergeCells(wb, x$sheets, 1, rows = (j0+1):(j0+nrow(current_qj)))
#     }
#     j0 = j1 + 1
#   }
#   
#   # Changes the font of all cells in the spreadsheet to be Arial and size 12
#   fontStyle <- createStyle(fontName = "Arial", fontSize = 12)
#   
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, fontStyle, cols = 1:ncol(x$data), rows = 1:(nrow(x$data)+1), stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   # Formats survey questions in column A to be centered (stack = TRUE means that it won't overwrite the formatting in previous createStyles)
#   centerStyle <- createStyle(halign = "center", valign = "center", wrapText = TRUE)
#   
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, centerStyle, cols = 1, rows = 2:nrow(x$data), stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   # Makes font bold
#   boldStyle <- createStyle(textDecoration = "bold")
#   
#   # Makes header row bold
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, boldStyle, cols = 1:ncol(x$data), rows = 1, stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   # Makes "Number of respondents" rows bold
#   for (x in sheet_and_data){
#     k0 <- 1
#     for (k1 in indexes){
#       current_qk <- x$data[k0:k1, 2:ncol(x$data)]
#       for(col in 1:ncol(current_qk)){
#         for(row in nrow(current_qk)){
#           addStyle(wb, x$sheets, boldStyle, cols = 2:(ncol(current_qk)+1), rows = (k0+nrow(current_qk)-1), gridExpand = TRUE, stack = TRUE)
#         }
#       }
#       k0 = k1+1
#     }
#   }
#   
#   
#   # Adds thick bottom border to the first row
#   thickborderStyle <- createStyle(border = "bottom", borderStyle = openxlsx_getOp("borderStyle", "thick"))
#   
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, thickborderStyle, cols = 1:ncol(x$data), rows = 1, stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   # Aligns text to the right
#   alignrightStyle <- createStyle(halign = "right")
#   
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, alignrightStyle, cols = 3:ncol(x$data), rows = 1:(nrow(x$data)+1), stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   # Displays percentages to one decimal place and puts totals in integer format, with a comma for thousands
#   decimalStyle <- createStyle(numFmt = '0.0')
#   
#   for (x in sheet_and_data){
#     addStyle(wb, x$sheets, decimalStyle, cols = 3:ncol(x$data), rows = 2:nrow(x$data), stack = TRUE, gridExpand = TRUE)
#   }
#   
#   
#   thousandsStyle <- createStyle(numFmt = 'COMMA')
#   
#   for (x in sheet_and_data){
#     l0 <- 1
#     for (l1 in indexes){
#       current_ql <- x$data[l0:l1,3:ncol(x$data)]
#       for(col in 1:ncol(current_ql)){
#         for(row in nrow(current_ql)){
#           addStyle(wb, x$sheets, thousandsStyle, cols = 3:(ncol(current_ql)+2), rows = (l0+nrow(current_ql)-1), gridExpand = TRUE, stack = TRUE)
#         }
#       }
#       l0 = l1+1
#     }
#   }
#   
#   # Auto-fit column widths
#   # Loop through each data frame in the list
#   for (x in sheet_and_data) {
#     
#     # Set column widths based on the content
#     for (col in 1:ncol(x$data)) {
#       max_width <- max(nchar(paste(names(x$data)[col], x$data[, col], sep = "")))  # Find the maximum width in characters
#       setColWidths(wb, x$sheets, cols = col, widths = max_width + 5)  # Add some padding
#     }
#   }
#   
#   return(wb)
#   
#   }

# 
# formatted_data <- formatted_data$Angus
# wb <- buildWorkbook(formatted_data, gridLines = FALSE)

perform_workbook_formatting <- function(wb, formatted_data) {

  # Create a list of each sheet and it's respective data
  sheet_and_data <- lapply(seq_along(formatted_data), function(i) {
    list(sheets = i, data = formatted_data[[i]])
  })

  # Convert numbers stored as text to numbers
  for (x in sheet_and_data) {
    for (cn in seq_len(ncol(x$data))) {
      col_data <- x$data[, cn]
      non_numeric_indices <- !is.na(as.numeric(col_data)) & !is.numeric(col_data)
      if (any(non_numeric_indices)) {
        col_data[non_numeric_indices] <- as.numeric(as.character(col_data[non_numeric_indices]))
        writeData(wb, x$sheets, col_data[non_numeric_indices], startCol = cn, startRow = which(non_numeric_indices) + 1L)
      }
    }
  }


  # Find indexes for question ends
  questions <- formatted_data[[1]][, 2, drop = FALSE]

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

  return(wb)

  }
