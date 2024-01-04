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



### 1 - Read in data ----

# Read in dataframes for national attitudes and store as list
file_path <- here("output", year, "National", paste0(year, "_attitudes_to_school_and_aspirations.xlsx"))

national_attitudes_to_school_and_aspirations <- lapply(excel_sheets(file_path), function(sheet) readxl::read_xlsx(file_path, sheet = sheet))
names(national_attitudes_to_school_and_aspirations) <- excel_sheets(file_path)



### 2 - Perform suppression ----
v <- national_attitudes_to_school_and_aspirations$stage


# Convert columns 3 to end into numeric
v[, 3:ncol(v)] <- sapply(v[, 3:ncol(v)], as.numeric)

# Function to perform data suppression
suppress_values <- function(df) {
  # Apply suppression to columns from the third column onward
  df[, 3:ncol(df)] <- lapply(df[, 3:ncol(df)], function(col) {
    # Replace values meeting the criteria with "[c]" except when 'Response' is 'Total'
    col[df$Response != 'Total' & col < 5 & col > 0] <- "[c]"
    return(col)
  })
  return(df)
}

# Apply data suppression
v_suppressed <- suppress_values(v)

# 'Survey question' = enjoy_learning_new_things and column P5
v <- v %>%
  mutate(P5 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(P5 < 5 & P5 > 0, "[c]", P5), P5))

v <- v %>%
  mutate(P6 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(P6 < 5 & P6 > 0, "[c]", P6), P6))

v <- v %>%
  mutate(P7 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(P7 < 5 & P7 > 0, "[c]", P7), P7))

v <- v %>%
  mutate(S1 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(S1 < 5 & S1 > 0, "[c]", S1), S1))



v <- v %>%
  mutate(
    across(all_stages, ~ ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total",
                                ifelse(. < 5 & . > 0, "[c]", .), .))
  )

v2 <- v %>%
  mutate(
    across(all_stages, ~ {
      temp <- ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total",
                     ifelse(. < 5 & . > 0, "[c]", .), .)
      if ("[c]" %in% temp) {
        c_count <- sum(temp == "[c]", na.rm = TRUE)  # Count [c], handling missing values
        if (c_count == 1) {
          non_c_values <- temp[temp != "[c]"]
          if (length(non_c_values) > 0) {
            min_val <- min(non_c_values, na.rm = TRUE)
            temp[temp == min_val] <- "[c]"
          }
        }
      }
      temp
    })
  )



v3 <- v %>%
  mutate(
    P5 = ifelse(
      `Survey.question` == "teachers_treat_me_fairly" & Response != "Total",
      ifelse(
        P5 >= 0 & P5 <= 5,
        "[c]",
        ifelse(
          sum(P5 == "[c]") == 1,
          ifelse(
            P5 == min(as.numeric(v$P5[v$P5 != "[c]"]), na.rm = TRUE),
            "[c]",
            P5
          ),
          P5
        )
      ),
      P5
    )
  )


v <- data.frame(
  `Survey question` = c("teachers_treat_me_fairly", "teachers_treat_me_fairly", "teachers_treat_me_fairly", "teachers_treat_me_fairly", "teachers_treat_me_fairly"),
  Response = c("Response1", "Response2", "Response3", "Response4", "Total"),
  P5 = c(8, 9, 4, 7, 1)
)





v_primp5 <- v %>%
  mutate(P5 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(P5 < 5 & P5 > 0, "[c]", P5), P5))

v_prims1 <- v %>%
  mutate(S1 = ifelse(`Survey question` == "teachers_treat_me_fairly" & Response != "Total", ifelse(S1 < 5 & S1 > 0, "[c]", S1), S1))




v_secs1 <- v_prims1 %>%
  mutate(
    S1 = ifelse(
      `Survey question` == "teachers_treat_me_fairly" & Response != "Total",
      ifelse(
          sum(S1 == "[c]") == 1,
          ifelse(
            S1 == min(S1[S1 != "[c]"]), "[c]", S1
          ),
          S1
      ),
      S1
    )
  )





## find indexes for question ends
questions <- v[,1]

indexes <- list()

for(qq in 1:(nrow(questions)-1)){
  
  if(questions[qq,] != questions[qq+1,]){
    
    indexes = append(indexes,qq)
    
  }
  
}

indexes = append(indexes,nrow(questions))



## suppression of cells (not including total row)
count <- 0
suppressed_output <- list()

  i0 <- 1
  count = count + 1
  for (i1 in indexes){
    
    current_q <- v[i0:5,3:ncol(v)]
    
    for(col in 1:ncol(current_q)){
      
      if (anyNA(current_q[,col])==FALSE){  #This skips whole columns of current_q if there are any NAs
        
        c_count <- 0
        
        for(row in 1:(nrow(current_q)-1)){
          
          ## primary suppression (suppress values less than x and greater than y)
          if(((current_q[row, col]/100 * current_q[nrow(current_q), col]) < 4.45) & (current_q[row, col]/100 * current_q[nrow(current_q), col] > 0)){
            
            v[i0+row-1,col] = "[c]"
            
            c_count = c_count + 1
            
          }
        } 
        
        # secondary suppression
        # if(c_count == 1 & nrow(current_q) > 2){
        #   
        #   min <- Inf 
        #   
        #   for(row in 1:(nrow(current_q)-1)){
        #     
        #     if (current_q[row,col]<min && x$outputs[i0+row-1,col]!="[c]"){
        #       min = current_q[row, col]
        #       next_lowest_row <- row
        #       
        #     }
        #     
        #   }
        #   
        #   x$outputs[i0+next_lowest_row-1,col] = "[c]"
        #   
        # }
        
      }
      
    }
    
    i0 = i1+1
  } 
  v 






