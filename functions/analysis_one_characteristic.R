# Function for analysing and formatting data by a specific characteristic (variable) in a dataset
analysis_one_characteristic <- function(dataset, var, que, que_quo, cat_order) {
  
  # Get the category vector based on the specified order
  cat_vector <- get(cat_order)
  
  # Step 1: Calculate percentages and format for the specified variable and question
  a <-
    rbind(
      (
        dataset %>% 
          select({{var}}, {{que}}) %>% 
          group_by({{var}}) %>%  
          count({{que}}) %>%  
          pivot_wider(names_from = c({{var}}), values_from = n) %>% 
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_percentages("col") %>% 
          adorn_pct_formatting(digits = 10, rounding = "half to even", affix_sign = FALSE)
      ),
      (
        dataset %>% 
          select({{var}}, {{que}}) %>% 
          group_by({{var}}) %>% 
          count({{que}}) %>%  
          pivot_wider(names_from = c({{var}}), values_from = n) %>% 
          filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>% 
          adorn_totals() %>% 
          slice_tail()
      )
    )
  
  # Step 2: Calculate overall percentages for the specified question
  b <- rbind(
    (
      dataset %>% 
        filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_percentages("col") %>% 
        adorn_pct_formatting(digits = 10, rounding = "half to even", affix_sign = FALSE)
    ),
    (
      dataset %>% 
        filter(!is.na({{que}}) & {{que}} != "Data not collected" & {{que}} != "Not applicable" & {{que}} != "Question not asked of stage") %>%
        count({{que}}) %>%
        adorn_totals() %>% 
        slice_tail())
  ) %>%
    rename(`Total %` = n)
  
  # Step 3: Combine the calculated percentages and totals, and add the survey question as a column
  c <- bind_cols(a, select(b, -intersect(names(a), names(b)))) %>% 
    mutate("Survey question" = que_quo) %>% 
    rename("Response" = que_quo) %>% 
    arrange(match(Response, cat_vector))
  
  # Step 4: Move "Survey question" to the first column position
  d <- c[, c("Survey question", setdiff(names(c), "Survey question"))]
  
  # Step 5: Return the formatted data
  return(d)
}