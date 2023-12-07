analysis_wemwbs_one_characteristic <- function(dataset, var) {
  
  a <-
    rbind(
      (
        dataset %>% 
          select({{var}}, total_wemwbs) %>% 
          filter(!is.na(total_wemwbs)) %>%
          group_by({{var}}) %>%  
          summarise(mean_wemwbs = mean(total_wemwbs)) %>% 
          mutate(mean_wemwbs = round(mean_wemwbs, digits = 1)) %>%
          pivot_wider(names_from = c({{var}}), values_from = 2) %>% 
          mutate(`Survey question` = 'Average WEMWBS score') %>%
          select(`Survey question`, everything())
      ),
      (
        dataset %>% 
          select({{var}}, total_wemwbs) %>% 
          filter(!is.na(total_wemwbs)) %>%
          group_by({{var}}) %>% 
          count() %>%  
          pivot_wider(names_from = c({{var}}), values_from = 2) %>% 
          mutate(`Survey question` = 'Average WEMWBS score') %>%
          select(`Survey question`, everything()) %>%
          adorn_totals() %>% 
          slice_tail()
      )
    )
  
  b <- rbind(
    (
      dataset %>% 
        filter(!is.na(total_wemwbs)) %>%
        summarise(mean_wemwbs = mean(total_wemwbs)) %>% 
        mutate(mean_wemwbs = round(mean_wemwbs, digits = 1)) %>%
        mutate(`Survey question` = 'Average WEMWBS score') %>%
        rename(`Total %` = mean_wemwbs) %>% 
        select(`Survey question`, everything())
    ),
    (
      dataset %>% 
        filter(!is.na(total_wemwbs)) %>%
        count(total_wemwbs) %>%
        rename(`Survey question` = total_wemwbs) %>% 
        rename(`Total %` = n) %>% 
        adorn_totals() %>% 
        slice_tail())
  )
  
  c <- bind_cols(a, select(b, -intersect(names(a), names(b))))
  
  return(c)
}