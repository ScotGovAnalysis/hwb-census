analysis_wemwbs_stage_and_characteristic <- function(dataset, var) {
  a <- left_join(
    rbind(
      (
        dataset %>%
          select(pc_stage, {{var}}, total_wemwbs) %>%
          filter(!is.na(total_wemwbs)) %>%
          group_by(pc_stage, {{var}}) %>%
          summarise(mean_wemwbs = mean(total_wemwbs)) %>% 
          mutate(mean_wemwbs = round(mean_wemwbs, digits = 1)) %>%
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = 3) %>%
          mutate(`Response` = 'Average WEMWBS score') %>%
          select(`Response`, everything())
      ),
      (
        dataset %>%
          select(pc_stage, {{var}}, total_wemwbs) %>%
          filter(!is.na(total_wemwbs)) %>%
          group_by(pc_stage, {{var}}) %>%
          count() %>%
          pivot_wider(names_from = c(pc_stage, {{var}}), values_from = 3) %>%
          mutate(`Response` = 'Average WEMWBS score') %>%
          select(`Response`, everything()) %>%
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    rbind(
      (
        dataset %>% 
          select({{var}}, total_wemwbs) %>%
          filter(!is.na(total_wemwbs)) %>%
          group_by({{var}}) %>% 
          summarise(mean_wemwbs = mean(total_wemwbs)) %>% 
          mutate(mean_wemwbs = round(mean_wemwbs, digits = 1)) %>%
          pivot_wider(names_from = c({{var}}), values_from = 2) %>% 
          mutate(`Response` = 'Average WEMWBS score') %>%
          select(`Response`, everything())
      ),
      (
        dataset %>% 
          select({{var}}, total_wemwbs) %>%
          filter(!is.na(total_wemwbs)) %>%
          group_by({{var}}) %>% 
          count() %>%
          pivot_wider(names_from = c({{var}}), values_from = 2) %>%
          mutate(`Response` = 'Average WEMWBS score') %>%
          select(`Response`, everything()) %>%
          adorn_totals() %>% 
          slice_tail()
      )
    ),
    by = 'Response'
  )
  
  b <- rbind(
    (dataset %>% 
       filter(!is.na(total_wemwbs)) %>%
       summarise(mean_wemwbs = mean(total_wemwbs)) %>% 
       mutate(mean_wemwbs = round(mean_wemwbs, digits = 1)) %>%
       mutate(`Response` = 'Average WEMWBS score') %>%
       rename(`Total %` = mean_wemwbs) %>% 
       select(`Response`, everything())),
    (dataset %>% 
       filter(!is.na(total_wemwbs)) %>%
       count(total_wemwbs) %>%
       rename(`Response` = total_wemwbs) %>% 
       rename(`Total %` = n) %>% 
       adorn_totals() %>% 
       slice_tail())
  )
  
  c <- bind_cols(a, select(b, -intersect(names(a), names(b))))
  
  d <- cbind(`Survey question` = "Average WEMWBS score", c)
  
  return(d)
}
