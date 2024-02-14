# balancing data

# balance the data according to the above determined value
balance_data <- function(dat, threshold) {
  #dat <- processed_data
  
  # Determine counts of each 'activity' and identify over-represented behaviors
  activity_counts <- dat %>% 
    group_by(activity) %>%
    tally() %>%
    mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
  
  # For over-represented behaviors, sample the desired threshold number of rows or all if less
  oversampled_data <- dat %>% 
    inner_join(activity_counts %>% filter(n > threshold), by = "activity") %>%
    group_by(activity) %>%
    sample_n(size = min(over_threshold[1], n()), replace = FALSE) 
  
  # For other behaviors, take all rows
  undersampled_data <- dat %>% 
    anti_join(filter(activity_counts, n > threshold), by = "activity")
  
  # Combine and return
  balance_data <- bind_rows(oversampled_data, undersampled_data)
  return(balance_data)
}
