# balancing data

# balance the data according to the above determined value
balance_data <- function(dat, threshold) {
  #dat <- processed_data
  
  # Determine counts of each 'activity' and identify over-represented behaviors
  dat_counts <- dat %>%
    count(activity)
  
  # For over-represented behaviors, sample the desired threshold number of rows or all if less
  dat_selected <- dat %>%
    group_by(activity, ID) %>%
    mutate(row_number = row_number()) %>%
    ungroup() %>%
    inner_join(dat_counts, by = "activity") %>%
    mutate(max_rows = if_else(n > threshold, threshold, n)) %>%
    filter(row_number <= max_rows) %>%
    select(-row_number, -n)
  
  # Combine and return
  balance_data <- dat_selected
  return(balance_data)
}


balance_ID_data <- function(dat, threshold) {
  #dat <- processed_data
  
  # Determine counts of each 'activity' and identify over-represented behaviors
  activity_counts <- dat %>% 
    group_by(activity) %>%
    tally() %>%
    mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
  
  # Calculate average number of samples per individual for each behavior
  avg_samples_per_individual <- dat %>%
    group_by(activity, ID) %>%
    tally() %>%
    group_by(activity) %>%
    summarize(avg_samples = mean(n))
  
  # For over-represented behaviors, keep the first threshold number of rows for each individual
  oversampled_data <- dat %>% 
    inner_join(activity_counts %>% filter(n > threshold), by = "activity") %>%
    inner_join(avg_samples_per_individual, by = "activity") %>%
    group_by(activity, ID) %>%
    arrange(ID, desc(avg_samples), .by_group = TRUE) %>%
    mutate(row_num = row_number()) %>%
    filter(row_num <= over_threshold[1]) %>%
    select(-avg_samples)
  
  # For other behaviors, take all rows
  undersampled_data <- dat %>% 
    anti_join(filter(activity_counts, n > threshold), by = "activity")
  
  # Combine and return
  balance_data <- bind_rows(oversampled_data, undersampled_data)
  return(balance_data)
}

