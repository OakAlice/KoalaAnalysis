# Code to create the training and testing data, saving them both as .rda files

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

# process the data
split_condition <- function(file_path, modelArchitecture, threshold, split, trainingPercentage) {
  
  dat <- read.csv(file_path)
  dat <- na.omit(dat)
  
  # Balance the data
  dat <- balance_data(dat, threshold)
  
  # Split data by different conditions
  if (split == "random") {
    
    # if split is random, select randomly based on the specified trainingPercentage
    ind <- dat %>% 
      group_by(activity) %>%
      sample_frac(trainingPercentage)
    
    trDat <- ind
    # remove the first and last 2 columns
    trDat <- trDat %>%
      select(-1, -ncol(trDat), -(ncol(trDat)-1))
    
    tstDat <- anti_join(dat, ind, by = "X")
    # remove the first and last 2 columns
    tstDat <- tstDat %>%
      select(-1, -ncol(tstDat), -(ncol(tstDat)-1))
    
  } else if (split == "chronological") { 
    
    # Group by ID and behavior, take the first % as the training 
    # and calculate the split index for each ID-behavior combination
    
    # if there is an ID column:
    id_behavior_split <- dat %>%
      group_by(ID, activity) %>%
      mutate(split_index = floor(trainingPercentage * n()))
    
    # Split data into training and testing based on the calculated split index for each ID-behavior combination
    train_data_list <- id_behavior_split %>%
      group_by(ID, activity) %>%
      group_split() %>%
      lapply(function(.x) .x[1:unique(.x$split_index[1]), ])
    
    test_data_list <- id_behavior_split %>%
      group_by(ID, activity) %>%
      group_split() %>%
      lapply(function(.x) .x[(unique(.x$split_index[1]) + 1):nrow(.x), ])
    
    # Combine all the training and testing data
    trDat <- bind_rows(train_data_list)
    tstDat <- bind_rows(test_data_list)
    
    # if there is not an ID column, 
    # list column 'time' chronologically and then take the percentages from there
    
  } else if (split == "LOIO") {
    
    number_leave_out <- ceiling((1-trainingPercentage)*test_individuals)
    
    # Sample a random individual from the dataset
    unique_IDs <- unique(dat$ID)
    selected_individual <- sample(unique_IDs, number_leave_out)
    #selected_individual <- 20
    
    tstDat <- dat %>% filter(ID %in% selected_individual)
    
    trDat <- subset(dat, !(ID %in% selected_individual))
    
  }
  
  # Formatting the data for the SOM
  trSamp2 <- function(x) { 
    d <- x[,2:21] # TODO: why/how is this hardcoded. ####
    activity <- as.factor(x$activity) # Corresponding activities
    out <- list(measurements = as.matrix(d), activity = activity)
    return(out)
  }
  
  # Default extension for other than SOM
  csv_extension <- ".csv"
  
  # Apply trSamp2 to the data for the SOM and adjust the file extension. Otherwise, leave as normal
  if (modelArchitecture == "SOM") {
    trDat <- trSamp2(trDat)
    tstDat <- trSamp2(tstDat)
    rda_extension <- ".rda"
    
    # Save the training data as .rda file
    training_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                                    paste0(overlap_percent, "%_overlap"), split, 
                                    paste0('TrainingData', rda_extension))
    save(trDat, file = training_file_path)
    
    # Save the testing data as .rda file
    testing_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                                   paste0(overlap_percent, "%_overlap"), split, 
                                   paste0('TestingData', rda_extension))
    save(tstDat, file = testing_file_path)
    
  } else {
    # Save the training data as .csv file
    training_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                                    paste0(overlap_percent, "%_overlap"), split, 
                                    paste0('TrainingData', csv_extension))
    write_csv(trDat, training_file_path)
    
    # Save the testing data as .csv file
    testing_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                                   paste0(overlap_percent, "%_overlap"), split, 
                                   paste0('TestingData', csv_extension))
    write_csv(tstDat, testing_file_path)
  }
  
}