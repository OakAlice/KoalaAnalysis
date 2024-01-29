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
split_condition <- function(processed_data, modelArchitecture, threshold, split, trainingPercentage, window_length, overlap_percent, test_individuals = NULL) {
  
  dat <- processed_data %>% na.omit()
  #dat <- balance_data(dat, threshold) # balancing currently not working
  
  remove_columns <- function(df) {
    df %>% select(-any_of(c("time", "n", "X", "over_threshold")))
  }
  
  if (split == "random") {
    index <- dat %>% 
      group_by(activity) %>%
      sample_frac(trainingPercentage)
    
    trDat <- remove_columns(index)
    tstDat <- remove_columns(anti_join(dat, trDat))
    
  } else if (split == "chronological") {
      if ("ID" %in% colnames(dat)) {
        id_behavior_split <- dat %>%
          group_by(ID, activity) %>%
          mutate(split_index = floor(trainingPercentage * n()))
        
        # Define a function to slice data based on split_index
        slice_data <- function(df) {
          split_point <- unique(df$split_index[1])
          list(training = df[1:split_point, ], testing = df[(split_point + 1):nrow(df), ])
        }
        
        split_lists <- id_behavior_split %>% 
          group_split(ID, activity) %>%
          map(slice_data)
        
        trDat <- bind_rows(map(split_lists, "training")) %>% remove_columns()
        tstDat <- bind_rows(map(split_lists, "testing")) %>% remove_columns()
        
      } else {
        dat <- dat %>% arrange(time)
        num_rows_to_sample <- floor(nrow(dat) * trainingPercentage)
        
        print(nrow(dat))
        num_rows_to_sample
        
        trDat <- dat[1:num_rows_to_sample, ] %>% remove_columns()
        tstDat <- anti_join(dat, trDat) %>% remove_columns()
      }
      
  } else if (split == "LOIO") {
    # Ensure test_individuals is provided for LOIO split
    if (is.null(test_individuals)) {
      stop("test_individuals must be provided for LOIO split")
    }
    
    number_leave_out <- ceiling((1 - trainingPercentage) * test_individuals)
    selected_individual <- sample(unique(dat$ID), number_leave_out)
    
    tstDat <- dat %>% filter(ID %in% selected_individual) %>% remove_columns()
    trDat <- dat %>% filter(!(ID %in% selected_individual)) %>% remove_columns()
  }
  
  # Formatting the data for the SOM
  trSamp2 <- function(x) {
    # Assuming columns 2 to 21 are the relevant features
    features <- x[,2:21]
    activities <- as.factor(x$activity)
    list(measurements = as.matrix(features), activity = activities)
  }
  
  # Save data based on model architecture
  if (modelArchitecture == "SOM") {
    trDat <- trSamp2(trDat)
    tstDat <- trSamp2(tstDat)
    
    return(list(train = trDat, test = tstDat))
    
    #rda_extension <- ".rda"
    #save(trDat, file = file.path(file_path, splitt, paste0('TrainingData', rda_extension)))
    #save(tstDat, file = file.path(file_path, splitt, paste0('TestingData', rda_extension)))
    
  } else {
    #csv_extension <- ".csv"
    #write_csv(trDat, file.path(file_path, splitt, paste0('TrainingData', csv_extension)))
    #write_csv(tstDat, file.path(file_path, splitt, paste0('TestingData', csv_extension)))
    
    return(list(train = trDat, test = tstDat))
  }
}
