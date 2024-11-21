# Script for partitioning the data and saving the test and training data


partition_data <- function(balanced_data, folds, training_percentage, stratification){
  
  Partition_data <- balanced_data %>%
    arrange(ID) %>%
    group_by(ID) %>%
    mutate(partition = ceiling(row_number() / n() * folds),
           unique_code = paste0(ID, "_", partition)) # Create unique code
  
  
  training_folds <- floor(training_percentage * folds)
  
  training <- Partition_data %>% 
    group_by(ID) %>%
    filter(partition %in% sample(1:folds, training_folds)) %>%  
    ungroup()
  
  validation <- anti_join(Partition_data, training, by = "unique_code") %>% select(-c("unique_code"))
  
  training <- training %>% select(-c("unique_code"))
  
  return(list(training = training,
              validation = validation))
}
