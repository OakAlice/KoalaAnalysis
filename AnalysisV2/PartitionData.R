# Script for partitioning the data and saving the test and training data


partition_data <- function(balanced_data, folds, trainingPercentage, stratification){
  
  Partition_data <- balanced_data %>%
    arrange(ID) %>%
    group_by(ID) %>%
    mutate(partition = ceiling(row_number() / n() * folds),
           unique_code = paste0(ID, "_", partition)) # Create unique code
  
  
  training_folds <- floor(trainingPercentage * folds)
  
  Training <- Partition_data %>% 
    group_by(ID) %>%
    filter(partition %in% sample(1:folds, training_folds)) %>%  
    ungroup()
  
  Validation <- anti_join(Partition_data, Training, by = "unique_code") %>% select(-c("unique_code"))
  
  Training <- Training %>% select(-c("unique_code"))
  
  return(list(Training = Training,
              Validation = Validation))
}
