# PART TWO: MODEL TUNING EXPERIMENTS
# The larger function for executing the model selection experiments


modelTuning <- function(otherDat, relabelledBehaviours, MovementData, downsampling_Hz, window, overlap, featuresList, 
                        balancing_thresholds, folds, trainingPercentage, ntree_list, targetBehaviours, 
                        ExperimentNumber, test_individuals, split) {
  
  modelOptions <- data.frame()
  
  #for (behaviourset in relabelledBehaviours){
    # create the behaviour labels for this round
    behaviourset <- relabelledBehaviours[2]
    behaviours <- MovementData[[behaviourset]]
    relabelled_data <- relabel_activities(otherDat, behaviours)
    relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
       #head(relabelled_data)
    num_behs <- length(unique(relabelled_data$activity))
    
    for (down_Hz in downsampling_Hz){
      for (window_length in window) {
        for (overlap_percent in overlap) {
          # Process data
          # this part will be parallel processed
          processed_data <- process_data(relabelled_data, featuresList, window_length, overlap_percent, down_Hz)
              #head(processed_data)
          
          for (threshold in balancing_thresholds){
            # balance the data
            balanced_data <- balance_data(processed_data, threshold)
                #head(balanced_data)
            
            for (trees_number in ntree_list) {
              
              # randomly partition into training and validation data for cross validation
              for (i in 1:folds){ 
                # add stratification later
                partitioned_data <- partition_data(balanced_data, folds, trainingPercentage, stratification)
                trDat <- na.omit(partitioned_data$Training)
                valDat <- na.omit(partitioned_data$Validation)
                
                # Train and validate Random Forest Model
                rf_model <- train_rf_model(trDat, trees_number)
                test_predictions <- predict_rf_model(rf_model, valDat)
                metrics_df <- evaluate_rf_model(test_predictions, valDat, targetBehaviours)
                
                
                summary_df <- save_rf_model(
                  rf_model, metrics_df, ExperimentNumber, 
                  down_Hz, num_behs, featuresList, threshold, window_length, 
                  overlap_percent, splitMethod, trees_number)
                
                modelOptions <- rbind(summary_df, modelOptions)
              }
            } 
          }
        }
      #}
    }
  }
  return(modelOptions)
}


exploreOptions <- function(modelOptions) {
  # clean up to just the stuff I want ( this will change later I'm sure)
  modelOptions <- modelOptions %>% select(-NumVariablesSplit)
  
  # Group by all hyperparameters up to ntree and calculate means for all metrics
  grouped_data <- modelOptions %>%
    group_by(ExperimentNumber, DesiredHz, numBehaviours, NumFeatures, Balancing, WindowLength, OverlapPercent, SplitMethod, ntree) %>%
    summarise(across(everything(), ~mean(as.numeric(., na.rm = TRUE))))
  return(grouped_data)
}



