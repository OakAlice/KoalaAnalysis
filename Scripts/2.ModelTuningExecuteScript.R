# PART TWO: MODEL TUNING EXPERIMENTS
# The larger function for executing the model feature_selected experiments


modelTuning <- function(Save_path, 
                        otherDat, 
                        train_individuals,
                        behaviours, 
                        downsampling_Hz, 
                        window, 
                        overlap, 
                        normalisation,
                        feature_selected,
                        featuresList, 
                        threshold, 
                        trees_number, 
                        folds, 
                        trainingPercentage,
                        stratification) {

  modelOptions <- data.frame()
  
    relabelled_data <- relabel_activities(otherDat, behaviours)
    relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
    num_behs <- length(unique(relabelled_data$activity))
 
    # Process data
    # this part will be parallel processed
    processed_data <- process_data(relabelled_data, featuresList, window, overlap, downsampling_Hz, featureNormalisation)
  
    # balance the data
    balanced_data <- balance_data(processed_data, threshold)
     
    # randomly partition into training and validation data for cross validation
    for (i in 1:folds){ 
      # add stratification later
      partitioned_data <- partition_data(balanced_data, folds, trainingPercentage, stratification)
      trDat <- na.omit(partitioned_data$Training)
      valDat <- na.omit(partitioned_data$Validation)
       
      if (modelArchitecture == "RF"){     
        # Train and validate Random Forest Model
        rf_model <- train_rf_model(trDat, trees_number)
        test_predictions <- predict_rf_model(rf_model, valDat)
      }

      metrics_df <- evaluate_model(test_predictions, valDat, targetBehaviours = NULL)
                
      summary_df <- save_model_results(metrics_df, train_individuals, num_behs, 
        downsampling_Hz, window, overlap, normalisation, feature_selected, featuresList, 
        threshold, trees_number)
                
      modelOptions <- rbind(summary_df, modelOptions)
                
      # iteratively save
      if(!file.exists(summary_file_path)) {
           write.csv(summary_df, summary_file_path, row.names = FALSE)
       } else {
           write.table(summary_df, file = summary_file_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
       }
  
  return(modelOptions)
  }
}

exploreOptions <- function(modelOptions) {
  # clean up to just the stuff I want ( this will change later I'm sure)
  
  # Group by all hyperparameters up to ntree and calculate means for all metrics
  grouped_data <- modelOptions %>%
    group_by(Hz, individuals, number_behaviours, number_features, 
             balanced_threshold, window_length, overlap_percent, feature_normalised,
             feature_selection, ntree) %>%
    summarise(across(everything(), ~mean(as.numeric(., na.rm = TRUE))))
  return(grouped_data)
}



