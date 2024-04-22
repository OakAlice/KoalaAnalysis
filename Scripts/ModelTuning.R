# PART TWO: MODEL TUNING EXPERIMENTS
# The larger function for executing the model feature_selection experiments


model_tuning <- function(summary_file_path, 
                        otherDat, 
                        num_individuals,
                        behaviours, 
                        down_Hz, 
                        window, 
                        overlap, 
                        feature_normalisation,
                        feature_selection,
                        features_list, 
                        threshold, 
                        ntrees, 
                        model_architecture,
                        folds, 
                        training_percentage,
                        split_stratified) {
  
  model_options <- data.frame()
  
    relabelled_data <- relabel_activities(otherDat, behaviours)
    relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
    num_behs <- length(unique(relabelled_data$activity))
 
    # Process data
    # this part will be parallel processed
    processed_data <- process_data(relabelled_data, features_list, window, overlap, down_Hz, feature_normalisation)
  
    # balance the data
    balanced_data <- balance_data(processed_data, threshold)
     
    # randomly partition into training and validation data for cross validation
    for (i in 1:folds){ 
      # add split_stratified later
      partitioned_data <- partition_data(balanced_data, folds, training_percentage, split_stratified)
      trDat <- na.omit(partitioned_data$training)
      valDat <- na.omit(partitioned_data$validation)
       
      model <- train_model(trDat, valDat)
      test_predictions <- predict_model(rf_model, valDat)

      metrics_df <- evaluate_model(test_predictions, valDat, target_behaviours = NULL)
                
      summary_df <- save_model_results(metrics_df, num_individuals, num_behs, 
        down_Hz, window, overlap, feature_normalisation, feature_selection, features_list, 
        threshold, ntrees)
                
      model_options <- rbind(summary_df, model_options)
                
      # iteratively save
      if(!file.exists(summary_file_path)) {
           write.csv(summary_df, summary_file_path, row.names = FALSE)
       } else {
           write.table(summary_df, file = summary_file_path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
       }
  
  return(model_options)
  }
}

summarise_options <- function(model_options) {
  # clean up to just the stuff I want ( this will change later I'm sure)
  
  # Group by all hyperparameters up to ntree and calculate means for all metrics
  grouped_data <- model_options %>%
    group_by(down_Hz, num_individuals, number_behaviours, number_features, 
            threshold, window, overlap, feature_normalised,
             feature_selection, ntrees) %>%
    summarise(across(everything(), ~mean(as.numeric(., na.rm = TRUE))))
  return(grouped_data)
}



