## Execute Script 2 continuous version ##
# doesn't print each of the stages, just saves the output to an appended csv

library(pacman)
p_load(dplyr, tidyverse, randomForest, caret, e1071, kohonen, cluster, purrr)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source each of the functions from other scripts
source("UserInput.R")
source("ReformattingData.R")
source("GeneralFunctions.R")
source("FeatureProcessing.R")
source("SplitData.R")
source("Clustering.R")
source("RandomForest.R")

# make the experiment directory
Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
ensure_dir(Experiment_path) # experiment directory

# where you want the summaries to be stored
summary_file_path <- file.path(Experiment_path, 'Summary.csv')

# Format Data
MoveData0 <- read.csv(MovementData)
formatted_data <- format_movement_data(MoveData0, columnSubset, test_individuals, desired_Hz, current_Hz, selectedBehaviours, ExperimentNumber)

# Process data, run models, and save to the same csv
for (window_length in window) {
  for (overlap_percent in overlap) {
    # Process data
    processed_data <- process_data(formatted_data, featuresList, window_length, overlap_percent)
    
    for (split in splitMethod) {
      # Split data
      list_train_test <- split_condition(processed_data, modelArchitecture, threshold, split, trainingPercentage)
      trDat <- list_train_test$train
      tstDat <- list_train_test$test
      
      #if ("RF" %in% modelArchitecture) {
        for (trees in ntree_list) {
          # Train and Test Random Forest Model
          rf_model <- train_rf_model(trDat, trees)
          test_predictions <- predict_rf_model(rf_model, tstDat)
          metrics_df <- evaluate_rf_model(test_predictions, tstDat, targetBehaviours)
          
          summary_df <- save_rf_model(
            rf_model, metrics_df, ExperimentNumber, test_individuals, 
            desired_Hz, selectedBehaviours, featuresList, threshold, window_length, 
            overlap_percent, split, trees, summary_file_path
          )
        }
      }
    }
  }
#}

