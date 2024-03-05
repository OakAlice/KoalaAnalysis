## Execute Script 2 continuous version ##
# saves the output of each experiment to an appended csv and then runs the optimal settings in a new model

library(pacman)
p_load(dplyr, tidyverse, randomForest, caret, e1071, kohonen, cluster, purrr, cowplot)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source each of the functions from other scripts
source("UserInput.R")
source("ReformattingData.R")
source("DataExploration.R")
source("GeneralFunctions.R")
source("FeatureProcessing.R")
source("SplitData.R")
#source("Clustering.R")
source("RandomForest.R")
source("OptimalModelRun.R")

# make the experiment directory
Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
ensure_dir(Experiment_path) # experiment directory

# where you want the summaries to be stored
summary_file_path <- file.path(Experiment_path, 'Summary.csv')

# read in Data
MoveData0 <- read.csv(MovementData)

# format
formatted_data <- format_movement_data(MoveData0, columnSubset, test_individuals, desired_Hz, current_Hz, selectedBehaviours, ExperimentNumber)

# explore # graphs will print to the Experiment directory
exploreData(Experiment_path, formatted_data, ignoreBehaviours)
plotBehaviouralSamples(selectedBehaviours, formatted_data, Experiment_path)
  
# Process data, run models, and save to the same csv
for (window_length in window) {
  for (overlap_percent in overlap) {
    # Process data
    processed_data <- process_data(formatted_data, featuresList, window_length, overlap_percent)
    
    for (split in splitMethod) {
      # Split data
      list_train_test <- split_condition(processed_data, modelArchitecture, threshold, split, trainingPercentage, validationPercentage, test_individuals)
      trDat <- na.omit(list_train_test$train)
      valDat <- na.omit(list_train_test$validate)
      tstDat <- na.omit(list_train_test$test)
      
      #if ("RF" %in% modelArchitecture) {
        for (trees in ntree_list) {
          # Train and Test Random Forest Model
          rf_model <- train_rf_model(trDat, trees)
          test_predictions <- predict_rf_model(rf_model, valDat)
          metrics_df <- evaluate_rf_model(test_predictions, valDat, targetBehaviours)
          
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

# select the optimal hyperparamters from the csv and create the model
optimal_window <- 3
optimal_overlap <- 0
optimal_split <- "LOIO"
optimal_ntree <- 10
optimal_threshold <- 1000

trainReturns <- train_optimal_model(formatted_data, featuresList, optimal_window, optimal_overlap, 
                      optimal_threshold, optimal_split, trainingPercentage, validationPercentage, optimal_ntree, test_individuals)

model <- trainReturns$model
tstDat <- trainReturns$hold_out_data # tstDat that hasn't been used in the system yet

testReturns <- test_optimal_model(model, valDat)

print(testReturns$plot)
testReturns$confusion
sum(diag(testReturns$confusion)) / sum(testReturns$confusion)


