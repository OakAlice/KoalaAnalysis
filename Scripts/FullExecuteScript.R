## Execute Script 2 continuous version ##
# saves the output of each experiment to an appended csv and then runs optimal settings in a new model

library(pacman)
p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, cluster, purrr, cowplot, scales)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source each of the functions from other scripts
source("UserInput.R")
source("ReformattingData.R")
source("CombiningBehaviours.R")
source("DataExploration.R")
source("GeneralFunctions.R")
source("FeatureProcessing.R")
source("SplitData.R")
#source("Clustering.R")
source("RandomForest.R")
source("OptimalModelRun.R")

## PART ZERO: SET UP ####
# make the experiment directory
Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
ensure_dir(Experiment_path) # experiment directory

# where you want the summaries to be stored
summary_file_path <- file.path(Experiment_path, 'Summary.csv')

# read in Data
MoveData0 <- read.csv(MovementData$data_location)

# format
formatted_data <- format_movement_data(MoveData0, columnSubset, test_individuals, desired_Hz, current_Hz, selectedBehaviours, ExperimentNumber)

# explore # graphs will print to the Experiment directory
exploreData(Experiment_path, formatted_data, ignoreBehaviours)
plotBehaviouralSamples(selectedBehaviours, formatted_data, Experiment_path)
  
# create the behaviour labels for this round
relabelled_data <- relabel_activities(formatted_data, relabelledBehaviours)
relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
#relabelled_data <- formatted_data

  
## PART ONE: MODEL SELECTION ####  
# Process data, run models, and save to the same csv
  for (window_length in window) {
    for (overlap_percent in overlap) {
      # Process data
      processed_data <- process_data(relabelled_data, featuresList, window_length, overlap_percent)
        
      for (split in splitMethod) {
          # Split data
          list_train_test <- split_condition(processed_data, modelArchitecture, threshold, split, trainingPercentage, validationPercentage, test_individuals, good_individuals)
          trDat <- na.omit(list_train_test$train)
          valDat <- na.omit(list_train_test$validate)
          tstDat <- na.omit(list_train_test$test)
          
          #if ("RF" %in% modelArchitecture) {
            for (trees_number in ntree_list) {
              # Train and Test Random Forest Model
              rf_model <- train_rf_model(trDat, trees_number)
              test_predictions <- predict_rf_model(rf_model, valDat)
              metrics_df <- evaluate_rf_model(test_predictions, valDat, targetBehaviours)
              
              summary_df <- save_rf_model(
                rf_model, metrics_df, ExperimentNumber, test_individuals, 
                desired_Hz, selectedBehaviours, featuresList, threshold, window_length, 
                overlap_percent, split, trees_number, summary_file_path
              )
        }
      }
    }
  }
#}

  
## PART TWO: OPTIMAL MODEL ####  
# select the optimal hyperparamters from the csv and create the model
optimal_window <- 2
optimal_overlap <- 10
optimal_split <- "SparkesKoalaValidation"
optimal_ntree <- 100
optimal_threshold <- 10000

#create dataasets
processed_data <- process_data(relabelled_data, featuresList, optimal_window, optimal_overlap)
list_train_test <- split_condition(processed_data, modelArchitecture, optimal_threshold, 
                                   optimal_split, trainingPercentage, validationPercentage, 
                                   test_individuals, good_individuals)
trDat <- na.omit(list_train_test$train)
valDat <- na.omit(list_train_test$validate)
tstDat <- na.omit(list_train_test$test)
randomised_tstDat <- tstDat
randomised_tstDat$activity <- randomised_tstDat$activity[sample(nrow(randomised_tstDat))]

# train model
rf_model <- train_rf_model(trDat, optimal_ntree)

# check it
probabilityReport <- FALSE
probabilityThreshold <- 0.5
testReturns <- test_optimal_model(rf_model, tstDat, probabilityReport, probabilityThreshold)

print(testReturns$plot_pred_act)
print(testReturns$confusion_plot)
print(testReturns$plot_stacked)
print(testReturns$metrics)
