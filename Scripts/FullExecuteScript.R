## Execute Script 2 continuous version ##
# saves the output of each experiment to an appended csv and then runs optimal settings in a new model

library(pacman)
p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
       WaveletComp, cluster, purrr, cowplot, scales, crqa, pracma, doParallel,
       foreach)

# set up for parallelisation
num_cores <- detectCores()
cl <- makeCluster(num_cores-2)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source each of the functions from other scripts
files <- c("UserInput.R", "ReformattingData.R", "CombiningBehaviours.R", 
           "DataExploration.R", "GeneralFunctions.R", "FeatureProcessing.R", 
           "SplitData.R", "RandomForest.R", "OptimalModelRun.R")

for (file in files) {
  source(file)
}

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
#relabelled_data <- formatted_data[1:1000,]

  
## PART ONE: MODEL SELECTION ####  
# Process data, run models, and save to the same csv
# running in parallel
results <- foreach(window_length = window, overlap_percent = overlap, .combine = 'rbind') %dopar% {
  # Process data
  processed_data <- process_data(relabelled_data, featuresList, window_length, overlap_percent)
  
  results_list <- list()
  
  for (split in splitMethod) {
    # Split data
    list_train_test <- split_condition(processed_data, modelArchitecture, threshold, split, 
                                       trainingPercentage, validationPercentage, test_individuals, 
                                       good_individuals)
    trDat <- na.omit(list_train_test$train)
    valDat <- na.omit(list_train_test$validate)
    tstDat <- na.omit(list_train_test$test)
    
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
      
      results_list <- c(results_list, summary_df)
    }
  }
  
  return(results_list)
}

# Combine results
final_results <- do.call(rbind, results)

# Stop the cluster
stopCluster(cl)


  
## PART TWO: OPTIMAL MODEL ####  
# select the optimal hyperparamters from the csv and create the model
optimal_window <- 2
optimal_overlap <- 0
optimal_split <- "SparkesKoalaValidation"
optimal_ntree <- 100
optimal_threshold <- 4000
test_type <- "test"
probabilityReport <- FALSE
probabilityThreshold <- 0.5
desired_Hz <- 100

#create select the dataset you want to use
relabelled_data <- formatted_data

# run the whole process... note that there is parallel procesing inside the function
optimal_results <- verify_optimal_results(
  formatted_data, featuresList, optimal_window, optimal_overlap, desired_Hz, optimal_split, optimal_ntree, 
  optimal_threshold, trainingPercentage, validationPercentage, test_individuals, good_individuals,
  test_type, probabilityReport, probabilityThreshold)

print(optimal_results$confusion_matrix)
print(optimal_results$confusion_plot)
print(optimal_results$stacked_plot)
print(optimal_results$NA_loss_plot)
print(optimal_results$metrics)

## BONUS ####
# look at the feature information
key_behaviours <- c("Groom", "Walk", "Branch", "Bound", "Trot")
# temporarily relabel for visuals
processed_data2 <- processed_data %>%
  mutate(activity = recode(activity,
                           "Grooming" = "Groom", "Walking" = "Walk", "Branch Walking" = "Branch",
                           "Bound/Half-Bound" = "Bound", "Trot" = "Trot"))

extractFeatureInformation(processed_data2, key_behaviours, 7) # go here and look at the function

