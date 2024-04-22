# Script for running on the HPC

## SCRIPT SET UP ####

# Load or install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
               WaveletComp, purrr, cowplot, scales, crqa, pracma, doParallel,
               foreach, parallel, neuralnet, xgboost, rpart)

# Load in all the scripts - assuming they are all together in the base path location
scripts <- c("Scripts/ModelTuning.R",
             "Scripts/GenerateOptimal.R",
             "Scripts/Dictionaries.R",
             "Scripts/UserInput.R",
             "Scripts/ReformattingData.R", 
             "Scripts/FeatureProcessing.R", 
             "Scripts/SplitData.R", "Scripts/Balancing.R", "Scripts/PartitionData.R",
             "Scripts/Models.R",
             "Scripts/ValidationEvaluation.R",
             "Scripts/UnlabelledData.R"
           ) #, "DataExploration.R")
# Function to source a script with error handling
source_script <- function(script_path) {
  if (file.exists(script_path)) {
    source(script_path)
  } else {
    message("Script not found: ", script_path)
  }
}

# Source each script
base_path <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis"
#base_path <- "//hpccache/HPCcache/private/oaw001"
for (script in scripts) {
  script_path <- file.path(base_path, script)
  source_script(script_path)
}

# create the save directory
# Function for ensuring directory exists or creating it if not
ensure_dir <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, showWarnings = FALSE)
  }
}

#Save_path <- file.path(base_path, "Output", MovementData$name, ExperimentNumber) ## previously: Experiment_path
#ensure_dir(Save_path) # experiment directory

## LOAD AND FORMAT DATA ####
# read in data
move_data <- read.csv(movement_data$data_location)
formatted_data <- format_movement_data(move_data, movement_data$column_subset, movement_data$time_format, num_individuals[1], movement_data$current_hz, 20)
  
  # explore # graphs will print to the Experiment directory
  # experiment_path <- "C:/Users/oakle/Documents/PhD docs/Chapter_Two/LabelledDataSets/Vehkaoja_Dog"
  #explore_data(experiment_path, formatted_data, ignore_behaviours)
  #plot_behaviours(selected_behaviours, formatted_data, experiment_path, 1000, 2)

## SPLIT OUT THE TEST SET ####
split_data <- split_condition(formatted_data, split_method, training_percentage, validation_percentage, num_individuals, test_individuals = 2)
otherDat <- na.omit(split_data$other)
tstDat <- na.omit(split_data$test)
test_individuals <- length(unique(tstDat$ID))

## MODEL SELECTION ####
# save path
# summary_file_path <- file.path(base_path, 'Output', 'Summary.csv')  # Path to the CSV file
summary_file_path <- file.path("C:/Users/oakle/Documents/PhD docs/Chapter_Two/Output", "Summary.csv")

# formulate all possible combinations
options_df <- expand.grid(num_individuals, behaviours, downsampling_Hz, split_stratified, window, 
                          overlap, feature_normalisation, feature_selection, 
                          balancing_thresholds, model_architecture, ntree_list)
colnames(options_df) <- c("num_individuals", "behaviours", "down_Hz", "split_stratified", "window", 
                          "overlap", "feature_normalisation", "feature_selection", 
                          "balancing_thresholds", "model_architecture", "ntrees")

# set up which of them will be searched
# grid = all

# Process data, run models, and save to a parent csv
model_options <- lapply(1:nrow(options_df), function(i) {
  model_tuning(summary_file_path, 
              otherDat, 
              num_individuals,
              options_df[i, "behaviours"], 
              options_df[i, "down_Hz"],
              options_df[i, "window"], 
              options_df[i, "overlap"],
              options_df[i, "feature_normalisation"],
              options_df[i, "feature_selection"],
              featuresList,
              options_df[i, "balancing_thresholds"], 
              options_df[i, "ntrees"],
              options_df[i, "model_architecture"],
              folds, 
              trainingPercentage,
              options_df[i, "split_stratified"]
              )
    }
)
model_options <- read.csv(summary_file_path)

summarised_model_options <- summarise_options(model_options)
# write.csv(summarised_model_options, file.path("C:/Users/oakle/Documents/PhD docs/Chapter_Two/Output", "AveragedSummary.csv"))
write.csv(summarised_model_options, file.path(base_path, 'Output', 'SummarisedModelOptions.csv'))

# make heatmaps
heatmaps <- generate_heatmap(summarised_model_options, var1, var2)








## PART THREE: TEST OPTIMAL MODEL ON HOLD-OUT DATA ####
optimal_trained_model <- generate_optimal_model(otherDat, 
                                       behaviourset = "behaviours_2", 
                                       movement_data, 
                                       down_Hz = 20, 
                                       window_length = 0.5, 
                                       overlap_percent = 50, 
                                       features_list, 
                                       threshold = 500, 
                                       folds = 10, 
                                       training_percentage = 0.6, 
                                       model_architecture = "RF",
                                       trees_number = 500)
# save for later
model_file_path <- file.path(base_path, 'Output', "OptimalTrainedModel.rda")
save(optimal_trained_model, file = model_file_path)

# assess performance on the hold-out data
# relabel and process the tstDat # turn this into a function
behaviours <- MovementData[["behaviours_2"]]
relabelled_data <- relabel_activities(tstDat, behaviours)
relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
processed_data <- process_data(relabelled_data, features_list, window_length = 0.5, overlap_percent = 50, 20) # last one is down_Hz
tstDat2 <- processed_data %>% select(-ID)

optimal_results <- verify_optimal_results(tstDat2, optimal_trained_model, test_type = "test", 
                                          probability_report = FALSE,  probability_threshold = NULL)

print(optimal_results$confusion_matrix)
print(optimal_results$confusion_plot)
print(optimal_results$stacked_plot)
#print(optimal_results$NA_loss_plot)
print(optimal_results$metrics)
