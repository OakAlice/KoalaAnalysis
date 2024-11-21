# Script for running on the HPC

## SCRIPT SET UP ####

# Saving locally:
  #base_path <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis"
# Saving on the HPC:
  base_path <- "//hpccache/HPCcache/private/oaw001"

# Load or install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
               WaveletComp, purrr, cowplot, scales, crqa, pracma, doParallel,
               foreach, parallel, neuralnet, xgboost, rpart)

# Load in all the scripts - assuming they are all together in the base path location
scripts <- c("Scripts/ModelTuning.R",
             "Scripts/Dictionaries.R",
             "Scripts/UserInput.R",
             "Scripts/ReformattingData.R", 
             "Scripts/FeatureProcessing.R", 
             "Scripts/SplitData.R", "Scripts/Balancing.R", "Scripts/PartitionData.R",
             "Scripts/Models.R",
             "Scripts/FormulateCombinations.R",
             "Scripts/ValidationEvaluation.R"
           ) #, "DataExploration.R", "GenerateOptimal.R)
# Function to source a script with error handling
source_script <- function(script_path) {
  if (file.exists(script_path)) {
    source(script_path)
  } else {
    message("Script not found: ", script_path)
  }
}

# Source each script
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

# where to save the outputs
save_path <- file.path(base_path, "Output", movement_data$name, experiment_number) ## previously: Experiment_path
ensure_dir(save_path) # experiment directory

## LOAD AND FORMAT DATA ####
# read in data
move_data <- read.csv(movement_data$data_location)
formatted_data <- format_movement_data(move_data, movement_data$column_subset, movement_data$time_format, num_individuals[1], movement_data$current_hz, downsampling_Hz)
  
  # explore # graphs will print to the Experiment directory
  # experiment_path <- "C:/Users/oakle/Documents/PhD docs/Chapter_Two/LabelledDataSets/Vehkaoja_Dog"
  #explore_data(experiment_path, formatted_data, ignore_behaviours)
  #plot_behaviours(selected_behaviours, formatted_data, experiment_path, 1000, 2)

## SPLIT OUT THE TEST SET ####
split_data <- split_condition(formatted_data, split_method, training_percentage, validation_percentage, num_individuals, test_individuals = 2)
otherDat <- na.omit(split_data$other)
tstDat <- na.omit(split_data$test)
test_individuals <- length(unique(tstDat$ID))

## MODEL SELECTION: HPO SEARCH ####
# formulate all possible combinations with the other variables
options_df <- expand.grid(num_individuals, behaviours, downsampling_Hz, split_stratified, window, 
                          overlap, feature_normalisation, feature_selection, 
                          balancing_thresholds, model_architecture)
colnames(options_df) <- c("individuals", "behaviour_set", "down_Hz", "split_stratification", "window_length", 
                          "overlap_percent", "feature_normalising", "feature_selecting", 
                          "balancing_threshold", "model_architecture")

# combine the preprocessing decisions with the hyperparameters
extended_options_df <- create_extended_options(model_hyperparameters_list, options_df)

# set up which of them will be searched
# grid = all

# Process data, run models, and save to a parent csv
model_options <- lapply(1:nrow(extended_options_df), function(i) {
  model_tuning(save_path, 
              movement_data$name,
              otherDat, 
              extended_options_df[i, "individuals"],
              extended_options_df[i, "behaviour_set"], 
              extended_options_df[i, "down_Hz"],
              extended_options_df[i, "split_stratification"],
              extended_options_df[i, "window_length"], 
              extended_options_df[i, "overlap_percent"],
              extended_options_df[i, "feature_normalising"],
              extended_options_df[i, "feature_selecting"],
              features_list,
              extended_options_df[i, "balancing_threshold"], 
              extended_options_df[i, "ntrees"],
              extended_options_df[i, "kernels"],
              extended_options_df[i, "costs"],
              extended_options_df[i, "model_architecture"],
              folds, 
              training_percentage
              )
    }
)
model_options <- read.csv(summary_file_path)

summarised_model_options <- summarise_options(model_options)
# write.csv(summarised_model_options, file.path("C:/Users/oakle/Documents/PhD docs/Chapter_Two/Output", "AveragedSummary.csv"))
write.csv(summarised_model_options, file.path(save_path, 'SummarisedModelOptions.csv'))

# make heatmaps
#heatmaps <- generate_heatmap(summarised_model_options, var1, var2)

# have moved optimal model generation to 'Scripts/GenerateOptimal.R'

