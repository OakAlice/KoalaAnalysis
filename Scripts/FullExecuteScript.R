# Script for running on the HPC

## SCRIPT SET UP ####

# Load or install and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
               WaveletComp, purrr, cowplot, scales, crqa, pracma, doParallel,
               foreach, parallel)

# Load in all the scripts - assuming they are all together in the base path location
scripts <- c("Scripts/2.ModelTuningExecuteScript.R",
             "Scripts/3.GenerateOptimalModel.R",
             "Scripts/Dictionaries.R",
             "Scripts/UserInput.R",
             "Scripts/ReformattingData.R", "Scripts/CombiningBehaviours.R", "Scripts/GeneralFunctions.R", 
             "Scripts/FeatureProcessing.R", 
             "Scripts/SplitData.R", "Scripts/RandomForest.R", "Scripts/PartitionData.R",
             "Scripts/ValidationEvaluation.R",
             "Scripts/UnlabelledData.R", "Scripts/Balancing.R"
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
#Save_path <- file.path(base_path, "Output", MovementData$name, ExperimentNumber) ## previously: Experiment_path
#ensure_dir(Save_path) # experiment directory

## LOAD AND FORMAT DATA ####
# read in data
MoveData <- read.csv(MovementData$data_location)
formatted_data <- format_movement_data(MoveData, MovementData$column_subset, MovementData$time_format, num_individuals[1], MovementData$current_hz, 20)
  
  # explore # graphs will print to the Experiment directory
  #exploreData(Experiment_path, formatted_data, ignoreBehaviours)
  #plot_behaviours(selectedBehaviours, formatted_data, Experiment_path, 1000, 2)

## SPLIT OUT THE TEST SET ####
split_data <- split_condition(formatted_data, splitMethod, trainingPercentage, validationPercentage, num_individuals, test_individuals = 2)
otherDat <- na.omit(split_data$other)
tstDat <- na.omit(split_data$test)
test_individuals <- length(unique(tstDat$ID))

## MODEL SELECTION ####
# save path
# summary_file_path <- file.path(base_path, 'Output', 'Summary.csv')  # Path to the CSV file
summary_file_path <- file.path("C:/Users/oakle/Documents/PhD docs/Chapter_Two/Output", "Summary.csv")

# formulate all possible combinations
options_df <- expand.grid(num_individuals, behaviours, downsampling_Hz, splitStratified, window, 
                          overlap, featureNormalisation, featureSelection, 
                          balancing_thresholds, modelArchitecture, ntree_list)
colnames(options_df) <- c("individuals", "behaviourset", "down_Hz", "stratification", "window_length", 
                          "overlap_percent", "normalised", "feature_selected", 
                          "threshold", "modelArchitecture", "trees_number")

# set up which of them will be searched
# grid = all

# Process data, run models, and save to a parent csv
modelOptions <- lapply(1:nrow(options_df), function(i) {
  modelTuning(summary_file_path, 
              otherDat, 
              num_individuals,
              options_df[i, "behaviourset"], 
              options_df[i, "down_Hz"],
              options_df[i, "window_length"], 
              options_df[i, "overlap_percent"],
              options_df[i, "normalised"],
              options_df[i, "feature_selected"],
              featuresList,
              options_df[i, "threshold"], 
              options_df[i, "trees_number"],
              folds, 
              trainingPercentage,
              options_df[i, "stratification"]
              )
    }
)
modelOptions <- read.csv(summary_file_path)

summarisedModelOptions <- exploreOptions(modelOptions)
# write.csv(summarisedModelOptions, file.path("C:/Users/oakle/Documents/PhD docs/Chapter_Two/Output", "AveragedSummary.csv"))
write.csv(summarisedModelOptions, file.path(Experiment_path, 'SummarisedModelOptions.csv'))

# make heatmaps
heatmaps <- generateHeatmap(summarisedModelOptions, var1, var2)

# just basic for now but will make more exploration later
# add MCC in
  
# currently we manually assess the csv to find the optimal conditions
# set up auto extraction

  
## PART THREE: TEST OPTIMAL MODEL ON HOLD-OUT DATA ####
OptimalMLModel <- generateOptimalModel(otherDat, 
                                       behaviourset = "behaviours_2", 
                                       MovementData, 
                                       down_Hz = 20, 
                                       window_length = 0.5, 
                                       overlap_percent = 50, 
                                       featuresList, 
                                       threshold = 500, 
                                       folds = 10, 
                                       trainingPercentage = 0.6, 
                                       trees_number = 500)
# save for later
model_file_path <- file.path(Experiment_path, "6BehOptimalModel.rda")
save(OptimalMLModel, file = model_file_path)

# assess performance on the hold-out data
# relabel and process the tstDat # turn this into a function
behaviours <- MovementData[["behaviours_2"]]
relabelled_data <- relabel_activities(tstDat, behaviours)
relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
processed_data <- process_data(relabelled_data, featuresList, window_length = 0.5, overlap_percent = 50, 20) # last one is down_Hz
tstDat2 <- processed_data %>% select(-ID)

optimal_results <- verify_optimal_results(tstDat2, OptimalMLModel, test_type = "test", 
                                          probabilityReport = FALSE,  probabilityThreshold = NULL)

print(optimal_results$confusion_matrix)
print(optimal_results$confusion_plot)
print(optimal_results$stacked_plot)
#print(optimal_results$NA_loss_plot)
print(optimal_results$metrics)


## PART FOUR: APPLYING TO UNLABELLED DATA ####  

# chunked folder
folders <- list.dirs(MovementData$Unlabelled_location, full.names = FALSE, recursive = FALSE)

current_Hz <- 50
optimal_Hz <- 50

load(model_file_path) # load it back in, comes as OptimalMLModel

prediction_outcome <- data.frame()

for (folder in folders) {
  
  # read them all into a single csv
  folder_path <- file.path(MovementData$Unlabelled_location, folder)
  combined_data <- bind_rows(lapply(list.files(folder_path, pattern = "\\.csv$", full.names = TRUE), read_csv))
  write_csv(combined_data, file.path(MovementData$Unlabelled_location, paste0(folder, "_full.csv")))
  
#} # uncomment to perform the predictions too
  #folder <- folders[1]
  files <- list.files(file.path(MovementData$Unlabelled_location, folder), full.names = TRUE, recursive = FALSE)
  
  for (file in files) {
    #file <- files[1]
    unlabelled_file <- read.csv(file)
    
    # extract the name
    file_name <- basename(file)
    ID_name <- str_extract(file_name, "(?<=_)(.*?)(?=_)")
    
    # organise
    formatted_file <- formattingUnlabelled(unlabelled_file, ID_name, columnSubsetUnlabelled, current_Hz, optimal_Hz, columnSubsetTraining)
    processed_file <- process_data(formatted_file, featuresList, optimal_window, optimal_overlap, optimal_Hz)
    predictions <- predictingUnlabelled(processed_file, OptimalMLModel)
    
    # summarise per timevalue
    summarised_predictions <- aggregate_windows(predictions$predict_file, summarisation_window)

    # append to document
    prediction_outcome <- rbind(summarised_predictions, prediction_outcome)
  }
}

#  prediction_outcome <- summarise(prediction_outcome, 1)
# save it
prediction_file_path <- file.path(Experiment_path, 'Predictions.csv')
write.table(prediction_outcome, file = prediction_file_path, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE)
# plot it
grid_plot <- behaviour_grid(prediction_outcome)











### READ ALL CSVS INTO ONE FILE TO CHECK ORIENTATION ###
folder_path <- file.path(MovementData$Unlabelled_location, "Dalene")
combined_data <- bind_rows(lapply(list.files(folder_path, pattern = "\\.csv$", full.names = TRUE), read_csv))
write_csv(combined_data, file.path(MovementData$Unlabelled_location, "Dalene_full.csv"))

#combined_data <- read.csv(file.path(MovementData$Unlabelled_location, "Rachel_full.csv"))
combined_data <- formattingUnlabelled(combined_data, "Dalene", columnSubsetUnlabelled, 50, 0.01, columnSubsetTraining)

plot_data <- combined_data[1:length(combined_data$time),]

plotTrace(plot_data)

## BONUS ####
# look at the feature information
key_behaviours <- c("Groom", "Walk", "Branch", "Bound", "Trot")
# temporarily relabel for visuals
processed_data2 <- processed_data %>%
  mutate(activity = recode(activity,
                           "Grooming" = "Groom", "Walking" = "Walk", "Branch Walking" = "Branch",
                           "Bound/Half-Bound" = "Bound", "Trot" = "Trot"))

extractFeatureInformation(processed_data2, key_behaviours, 7) # go here and look at the function

