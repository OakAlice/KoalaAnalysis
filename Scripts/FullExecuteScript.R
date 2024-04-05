## Execute Script ####

# load libraries and scripts

library(pacman)
p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
       WaveletComp, purrr, cowplot, scales, crqa, pracma, doParallel,
       foreach, parallel)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

scripts <- c("UserInput.R", "ReformattingData.R", "CombiningBehaviours.R", 
           "GeneralFunctions.R", "FeatureProcessing.R", "UnlabelledData.R",
           "SplitData.R", "RandomForest.R", "3.GenerateOptimalModel.R", "PartitionData.R",
           "2.ModelTuningExecuteScript.R") #, "DataExploration.R")
for (script in scripts) {
  source(script)
}

# create a save space
  Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
  ensure_dir(Experiment_path) # experiment directory

# read in data and format to standardised shape
  MoveData0 <- read.csv(MovementData$Training_location)
  
  # debugging
  debugging_data <- MoveData0 %>%
    group_by(activity) %>%
    slice(1:3000)
  
  MoveData <- debugging_data
  MoveData <- MoveData0
  
  formatted_data <- format_movement_data(MoveData, columnSubsetTraining, num_individuals, 100, 100, selectedBehaviours)
  
  # explore # graphs will print to the Experiment directory
  #exploreData(Experiment_path, formatted_data, ignoreBehaviours)
  #plot_behaviours(selectedBehaviours, formatted_data, Experiment_path, 1000, 2)


## PART ONE: SPLIT OUT THE TEST SET ####
split_data <- split_condition(formatted_data, splitMethod, trainingPercentage, validationPercentage, num_individuals, test_individuals)
otherDat <- na.omit(split_data$other)
tstDat <- na.omit(split_data$test)

## PART TWO: MODEL SELECTION ####
# Process data, run models, and save to a parent csv
modelOptions <- modelTuning(otherDat, relabelledBehaviours, MovementData, downsampling_Hz, window, overlap, featuresList, 
                        balancing_thresholds, folds, trainingPercentage, ntree_list, targetBehaviours, 
                        ExperimentNumber, test_individuals, split)

summarisedModelOptions <- exploreOptions(modelOptions)
write.csv(summarisedModelOptions, file.path(Experiment_path, 'SummarisedModelOptions.csv'))
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

