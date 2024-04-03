## Execute Script 2 continuous version ##
# saves the output of each experiment to an appended csv and then runs optimal settings in a new model

library(pacman)
p_load(dplyr, tidyverse, randomForest, ggpubr, caret, e1071, kohonen, 
       WaveletComp, purrr, cowplot, scales, crqa, pracma, doParallel,
       foreach, parallel)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source each of the functions from other scripts
scripts <- c("UserInput.R", "ReformattingData.R", "CombiningBehaviours.R", 
           "GeneralFunctions.R", "FeatureProcessing.R", "UnlabelledData.R",
           "SplitData.R", "RandomForest.R", "OptimalModelRun.R") #, "DataExploration.R")
for (script in scripts) {
  source(script)
}

# Function to handle errors and print messages
handle_error <- function(e) {
  message("Error occurred: ", conditionMessage(e))
}

## PART ZERO: SET UP ####
# Add try-catch block to handle errors
tryCatch({
  # make the experiment directory
  Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
  ensure_dir(Experiment_path) # experiment directory
  
  # where you want the summaries to be stored
  summary_file_path <- file.path(Experiment_path, 'Summary.csv')
  
  # read in Data
  MoveData0 <- read.csv(MovementData$Training_location)
  
  # format
  
  ### MOVE THIS #### Put it in the loop
  formatted_data <- format_movement_data(MoveData0, columnSubsetTraining, test_individuals, 50, 100, selectedBehaviours, ExperimentNumber)
  
  # explore # graphs will print to the Experiment directory
  #exploreData(Experiment_path, formatted_data, ignoreBehaviours)
  #plot_behaviours(selectedBehaviours, formatted_data, Experiment_path, 1000, 2)

}, error = handle_error)  # Specify the error handling function

## PART ONE: MODEL SELECTION #### 

# Process data, run models, and save to the same csv
for (behaviourset in relabelledBehaviours){
  # create the behaviour labels for this round
  # behaviourset <- relabelledBehaviours[2]
  behaviours <- MovementData[[behaviourset]]
  relabelled_data <- relabel_activities(formatted_data, behaviours)
  relabelled_data <- relabelled_data[relabelled_data$activity != "NA", ]
  num_behs <- length(unique(relabelled_data$activity))

  for (frequency in downsampling_Hz){
    for (window_length in window) {
      for (overlap_percent in overlap) {
        # Process data
        # this part will be parallel processed
        processed_data <- process_data(relabelled_data, featuresList, window_length, overlap_percent, frequency)
        
        for (split in splitMethod) {
          for (threshold in balancing_thresholds){
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
                frequency, num_behs, featuresList, threshold, window_length, 
                overlap_percent, split, trees_number, summary_file_path
              )
            }
          }
        }
      }
    }
  }
}

  
## PART TWO: OPTIMAL MODEL ####  
# select the optimal hyperparamters from the csv and create the model
optimal_window <- 2
optimal_overlap <- 10
optimal_split <- "SparkesKoalaValidation"
behaviourset <- relabelledBehaviours[2]
optimal_ntree <- 500
optimal_threshold <- 4000
test_type <- "test"
probabilityReport <- FALSE
probabilityThreshold <- 0.5
optimal_Hz <- 50

# run the whole process... note that there is parallel procesing inside the function
optimal_results <- verify_optimal_results(
  relabelled_data, featuresList, optimal_window, optimal_overlap, optimal_Hz, 
  optimal_split, optimal_ntree, optimal_threshold, trainingPercentage, validationPercentage, 
  test_individuals, good_individuals, test_type, probabilityReport, probabilityThreshold)

print(optimal_results$confusion_matrix)
print(optimal_results$confusion_plot)
print(optimal_results$stacked_plot)
#print(optimal_results$NA_loss_plot)
print(optimal_results$metrics)

OptimalMLModel <- optimal_results$trained_model
model_file_path <- file.path(Experiment_path, "OptimalModel.rda")
save(OptimalMLModel, file = model_file_path)





## PART THREE: APPLYING TO UNLABELLED DATA ####  

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
    summarised_predictions <- summarise(predictions$predict_file, summarisation_window)

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

