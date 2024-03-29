## Execute the script, all functions in order

library(pacman)
p_load(dplyr, tidyverse, randomForest, caret, e1071)
      # kohonen, data.table, lattice, glue, moments, fs, grid, png, reshape2, e1071)

setwd("C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Scripts") # scripts location

# source the variables on the preceding script
source("UserInput.R")
source("ReformattingData.R")
source("GeneralFunctions.R")
source("FeatureProcessing.R")
source("SplitData.R")
source("RandomForest.R")


#### Create Directories ####  
# There can be different overlaps, windows, splits, and epochs
Experiment_path <- paste0(save_directory, "/Experiment_", ExperimentNumber)
ensure_dir(Experiment_path) # experiment directory
create_experiment_directories(Experiment_path, window, overlap, splitMethod, data_presentations) # subdirectories

#### Format Data ####
# load in the data
MoveData0 <- read.csv(MovementData)
formatted_data <- format_movement_data(MoveData, columnSubset, test_individuals, desired_Hz, current_Hz, selectedBehaviours, ExperimentNumber)
write.csv(formatted_data, paste0(Experiment_path, '/Formatted_MoveData.csv'))

#### Feature Creation ####
MoveData <- read.csv(paste0(Experiment_path, '/Formatted_MoveData.csv'))
# Process data, creating features according to window length and overlap
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    
    # Define the correct subdirectory path using Experiment path, window, and overlap
    overlap_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"))
    print(overlap_path) # progress check
    
    processed_data <- process_data(MoveData, featuresList, window_length, overlap_percent)
    
    # Use the overlap_path to write the CSV in the correct subdirectory
    write.csv(processed_data, file.path(overlap_path, 'Processed_Data.csv'))
  }
}

#### Create Training and Testing Data ####
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    for (splitt in splitMethod) { # for each of the split methods (use tt so not a base R function)
      # Define the correct subdirectory path using Experiment path, window, and overlap
      file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"))
      print(file_path)
      split_condition(file_path, modelArchitecture, threshold, splitt, trainingPercentage)
    }
  }
}

#### RUNNING MODELS ####
if ("RF" %in% modelArchitecture){
  # random forest
  for (window_length in window) { # for each of the windows
    for (overlap_percent in overlap) { # for each of the overlaps
      for (split in splitMethod) { # for each of the split methods 
        for (trees in ntree) { # for each of the tree numbers 
          # Define the correct sub-directory path using Experiment path, window, and overlap
          trDatPath <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TrainingData.csv')
          tstDatPath <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TestingData.csv')
          
          trDat <- read_csv(trDatPath, show_col_types = FALSE)
          tstDat <- read_csv(tstDatPath, show_col_types = FALSE)
          
          # train the model #simple: add hyperparameters later
          rf_model <- train_rf_model(trDat, ntree)
          
          # extract testing data and format
          test_predictions <- predict_rf_model(rf_model, tstDat)
          
          # test the model
          metrics_df <- evaluate_rf_model(test_predictions, tstDat)
          
          # reformat and save outcomes to the respective folders
          summary_df <- save_rf_model(rf_model, metrics_df)
          
          write.csv(summary_df, file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                    paste0(overlap_percent, "%_overlap"), split, "RF", paste0(trees, "_param"), 'Summary.csv'))
          
        }
      }
    }
  }
  
  
#### SOM UNDER HERE  
  
#else if ("SOM" %in% modelArchitecture) {
  
  # Self-organising map
  for (window_length in window) { # for each of the windows
    for (overlap_percent in overlap) { # for each of the overlaps
      for (split in splitMethod) { # for each of the split methods 
        
        #window_length <- 1
        #overlap_percent <- 0
        #split <- c("LOIO")
        
        file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split)
        
        # progress tracking
        print(file_path)
        
        load(file = file.path(file_path, "TrainingData.rda"))
        load(file = file.path(file_path, "TestingData.rda")) # this makes Jack sad, :(
        optimal_dimensions <- run_som_tests(trDat, tstDat, file_path)
        write.csv(optimal_dimensions, file.path(file_path, "Optimal_dimensions.csv"))
      }
    }
  }
  
  #### DEVELOP THE SOM MAPS ####
  # Essentially the same execution as above, but using the optimal dimensions
  for (window_length in window) { # for each of the windows
    for (overlap_percent in overlap) { # for each of the overlaps
      for (split in splitMethod) { # for each of the split methods 
        for (epochs in data_presentations) { # for each of the rlen lengths
          
          #window_length <- 1
          #overlap_percent <- 0
          #split <- c("LOIO")
          # epochs <- data_presentations[1]
          
          file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), 
                                 paste0(overlap_percent, "%_overlap"), split)
          
          # progress tracking
          print(file_path)
          
          # load the files
          load(file = file.path(file_path, "TrainingData.rda"))
          load(file = file.path(file_path, "TestingData.rda"))
          optimal_dimensions <- read.csv(file = file.path(file_path, "Optimal_dimensions.csv"))
          
          # extract the shape
          width <- optimal_dimensions$Width
          height <- optimal_dimensions$Height
          
          # produce the results
          som_results <- performOptimalSOM(trDat, tstDat, width, height, file_path, epochs)
          save_and_plot_optimal_SOM(trDat, tstDat, width, height, file_path, epochs)
        }
      }
    }
  }
}






#### SOM Results ####
Results_tables <- find_all_instances(paste0("Experiment_", ExperimentNumber), "Statistical_results.csv")
Summarise_results(ExperimentNumber, Results_tables)
Results_maps <- find_all_instances(paste0("Experiment_", ExperimentNumber), "optimal_SOM_plot.png")
Plot_results(ExperimentNumber, Results_maps)
display_experiment_params(ExperimentNumber, MovementData, test_individuals, current_Hz, 
                          desired_Hz, columnSubset, selectedBehaviours, featuresList, 
                          trainingPercentage, threshold, window, overlap, splitMethod, data_presentations)