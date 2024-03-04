# User Input
# The script where the variables are entered and selected

source("BehavioursLists.R")
source("ColumnSubsetLists.R")
source("CsvLocationLists.R")

## SET UP ####
  # Experiment Number # for keeping track of your work
  ExperimentNumber <- 8
  
  # directory where everything is to be saved
  save_directory <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Redo"
  
  # The raw csv data
  MovementData <- JordanPerentieData


## FORMATTING ####
  # if tagging by individuals # how many individuals to sample, set to NA if all
  test_individuals <- 2
  
  # Desired sampling frequency, as Hz (potentially different from actual sampling frequency)
  current_Hz <- 50
  desired_Hz <- 50
  
  # tell me which of your columns represent the 
  # ID, Time, X_accel, Y_accel, Z_accel, X_gyro, Y_gyro, Z_gyro, and activity
  # create new list, or select from ColumnSubsetList
  columnSubset <- JordanPerentieColumns
  
  # what format is the time in: matlab, other
  timeFormat <- "matlab"
  
  # select the behaviours to include in the analysis
  # enter new list, or select from BehavioursList
  selectedBehaviours <- JordanPerentieBehaviours
  
  # target behaviours
  targetBehaviours <- c("Walking")


## PREPROCESSING ####
# can select multiple settings for each
  
  # Balancing
  # Sampling Threshold, run the below code to visualise the dataset and choose a threshold
  threshold <- c(500)
  
  # up or down sampling 
  balancing <- c("up", "down")

  # Smoothing and normalisation
  Smoothing <- c(TRUE, FALSE)
  Normalisation <- c(TRUE, FALSE)
  
  # Window length, in seconds
  window <- c(0.5, 1, 2, 3)
  
  # Window overlap, as a decimal percentage # if <0, is overlapping
  overlap <- c(0, 10, 50)
  
  # Features to be calculated on every axis, select from following list: 
  # "mean", "max", "min", "sd", "sk", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA"
  featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA")


## VALIDATION ####
  # Training Testing split method (choose from: random, chronological, LOIO, 2_individuals)
  # 2_individuals: split ind 1 chronologically for training and validation, and ind 2 as LOIO test
  splitMethod <- c("random", "chronological", "2_individuals")
  
  # stratify within the behaviours, pick from: TRUE, FALSE, or both
  splitStratified <- c(TRUE, FALSE)
  
  # Proportion of training data, as a decimal percentage
  trainingPercentage <- 0.5
  validationPercentage <- 0.2
  # testingPercentage is the remainder


## MODEL ARCHITECTURE ####
# Model architecture, RF or SOM
  modelArchitecture <- "RF"
  
  # hyperparamters ### need to make this with multiple versions
  #data_presentations <- c(100, 200) # for the SOM
  ntree_list <- c(10, 50, 100, 200) # for RF

