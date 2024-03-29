# User Input
# The script where the variables are entered and selected

source("Dictionaries.R")

## SET UP ####
  # Experiment Number # for keeping track of your work
  ExperimentNumber <- 9
  
  # directory where everything is to be saved
  save_directory <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Redo"
  
  # which of the studies from the dictionary page
  MovementData <- SparkesKoala

## FORMATTING ####
  test_individuals <- MovementData$test_individuals
  current_Hz <- MovementData$current_hz
  downsampling_Hz <- c(20, 50, 100)
  columnSubsetTraining <- MovementData$column_subset
  columnSubsetUnlabelled <- MovementData$column_subset2
  timeFormat <- MovementData$time_format
  selectedBehaviours <- MovementData$behaviours_1
  relabelledBehaviours <- c("behaviours_1","behaviours_2", "behaviours_3")
  behaviour_options <- MovementData$behaviours_2
  ignoreBehaviours <- c("<NA>")
  targetBehaviours <- MovementData$target_behaviour

## PREPROCESSING ####
# can select multiple settings for each
  
  # Balancing
  # Sampling Threshold
  balancing_thresholds <- c(500, 1000, 2000, 4000)
  
  # Window length, in seconds
  window <- c(0.5, 1, 2)
  
  # Window overlap, as a decimal percentage # if <0, is overlapping
  overlap <- c(0, 50)
  
  # Features to be calculated on every axis, select from following list: 
  # "mean", "max", "min", "sd", "sk", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA"
  featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", 
                    "RMS", "FFT", "entropy", "zero_crossing")
  
  # Feature normalisation
  Normalisation <- c(FALSE) # "MinMaxScaling", "Standardiation"


## VALIDATION ####
  # Training Testing split method (choose from: random, chronological, LOIO, 2_individuals, unique)
  # 2_individuals: split ind 1 chronologically for training and validation, and ind 2 as LOIO test
  # unique options when nothing else worked: SparkesKoalaValidation
  splitMethod <- c("SparkesKoalaValidation")
  good_individuals <- c("Elsa", "Meeka") # just for SparkesKoalaValidation 
  
  # stratify within the behaviours, pick from: TRUE, FALSE, or both
  #splitStratified <- c(TRUE, FALSE)
  
  # Proportion of training data, as a decimal percentage
  trainingPercentage <- 0.6
  validationPercentage <- 0.4
  # testingPercentage is the remainder


## MODEL ARCHITECTURE ####
# Model architecture, RF or SOM
  modelArchitecture <- "RF"
  probabilityReport <- c(FALSE) # True or false
  probabilityThreshold <- 80 # as a percentage, above which accepted as true
  
  # hyperparamters ### need to make this with multiple versions
  #data_presentations <- c(100, 200) # for the SOM
  ntree_list <- c(100, 200, 500, 1000) # for RF
  
## PREDICTION ####
summarisation_window <- 1 # in minutes

