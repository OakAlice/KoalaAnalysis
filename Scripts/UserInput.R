# User Input
# The script where the variables are entered and selected

source(file.path(base_path, "Scripts/Dictionaries.R"))

## SET UP ####
  # Experiment Number # for keeping track of your work
  ExperimentNumber <- 1
  
  # which of the studies from the dictionary page
  MovementData <- Vehkaoja_Dog # INPUT HERE ####
  
  # base directory # where Data, Scripts, and Output are saved # currently for the HPC
  #base_path <- "//hpccache/HPCcache/private/oaw001"

  
### EXPERIMENT HARDCODES #### 
  selectedBehaviours <- MovementData$behaviours_1
  behaviours <- "behaviours_1"
  behaviour_options <- NULL
  ignoreBehaviours <- c("<NA>")
  folds <- 3 # unless I can calculate this from the number of individuals
  
  # Proportion as a decimal percentage
  trainingPercentage <- 0.7
  validationPercentage <- 0.3
  testingPercentage <- 0.3
  
## VALIDATION ####
# Training Testing split method (choose from: random, chronological, LOIO, 2_individuals, or something unique)
# 2_individuals: split ind 1 chronologically for training and validation, and ind 2 as LOIO test
# unique options if necessary: SparkesKoalaValidation
  splitMethod <- c("LOIO")
  # test_individuals <- c("Elsa") # special setting just for SparkesKoalaValidation 
  crossValidation <- c(TRUE, FALSE) # do you want to cross validate or not?
  splitStratified <- c(TRUE, FALSE) # stratify within the behaviours for each cross validation
  
## HYPERPARAMETERS TO SEARCH ####
  searchStrategy <- "grid" # "random", "BO"
  num_individuals <- c(8, 5) # can make a list, MovementData$individuals
  downsampling_Hz <- c(20) # downsampling options
  balancing_thresholds <- c(500) # should I be experimenting with this?
  window <- c(1, 2) # window length in seconds
  overlap <- c(0) # window overlap as a percentage
  
  # Full feature list thus far coded in
  featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA") #, 
                    #"RMS", "FFT", "entropy", "zero_crossing")
  featureSelection <- c(FALSE) # whether selection should be performed
  featureNormalisation <- c(FALSE) # "MinMaxScaling", "Standardiation" # Feature normalisation

## MODEL ARCHITECTURE ####
# Model architecture, RF or SOM
  modelArchitecture <- "RF" # add SVM, DT, NN, MLP, and XGB
  probabilityReport <- c(FALSE) # True or false
  probabilityThreshold <- 0 # as a percentage, if probability is true
  
## HYPERMARAMETERS ####
  #data_presentations <- c(100, 200) # for the SOM
  ntree_list <- c(100, 250, 500, 1000) # for RF
  
  
  
  
  
  
  
# don't worry about this stuff for now
## PREDICTION ON THE UNLABELLED ####
summarisation_window <- 1 # in minutes
columnSubsetUnlabelled <- MovementData$column_subset2 # 




