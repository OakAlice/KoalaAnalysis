# User Input
# The script where the variables are entered and selected

source(file.path(base_path, "Scripts/Dictionaries.R"))

## SET UP ####
  # Experiment Number # for keeping track of your work
  experiment_number <- 1
  
  # which of the studies from the dictionary page
  movement_data <- Vehkaoja_Dog # INPUT HERE ####

### EXPERIMENT HARDCODES #### 
  selected_behaviours <- movement_data$behaviours_1
  behaviours <- "behaviours_1"
  behaviour_options <- NULL
  ignore_behaviours <- c("<NA>", "<undefined>")
  folds <- 3 # unless I can calculate this from the number of individuals?
  
  # Proportion as a decimal percentage
  training_percentage <- 0.7
  validation_percentage <- 0.3
  testing_percentage <- 0.3
  
## VALIDATION ####
# Training Testing split method (choose from: random, chronological, LOIO, 2_individuals, or something unique)
# 2_individuals: split ind 1 chronologically for training and validation, and ind 2 as LOIO test
# unique options if necessary: SparkesKoalaValidation
  split_method <- c("LOIO")
  # test_individuals <- c("Elsa") # special setting just for SparkesKoalaValidation 
  cross_validation <- c(TRUE, FALSE) # do you want to cross validate or not?
  split_stratified <- c(TRUE, FALSE) # stratify within the behaviours for each cross validation # not coded yet
  
## HYPERPARAMETERS TO SEARCH ####
  search_strategy <- "grid" # "random", "BO" # not coded yet
  num_individuals <- c(5) # can make a list, movement_data$individuals
  downsampling_Hz <- c(10) # downsampling options
  balancing_thresholds <- c(500) # how mnay samples from each behaviour
  window <- c(1, 2) # window length in seconds
  overlap <- c(0) # window overlap as a percentage
  
  # Full feature list thus far coded in
  features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA") #, 
                    #"RMS", "FFT", "entropy", "zero_crossing")
  feature_selection <- c(FALSE) # whether selection should be performed # not coded yet
  feature_normalisation <- c(FALSE) # "MinMaxScaling", "Standardiation" # Feature normalisation

## MODEL ARCHITECTURE ####
# Model architecture, RF or SOM
  model_architecture <- c("SVM") # so far: RF and SVM
  probability_report <- c(FALSE) # True or false
  probability_threshold <- 0 # as a percentage, if probability is true
  
## HYPERMARAMETERS ####
model_hyperparameters_list <- list(
  RF = list(ntrees = c(100, 250, 500, 1000)),
  SVM = list(kernels = "linear", 
             costs = c(0.5, 1))
)

  
  
  
  
  
  
  
# don't worry about this stuff for now
## PREDICTION ON THE UNLABELLED ####
summarisation_window <- 1 # in minutes
column_subset_unlabelled <- movement_data$column_subset2 # 




