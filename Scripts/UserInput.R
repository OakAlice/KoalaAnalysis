# User Input

# The script where the variables are entered and selected

#### ONE VARIABLE / SET PER RUN ####

# Experiment Number # so all the results go into different folders and don't write over
ExperimentNumber <- 1

# directory where everything is to be saved
save_directory <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Redo"

# The data to analyse
MovementData <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/TrainingData2.csv"

# if tagging by individuals # my koalas dont have this functionality
# how many individuals to sample, set to NA if all
test_individuals <- NA

# Desired sampling frequency, as Hz (potentially different from actual sampling frequency)
current_Hz <- 25
desired_Hz <- 25

# tell me what each of the columns are the 
# ID, Time, X_accel, Y_accel, Z_accel, X_gyro, Y_gyro, Z_gyro, and activity
# comment out the ones you dont want to use
columnSubset <- c("Time" = "time", 
                  "X" = "X_accel", "Y" = "Y_accel", "Z" = "Z_accel",
                  #"GX" = "X_gyro", "GY" = "Y_gyro", "GZ" = "Z_gyro",
                  "Activity" = "activity")

# select the behaviours to include in the analysis
# for the dog data
selectedBehaviours <- c("Climb_1", "Climb_2", "Climb_3", "Climb_4", 
                        "Tree_movement", "Tree_still", 
                        "Walking_1", "Walking_2", "Walking_3", "Walking_4", 
                        "Ground_Movement", "Ground_Still")

# Features to be calculated on every axis, select from following list: "mean", "max", "min", "sd", "sk", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA"
featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA")

# Proportion of training data, as a decimal percentage (for chronological and random only)
trainingPercentage <- 0.6
validationPercentage <- 0.2
# testingPercentage is the remainder

# Sampling Threshold, run the below code to visualise the dataset and choose a threshold
####### THIS IS GOING TO HAVE TO BE AUTO, OR A PROMPT SCREEN???
threshold <- NA

#### CAN TRIAL MULTIPLE PER RUN ####

# rlen (number of data presentation epochs)
data_presentations <- c(10)

# Window length, in seconds
window <- c(0.5, 1, 2)

# Window overlap, as a % # if <0, is overlapping
overlap <- c(0)

# Training Testing split method (choose from: random, chronological, LOIO)
splitMethod <- c("random")
