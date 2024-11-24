# Koala Analysis Script v2 ------------------------------------------------
# Updated version of the script for the koala data in Gabby Sparkes' PhD
# Written by Oakleigh Wilson, Nov 2024

# Install packages  -------------------------------------------------------
library(data.table)
library(tidyverse)
library(tsfeatures)
library(umap)
library(caret)
library(ggpubr) # for retrieving the legend in one of my plots
library(randomForest)
library(rBayesianOptimization)
library(ranger)

# Hardcoded variables -----------------------------------------------------
#base_path <- "D:/KoalaAnalysis/AnalysisV2"
base_path <- "C:/Users/oaw001/Documents/KoalaAnalysis/AnalysisV2"
sample_rate <- 50
available_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z", "Gyroscope.X", "Gyroscope.Y", "Gyroscope.Z")
window_length <- 1
overlap_percent <- 50
sig_individuals <- c("Elsa", "Meeka", "Hardy", "Nicole")

# Load and modify data --------------------------------------------------
# if(file.exists(file.path(base_path, "Data", "CleanLabelledData.csv"))){
#   data <- fread(file.path(base_path, "Data", "CleanLabelledData.csv"))
# } else {
#   data <- fread(file.path(base_path, "Data", "FinalTrainingData.csv")) # data Gabby gave me
#   data <- data %>% rename(Activity = activity,
#                           Time = time,
#                           Accelerometer.X = X_accel,
#                           Accelerometer.Y = Y_accel,
#                           Accelerometer.Z = Z_accel,
#                           Gyroscope.X = X_gyro,
#                           Gyroscope.Y = Y_gyro,
#                           Gyroscope.Z = Z_gyro
#   )
#   # elsa and meeka were collected at 100Hz and all the others were 50 Hz
#   ElsaMeekadata <- data %>% 
#     filter(ID %in% c("Meeka", "Elsa")) %>% 
#     mutate(rownum = row_number()) %>% 
#     filter(rownum %% 2 == 1) %>% 
#     select(-rownum)
#   
#   Otherdata <- data %>% filter(!ID %in% c('Elsa', 'Meeka'))
#   
#   data <- rbind(ElsaMeekadata, Otherdata)
#   
#   fwrite(data, file.path(base_path, "Data", "CleanLabelledData.csv"))
# }

# Split test data out and load other data ---------------------------------
#source(file.path(base_path, "Scripts", "SplitTestData_New.R"))

# Visualise behaviours ----------------------------------------------------
# this will save an html report 
# there are hard coded variables to change in this script

#source(file.path(base_path, "Scripts", "RenderingMarkdown_New.R"))

# Balance samples in training data ----------------------------------------
# Because Meeka and Elsa have so much data, I need to downsample this
# I did this manually, but if you hit source it will just pull in the end result
#source(file.path(base_path, "Scripts", "BalanceOtherData_New.R"))



# Generate features for training data -------------------------------------
# currently set to only process a very small number of windows as it takes forever
#source(file.path(base_path, "Scripts", "GenerateFeatures_New.R"))

# Hyperparmeter Optimisation ----------------------------------------------
source(file.path(base_path, "Scripts", "HPO_New.R"))


