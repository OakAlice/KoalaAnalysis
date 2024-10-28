## script using both Oak's and my code to join the relabelled data (Elsa, Meeka),
## and new wild labelled data (Hardy, Nicole) with the original training data 
## containing the rest of the behaviours which weren't relabelled.

## STEP ONE - remove old labels 
originaltrainingdata <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Data for Oak/All of the behaviours - oak/FinalTrainingData.csv", header=T)

## removing the old labelled data for Meeka which I have now relabelled
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Bellowing"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Foraging/Eating"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Branch Walking"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Climbing Up"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Climbing Down"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Meeka" & activity == "Walking"))

## now removing the old labelled data for Elsa which I have now relabelled 
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Elsa" & activity == "Foraging/Eating"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Elsa" & activity == "Branch Walking"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Elsa" & activity == "Climbing Up"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Elsa" & activity == "Climbing Down"))
originaltrainingdata<-subset(originaltrainingdata,!(ID == "Elsa" & activity == "Walking"))

## output to training data file for combining in the next step
write_csv(originaltrainingdata, "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/originaltrainingdata.csv")

### STEP TWO - converting new labelled data from txt to csv
# Cleaning the training data
# for turning MatLab into csv. Not an automated flow. Alter this code to use needs.

#install.packages("pacman")
library(pacman)
p_load("dplyr", "tidyverse", "utils")

options(digits=22) # I want to be able to deal with time precisely

# originally written ####
txt_to_csv_append <- function(input_dir, activity_key_path, output_file) {
  file_paths <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)
  
  all_training_data <- list()
  
  for (file_path in file_paths) {
    # Extract ID from filename
    #file_path <- file_paths[1]
    file_name <- basename(file_path)
    #ID <- sub(".*_([^.]+)\\.txt$", "\\1", file_name) # last word in string
    ID <- sub("^([^_]+)_.*\\.txt$", "\\1", file_name)# first word in string
    
    
    # Read the TXT file
    # change the columns depending on what you have
    training_data <- read_tsv(file_path, col_types = cols(
      #delete = col_double(),
      Time = col_double(),
      X = col_double(),
      GX = col_double(),
      Y = col_double(),
      GY = col_double(),
      Z = col_double(),
      GZ = col_double(),
      Number = col_double()
    ), col_names = c("Time", "X", "Y", "Z", "GX", "GY", "GZ", "Number")) # "GX", "GY", "GZ"
    
    # Add the ID column
    training_data$ID <- ID
    
    # Append to the list
    all_training_data[[length(all_training_data) + 1]] <- training_data
  }
  
  # Combine all DataFrames in the list into one
  combined_data <- bind_rows(all_training_data)
  
  # Load activity key and merge with combined data
  activity_key <- read_csv(activity_key_path, show_col_types = FALSE)
  labelled_data <- left_join(combined_data, activity_key, by = "Number")
  
  # Remove the Number column
  labelled_data <- select(labelled_data, -c(Number))
  
  # Write the combined and labelled data to a CSV file
  #write_csv(labelled_data, output_file)
  #write.csv(combined_data, output_file)
  
  return(labelled_data)
}

# Paths
input_dir <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/txt"
output_file <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/newtrainingdata.csv"
activity_key_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Activity_key.csv"

# Run the function # make new training data
labelled_data <- NULL
labelled_data <- txt_to_csv_append(input_dir, activity_key_path, output_file)

## STEP THREE: combine the two data sets to create FinalTrainingData.csv
colnames(labelled_data) <- c("time", "X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro","ID", "activity")
write.csv(labelled_data, "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/newtrainingdata.csv")

newTD <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/newtrainingdata.csv")
newTD <- subset(newTD, select =-c(X))
oldcleanTD <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/originaltrainingdata.csv")

FinalTrainingData <- rbind(newTD, oldcleanTD)
write_csv(FinalTrainingData, "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FinalTrainingData.csv")

