# Cleaning the training data
# for turning MatLab into csv. Not an automated flow. Alter this code to use needs.

#install.packages("pacman")
library(pacman)
p_load("dplyr", "tidyverse", "utils")

options(digits=22) # I want to be able to deal with time precisely

# originally written for Perentie Jordan data ####
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
  labelled_data <- select(labelled_data, -Number)
  
  # Write the combined and labelled data to a CSV file
  write_csv(labelled_data, output_file)
  
  return(labelled_data)
}

# Paths
input_dir <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/txt"
output_file <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FinalTrainingData.csv"
activity_key_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Activity_key.csv"


# Run the function
labelled_data <- txt_to_csv_append(input_dir, activity_key_path, output_file)


# select only some specific individuals
#filtered_data <- labelled_data[labelled_data$ID %in% c("Meeka", "Elsa"), ]

# balance it down so reasonable counts
filtered_data <- playData %>%
  rename(activity = Activity, time = Time)
balance_data <- balance_ID_data(filtered_data, 200000) # change activity for Activity if necessary
balance_data <- balance_data %>%
  select(-n, -over_threshold)

balance_data <- balance_data %>%
  #rename(Time = time) %>%
  mutate(ID = ifelse(ID == "ELsa", "Elsa", ID))


output_file <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/AllIndBalancedTrainingData.csv"
write_csv(balance_data, output_file)



### ADDING NEW DATA FOR GABBY ####
# her original data
original_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FinalTrainingData.csv"
original <- read.csv(original_path)
  # remove the bad behaviours
  original <- original %>% filter(!(activity %in% c("Bellowing","Foraging/Eating", "Branch Walking","Climbing Up","Climbing Down","Walking")))
  original <- original %>% select(-row_num)
  
# the new training data in a csv
new_training_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FixedTrainingData.csv"
new_training <- read.csv(new_training_path)
  # rename the columns 
  column_subset = c("Time" = "time",                                      # training data
                  "X" = "X_accel", "Y" = "Y_accel", "Z" = "Z_accel",
                  "GX" = "X_gyro", "GY" = "Y_gyro", "GZ" = "Z_gyro",
                  "ID" = "ID", "Activity" = "activity")
  new_training <- rename(new_training, !!!setNames(names(new_training), column_subset))

# the new wild data is still as individual files
wild_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/txt"
activity_key_path <- "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Activity_key.csv"
  files <- list.files(wild_path, full.names = TRUE, pattern = "\\.csv$") # list all csv files in the directory
  
  wild_data <- data.frame()
  
  for (file in files) {
    file1 <- read.csv(file, header = TRUE, col.names = c("time", "X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro", "activity")) # read in the csv file
    ID <- sub("^([^_]+)_.*\\.csv$", "\\1", basename(file)) # extract the first word in the file as the ID column
    file1$ID <- ID
    wild_data <- rbind(wild_data, file1) # append the data to wild_data
  }
  # correct the activity labels into words
  activity_key <- read_csv(activity_key_path, show_col_types = FALSE)
  activity_key$activity <- activity_key$Number
  activity_key <- activity_key %>% select(-Number)
  wild_data <- left_join(wild_data, activity_key, by = "activity")
  wild_data <- wild_data %>% select(-c("Activity.y", "Number", "activity"))
  wild_data$activity <- wild_data$Activity.x
  wild_data <- wild_data %>% select(-c("Activity.x"))

# now feed them all together
  NewTrainingData <- rbind(original, new_training, wild_data)

unique(NewTrainingData$activity)
output_file <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/FinalTrainingData.csv"
write_csv(NewTrainingData, output_file)



# Niche stuff for my koala data ####

# Create the output file and write the header
write_csv(tibble(Time = numeric(), X = numeric(), Y = numeric(), Z = numeric(),
                 #GX = numeric(), GY = numeric(), GZ = numeric(), 
                 Activity = numeric(), ID = character()), 
          output_path, col_names = TRUE)

# Function to reformat and append data to the output file # this is specific to just my koala data
reformat_oak_koala <- function(file_path, output_file_path) {
  # Read the data and split into individual values
  all_values <- read_lines(file_path) %>%
    str_split("\\s+", simplify = FALSE) %>%
    unlist() %>%
    # Remove empty strings and NA values
    .[. != "" & !is.na(.)]
  
  # Calculate the total number of complete rows (8 values per row)
  num_complete_rows <- length(all_values) %/% 8
  
  # Reshape into a matrix with 8 columns
  reshaped_data <- matrix(all_values[1:(num_complete_rows * 8)], ncol = 8, byrow = TRUE)
  
  # Append the reshaped data to the output CSV file
  write_csv(as_tibble(reshaped_data), output_file_path, append = TRUE, col_names = FALSE)
}

