# Cleaning the training data
# for turning MatLab into csv. Not an automated flow. Alter this code to use needs.

#install.packages("pacman")
library(pacman)
p_load("dplyr", "tidyverse", "utils")

# originally written for Perentie Jordan data ####
txt_to_csv_append <- function(input_dir, output_file) {
  file_paths <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)
  
  all_training_data <- list()
  
  for (file_path in file_paths) {
    # Extract ID from filename
    file_name <- basename(file_path)
    ID <- sub(".*_([^.]+)\\.txt$", "\\1", file_name)
    
    # Read the TXT file
    training_data <- read_tsv(file_path, col_types = cols(
      Time = col_double(),
      X = col_double(),
      Y = col_double(),
      Z = col_double(),
      Number = col_double()
    ), col_names = c("Time", "X", "Y", "Z", "Number"))
    
    # Add the ID column
    training_data$ID <- ID
    
    # Append to the list
    all_training_data[[length(all_training_data) + 1]] <- training_data
  }
  
  # Combine all DataFrames in the list into one
  combined_data <- bind_rows(all_training_data)
  
  # Load activity key and merge with combined data
  activity_key <- read_csv(activity_key_path)
  labelled_data <- left_join(combined_data, activity_key, by = "Number")
  
  # Remove the Number column
  labelled_data <- select(labelled_data, -Number)
  
  # Write the combined and labelled data to a CSV file
  write_csv(labelled_data, output_file)
}

# Paths
input_dir <- "C:/Users/oakle/Documents/PhD docs/Perentie/Training Data"
output_path <- "C:/Users/oakle/Documents/PhD docs/Perentie/TrainingData.csv"
activity_key_path <- "C:/Users/oakle/Documents/PhD docs/Perentie/ActivityKey.csv"

# Run the function
txt_to_csv_append(input_dir, output_path)






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

