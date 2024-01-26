# Cleaning the training data
# Mine is weird, like out of column wack, so have to fix that
# and then want to convert the matlab time to real time
# and then quantify how much we have of each behaviour

install.packages("pacman")
library(pacman)
p_load("dplyr", "tidyverse", "utils")

# Specify the input and output directories
input_dir <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/TrainingData"
output_path <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/TrainingData.csv"
activity_key <- "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/ActivityKey.csv"

# Create the output file and write the header
write_csv(tibble(Time = numeric(), X = numeric(), Y = numeric(), Z = numeric(),
                 GX = numeric(), GY = numeric(), GZ = numeric(), Number = numeric()), 
          output_path, col_names = TRUE)

# Function to reformat and append data to the output file
reformat_and_append_data <- function(file_path, output_file_path) {
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

# Get all file paths in the input directory
file_paths <- list.files(input_dir, full.names = TRUE)

# Apply the function to each file using a for loop
for (file_path in file_paths) {
  reformat_and_append_data(file_path, output_path)
}

# Now replace the activity number with the activity name
# read in the activity key
activity_key <- read.csv(activity_key)
labelled_data <- read.csv(output_path)
labelled_data <- merge(labelled_data, activity_key, by = "Number")
labelled_data <- subset(labelled_data, select = -c(Number))

write_csv(labelled_data, "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/TrainingData2.csv")
