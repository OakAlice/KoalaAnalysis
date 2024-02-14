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
    #file_path <- file_paths[1]
    file_name <- basename(file_path)
    # ID <- sub(".*_([^.]+)\\.txt$", "\\1", file_name) # last word in string
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
    ), col_names = c("Time", "X", "Y", "Z", "GX", "GY", "GZ", "Number"))
    
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
input_dir <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/TxtFiles"
output_file <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/TrainingData.csv"
activity_key_path <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/Activity_Key.csv"

# Run the function
labelled_data <- txt_to_csv_append(input_dir, output_file)


# select only some specific individuals
filtered_data <- labelled_data[labelled_data$ID %in% c("Meeka", "Elsa"), ]

# visualise the class imbalance you have 
ggplot(filtered_data, aes(x = Activity)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Activities",
       x = "Activity",
       y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# balance it down so reasonable counts
balance_data <- balance_data(filtered_data, 40000) # change activity for Activity if necessary
output_file <- "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/ProperTrainingData2.csv"
write_csv(balance_data, output_file)




# balance data
balance_data <- function(dat, threshold) {
  #dat <- processed_data
  
  # Determine counts of each 'Activity' and identify over-represented behaviors
  Activity_counts <- dat %>% 
    group_by(Activity) %>%
    tally() %>%
    mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
  
  # For over-represented behaviors, sample the desired threshold number of rows or all if less
  oversampled_data <- dat %>% 
    inner_join(Activity_counts %>% filter(n > threshold), by = "Activity") %>%
    group_by(Activity) %>%
    sample_n(size = min(over_threshold[1], n()), replace = FALSE) 
  
  # For other behaviors, take all rows
  undersampled_data <- dat %>% 
    anti_join(filter(Activity_counts, n > threshold), by = "Activity")
  
  # Combine and return
  balance_data <- bind_rows(oversampled_data, undersampled_data)
  return(balance_data)
}


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

