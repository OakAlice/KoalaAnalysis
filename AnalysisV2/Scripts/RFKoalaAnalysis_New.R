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

# Hardcoded variables -----------------------------------------------------
#base_path <- "D:/KoalaAnalysis/AnalysisV2"
base_path <- "C:/Users/oaw001/Documents/KoalaAnalysis/AnalysisV2"
sample_rate <- 50
available_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z", "Gyroscope.X", "Gyroscope.Y", "Gyroscope.Z")
window_length <- 1
overlap_percent <- 50
sig_individuals <- c("Elsa", "Meeka", "Hardy", "Nicole")

# Load and modify data --------------------------------------------------
if(file.exists(file.path(base_path, "Data", "CleanLabelledData.csv"))){
  data <- fread(file.path(base_path, "Data", "CleanLabelledData.csv"))
} else {
  data <- fread(file.path(base_path, "Data", "FinalTrainingData.csv")) # data Gabby gave me
  data <- data %>% rename(Activity = activity,
                          Time = time,
                          Accelerometer.X = X_accel,
                          Accelerometer.Y = Y_accel,
                          Accelerometer.Z = Z_accel,
                          Gyroscope.X = X_gyro,
                          Gyroscope.Y = Y_gyro,
                          Gyroscope.Z = Z_gyro
  )
  # elsa and meeka were collected at 100Hz and all the others were 50 Hz
  ElsaMeekadata <- data %>% 
    filter(ID %in% c("Meeka", "Elsa")) %>% 
    mutate(rownum = row_number()) %>% 
    filter(rownum %% 2 == 1) %>% 
    select(-rownum)
  
  Otherdata <- data %>% filter(!ID %in% c('Elsa', 'Meeka'))
  
  data <- rbind(ElsaMeekadata, Otherdata)
  
  fwrite(data, file.path(base_path, "Data", "CleanLabelledData.csv"))
}

# Split test data out and load other data ---------------------------------
source(file.path(base_path, "Scripts", "SplitTestData_New.R"))

# Visualise behaviours ----------------------------------------------------
# this will save an html report 
# there are hard coded variables to change in this script

source(file.path(base_path, "Scripts", "RenderingMarkdown_New.R"))

# Balance samples in training data ----------------------------------------
# Because Meeka and Elsa have so much data, I need to downsample this
# I did this manually, but if you hit source it will just pull in the end result
source(file.path(base_path, "Scripts", "BalanceOtherData_New.R"))



# Generate features for training data -------------------------------------
# currently set to only process a very small number of windows as it takes forever
source(file.path(base_path, "Scripts", "GenerateFeatures_New.R"))





# group behaviours into generalised groups
data[, GeneralisedActivity := fifelse( # exclude bellowing from model and cluster other activities
  Activity %in% c("Branch Walking", "Tree Movement", "Swinging/Hanging"), "TreeMovement",
  fifelse(Activity %in% c("Climbing up", "Rapid Climbing", "Climbing Down"), "Climbing",
          fifelse(Activity %in% c("Foraging/Eating"), "Feed",
                  fifelse(Activity == "Tree Sitting", "TreeSitting", 
                          fifelse(Activity == "Ground Sitting", "GroundSitting",
                                  fifelse(Activity %in% c("Sleeping/Resting"), "Still",
                                          fifelse(Activity %in% c("Shake", "Grooming"), "Groom", 
                                                  fifelse(Activity %in% c("Walking", "Bound/Half-Bound"), "Walking", "NA")
                                          )))))))]












# Process into features -----------------------------------------------------

if (file.exists( file.path(base_path, "Data", "Feature_data", paste0(dataset_name, "_multi_other_features.csv")))) {
  feature_data <- fread(file.path(base_path, "Data", "Feature_data", paste0(dataset_name, "_multi_other_features.csv")))
  
} else {
  
  # extract the appropriate window_length from the dictionary above
  window_length <- behaviour_lengths[[dataset_name]][["multi"]]
  
  data <- fread(file.path(base_path, "Data", "Hold_out_test", paste0(dataset_name, "_other.csv")))
  
  for (id in unique(data$ID)) {
    dat <- data %>% filter(ID == id) %>% arrange(Time)
    
    feature_data <-
      generateFeatures(
        window_length,
        sample_rate,
        overlap_percent,
        raw_data = dat,
        features = features_type
      )
    
    # save it 
    fwrite(feature_data, file.path(base_path, "Data", "Feature_data", paste0(dataset_name, "_", id, "_multi_features.csv")))
  }
  
  # stitch all the id feature data back together
  files <- list.files(file.path(base_path, "Data/Feature_data"), pattern = "*.csv", full.names = TRUE)
  pattern <- paste0(dataset_name, ".*", "multi")
  matching_files <- grep(pattern, files, value = TRUE)
  
  feature_data_list <- lapply(matching_files, read.csv)
  feature_data <- do.call(rbind, feature_data_list)
  
  # save this as well
  fwrite(feature_data, file.path(base_path, "Data", "Feature_data", paste0(dataset_name, "_multi_features.csv")))
}













# Plot Functions ---------------------------------------------------------------


plotTraceExamples <- function(behaviours, data, individuals, n_samples, n_col) {
  
  data <- data %>% filter(ID %in% sample(unique(data$ID), individuals))
  
  # Create plots for each behavior (with error catching)
  plots <- purrr::map(behaviours, function(behaviour) {
    tryCatch(
      {
        plot_behaviour(behaviour, n_samples, data)
      },
      error = function(e) {
        message("Skipping plot for ", behaviour, ": ", e$message)
        NULL  # Return NULL to indicate skipping
      }
    )
  })
  
  # Remove NULL plots (for behaviors with no data)
  plots <- purrr::compact(plots)
  
  # Combine plots into a single grid
  grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = n_col)
  
  return(grid_plot)
}

# Function to create the plot for each behavior
plot_behaviour <- function(behaviour, n_samples, data) {
  
  df <- data %>%
    filter(GeneralisedActivity == behaviour) %>%
    group_by(ID, GeneralisedActivity) %>%
    slice(1:n_samples) %>%
    mutate(relative_time = row_number())
  
  # Check if the filtered dataframe is empty
  if (nrow(df) == 0) {
    stop("No data available for behaviour: ", behaviour)
  }
  
  ggplot(df, aes(x = relative_time)) +
    geom_line(aes(y = Accelerometer.X, color = "X"), show.legend = FALSE) +
    geom_line(aes(y = Accelerometer.Y, color = "Y"), show.legend = FALSE) +
    geom_line(aes(y = Accelerometer.Z, color = "Z"), show.legend = FALSE) +
    labs(title = paste(behaviour),
         x = NULL, y = NULL) +
    scale_color_manual(values = c(X = "salmon", Y = "turquoise", Z = "darkblue"), guide = "none") +
    facet_wrap(~ ID, nrow = 1, scales = "free_x") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
}

## set colours ####
generate_random_colors <- function(n) {
  colors <- rgb(runif(n), runif(n), runif(n))
  return(colors)
}


## total volume by Activity and ID ####
plotActivityByID <- function(data, frequency, colours) {
  my_colours <- generate_random_colors(colours)
  # summarise into a table
  labelledDataSummary <- data %>%
    count(ID, GeneralisedActivity)
  
  # account for the HZ, convert to minutes
  labelledDataSummaryplot <- labelledDataSummary %>%
    mutate(minutes = (n/frequency)/60)
  
  # Plot the stacked bar graph
  plot_activity_by_ID <- ggplot(labelledDataSummaryplot, aes(x = GeneralisedActivity, y = minutes, fill = as.factor(ID))) +
    geom_bar(stat = "identity") +
    labs(x = "Activity",
         y = "minutes") +
    theme_minimal() +
    scale_fill_manual(values = my_colours) +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(plot_activity_by_ID)
}
