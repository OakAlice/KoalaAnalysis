
# This is me exploring anew -----------------------------------------------

library(data.table)
library(tidyverse)

base_path <- "C:/Users/oaw001/Documents/KoalaAnalysis/AnalysisV2"

# load in her data
data <- fread(file.path(base_path, "FinalTrainingData.csv"))
data <- data %>% rename(Activity = activity,
                        Time = time,
                        Accelerometer.X = X_accel,
                        Accelerometer.Y = Y_accel,
                        Accelerometer.Z = Z_accel
                        )

# explore it very basically

sum <- data %>% group_by(ID, activity) %>% count()
# rename these groups

data[, GeneralisedActivity := fifelse( # exclude bellowing from model and cluster other activities
  Activity %in% c("Branch Walking", "Climbing up", "Rapid Climbing", "Tree Movement", "Climbing Down", "Swinging/Hanging"), "TreeMovement",
  fifelse(Activity %in% c("Forging/Eating"), "Feed",
          fifelse(Activity == "Tree Sitting", "TreeSitting", 
            fifelse(Activity == "Ground Sitting", "GroundSitting",
              fifelse(Activity %in% c("Sleeping/Resting"), "Still",
                      fifelse(Activity %in% c("Shake", "Grooming"), "Groom", 
                              fifelse(Activity %in% c("Walking", "Bound/Half-Bound"), "Walking", NA)
                    ))))))]

behaviours <- unique(data$GeneralisedActivity)
individuals <- length(unique(data$ID))
n_samples <- 250
n_col <- 3
frequency <- 50

plotTraceExamples(behaviours, data, individuals, n_samples, n_col = 2)
plotActivityByID(data, frequency, colours = individuals)







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
    #filter(!Activity %in% ignore_behaviours) %>%
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
