## Basic Data Exploration
# before beginning to build to model, generally explore the data
# in the process of automating this 

### PART ONE: MAKE A PLOT ####
my_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#e49e18", 
               "#ffd92f", "#e5c494", "#b3b3b3", "#ff69b4", "#ba55d3", "#3fd7af")

exploreData <- function(Experiment_path, formatted_data, ignoreBehaviours) {
  # summarise into a table
  labelledDataSummary <- formatted_data %>%
    filter(!activity %in% ignoreBehaviours) %>%
    group_by(ID, activity) %>%
    summarise(count = n())
  
  # account for the HZ, convert to seconds
  labelledDataSummaryplot <- labelledDataSummary %>%
    mutate(seconds = count/current_Hz)
  
  # Plot the stacked bar graph
  behaviourIndividualDistribution <- ggplot(labelledDataSummaryplot, aes(x = activity, y = seconds, fill = ID)) +
    geom_bar(stat = "identity") +
    labs(x = "Activity",
         y = "Seconds") +
    theme_minimal() +
    scale_fill_manual(values = my_colours) +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # save the image
  ggsave(file.path(Experiment_path, "behaviourIndividualDistribution.png"), plot = behaviourIndividualDistribution)
}


# PART TWO: DISPLAYING SAMPLES OF EACH TRACE TYPE ####
behaviourList <- GabbyKoalaBehaviours
formatted_data <- formatted_data
plotBehaviouralSamples <- function(behaviourList, formatted_data, Experiment_path, n_samples) {
  # Function to create the plot for each behavior
  plot_behaviour <- function(behaviour, n_samples) {
    df <- formatted_data %>%
      filter(activity == behaviour) %>%
      group_by(ID, activity) %>%
      slice_head(n = n_samples) %>%
      mutate(relative_time = row_number())
    
    
    ggplot(df, aes(x = relative_time)) +
      geom_line(aes(y = X_accel, color = "X_accel"), show.legend = FALSE) +
      geom_line(aes(y = Y_accel, color = "Y_accel"), show.legend = FALSE) +
      geom_line(aes(y = Z_accel, color = "Z_accel"), show.legend = FALSE) +
      labs(title = paste(behaviour, "Samples"),
           x = "Relative Time") +
      scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"), guide = "none") +
      facet_wrap(~ ID, nrow = 1, scales = "free_x") +
      theme_minimal() +
      theme(panel.grid = element_blank())
  }
  
  # Create plots for each behavior
  plots <- purrr::map(behaviourList, ~ plot_behaviour(.x))
  
  # Combine plots into a single grid
  grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = 4)
  
  # Save the grid plot
  ggsave(file.path(Experiment_path, "behaviours_grid_plot.png"), plot = grid_plot)
}




# just to make one specific behaviour
n_samples <- 200
purrr::map("Climbing Up", ~ plot_behaviour(.x, n_samples))

