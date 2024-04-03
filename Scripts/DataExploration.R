## Basic Data Exploration
# before beginning to build to model, generally explore the data

### PART ONE: MAKE A PLOT ####
my_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#e49e18", 
               "#ffd92f", "#e5c494", "#b3b3b3", "#ff69b4", "#ba55d3", "#3fd7af")

exploreData <- function(Experiment_path, formatted_data, ignoreBehaviours) {
  # summarise into a table
  labelledDataSummary <- formatted_data %>%
    filter(!activity %in% ignoreBehaviours) %>%
    count(ID, activity)
  
  # account for the HZ, convert to seconds
  labelledDataSummaryplot <- labelledDataSummary %>%
    mutate(seconds = n/current_Hz)
  
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
plot_behaviours <- function(behaviourList, formatted_data, Experiment_path, n_samples, n_col) {
  # Function to create the plot for each behavior
  plot_behaviour <- function(behaviour, n_samples) {
    df <- formatted_data %>%
      filter(activity == behaviour) %>%
      group_by(ID, activity) %>%
      slice(1:n_samples) %>%
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
  plots <- purrr::map(behaviourList, ~ plot_behaviour(.x, n_samples))
  
  # Combine plots into a single grid
  grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = n_col)
  
  # Save the grid plot
  #ggsave(file.path(Experiment_path, "behaviours_grid_plot.png"), plot = grid_plot)
  
  return(grid_plot)
}

# looking at feature information
extractFeatureInformation <- function(processed_data, key_behaviours, number_columns){
  summary <- processed_data %>%
    filter(activity %in% key_behaviours) %>%
    group_by(activity, ID) %>%
    summarise(across(where(is.numeric), 
                     list(max = ~max(.), min = ~min(.), mean = ~mean(.), var = ~var(.)),
                     .names = "{col}_{fn}"))
  
  
  my_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#e49e18", 
                  "#ffd92f", "#e5c494", "#b3b3b3", "#ff69b4", "#ba55d3", "#3fd7af")
  
  
  numeric_cols <- colnames(processed_data)[!colnames(processed_data) %in% c("activity", "ID")]
  numeric_cols <- numeric_cols[numeric_cols != "time"]
  #selected_cols <- grep("X", numeric_cols, value = TRUE)
  #selected_cols <- grep("accel", numeric_cols, value = TRUE)
  selected_cols <- numeric_cols 
  
  plots <- list()
  
  # Loop over each numeric column and create a plot
  for (col in selected_cols) {
    p <- ggplot(summary, aes_string(x = "activity", y = paste0(col, "_mean"), color = "as.factor(ID)")) +
      geom_point(position = position_jitterdodge(jitter.width = 0.2), size = 3) +
      geom_errorbar(aes_string(ymin = paste0(col, "_min"), ymax = paste0(col, "_max")),
                    position = position_jitterdodge(jitter.width = 0.2),
                    width = 0.4) +
      labs(title = paste(col)) +
      scale_color_manual(values = my_colours, name = "ID") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),  # Remove y-axis labels
            panel.border = element_rect(color = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 8))
    
    plots[[col]] <- p
  }
  
  # Combine all plots into a single image
  multiplot <- do.call(gridExtra::grid.arrange, c(plots, ncol = number_columns))
  
  return(activityPlot = multiplot)
}


# similar to above but more detailed
plotFeatureInformation <- function(processed_data, key_behaviours, number_columns){
  reduced_data <- processed_data %>%
    filter(activity %in% key_behaviours)
  
  my_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#e49e18", 
                  "#ffd92f", "#e5c494", "#7a42f4", "#ff69b4", "#ba55d3", "#3fd7af")
  
  numeric_cols <- colnames(processed_data)[!colnames(processed_data) %in% c("activity", "ID")]
  numeric_cols <- numeric_cols[numeric_cols != "time"]
  selected_cols <- grep("X", numeric_cols, value = TRUE)
  selected_cols <- grep("accel", selected_cols, value = TRUE)
  selected_cols <- selected_cols 
  
  plots <- list()
  
  # Loop over each numeric column and create a plot
  for (col in selected_cols) {
    p <- ggplot(reduced_data, aes_string(x = "activity", y = paste0(col), color = "as.factor(ID)")) +
      geom_point(position = position_jitterdodge(jitter.width = 0.1), size = 3, alpha = 0.5) +
      labs(title = paste(col)) +
      scale_color_manual(values = my_colours, name = "ID") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 8))
    
    plots[[col]] <- p
  }
  
  # Create a separate plot for the legend
  legend_plot <- ggplot(reduced_data, aes(x = "activity", fill = as.factor(ID))) + 
    geom_bar() +
    scale_fill_manual(values = my_colours, name = "ID")
  leg <- get_legend(legend_plot)
  leg <- as_ggplot(leg)
  
  # Combine all plots into a single image
  multiplot <- do.call(gridExtra::grid.arrange, c(plots[selected_cols], ncol = number_columns))
  multiplot_legend <- cowplot::plot_grid(multiplot, leg, ncol = 2, rel_widths = c(1, 0.2))
  
  return(multiplot_legend)
}




# plotting the behaviours over the top of eachother
plotFeatureFrequency <- function(processed_data, key_behaviours, number_columns){
  reduced_data <- processed_data %>%
    filter(activity %in% key_behaviours)
  
  my_colours <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#e49e18", 
                  "#ffd92f", "#e5c494", "#7a42f4", "#ff69b4", "#ba55d3", "#3fd7af")
  
  numeric_cols <- colnames(processed_data)[!colnames(processed_data) %in% c("activity", "ID")]
  numeric_cols <- numeric_cols[numeric_cols != "time"]
  #selected_cols <- grep("X", numeric_cols, value = TRUE)
  selected_cols <- grep("gyro", numeric_cols, value = TRUE)
  selected_cols <- selected_cols 
  
  reduced_data <- reduced_data %>% # make it numeric
    mutate(across(all_of(selected_cols), as.numeric))
  
  plots <- list()
  
  # Loop over each numeric column and create a plot
  for (col in selected_cols) {
    #col <- selected_cols[1]
    p <- ggplot(reduced_data, aes_string(x = paste0(col), color = "as.factor(activity)")) +
      geom_freqpoly(aes(y = ..density..), binwidth = 0.2, linewidth = 1) +
      labs(title = paste(col)) +
      scale_color_manual(values = my_colours, name = "ID") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size = 5))
    
    plots[[col]] <- p
  }
  
  # Create a separate plot for the legend
  legend_plot <- ggplot(reduced_data, aes(x = "activity", fill = as.factor(activity))) + 
    geom_bar() +
    scale_fill_manual(values = my_colours, name = "ID")
  leg <- get_legend(legend_plot)
  leg <- as_ggplot(leg)
  
  # Combine all plots into a single image
  multiplot <- do.call(gridExtra::grid.arrange, c(plots[selected_cols], ncol = number_columns))
  multiplot_frequency <- cowplot::plot_grid(multiplot, leg, ncol = 2, rel_widths = c(1, 0.1))
  
  return(multiplot_frequency)
}