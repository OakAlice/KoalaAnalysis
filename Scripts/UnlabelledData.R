# Assuming that the raw .csv for for each individual is in the folder you specify

# mode function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]  }
  
# PART ONE: PROCESSING THE RAW DATA ####
  
formattingUnlabelled <- function(unlabelled_file, ID_name, columnSubsetUnlabelled, current_Hz, optimal_Hz, columnSubset){
  # select and rename the relevant columns
  MoveData <- subset_and_rename(unlabelled_file, columnSubsetUnlabelled)

  # potentially downsample the data
  if (!is.null(optimal_Hz) && !is.null(current_Hz)) {
    if (optimal_Hz < current_Hz) {
      skip <- current_Hz / optimal_Hz
      MoveData <- MoveData[seq(1, nrow(MoveData), by = skip), ]
    } else if (optimal_Hz > current_Hz) {
      message("optimal_Hz is higher than the current_Hz. Cannot upsample.")
    }
    # If optimal_Hz == current_Hz, no action is needed
  } else {
    message("optimal_Hz and current_Hz not both defined.")
  }
  
  # format time
  if (timeFormat == "matlab"){
    MoveData$time <- as.POSIXct((MoveData$time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
  }
  
  # add the ID column
  MoveData$ID <- ID_name

  # return the data
  return(MoveData)
}
  
# PART TWO: DIRECT DECODING #### 
predictingUnlabelled <- function(processed_file, OptimalMLModel){
  
  unlabelled_predictors <- processed_file %>%
    ungroup() %>%
    select(-any_of(c("ID", "time"))) %>%
    mutate(across(everything(), as.numeric))
  
  ID_details <- processed_file %>%
    ungroup() %>%
    select(any_of(c("ID", "time")))
  
  activity_predictions <- predict(OptimalMLModel, unlabelled_predictors) # random forest
  
  predict_file <- cbind(activity_predictions, ID_details)
  
  return (list(predict_file = predict_file,
              activity_predictions = activity_predictions,
              details = ID_details))
}


# Summarise into behaviour windows ####
summarise <- function(predict_file, summarisation_window) {
  # Convert timestamp to POSIXct format
  predict_file$timestamp <- as.POSIXct(predict_file$time, format = "%Y-%m-%d %H:%M:%S")
  
  # Calculate the start and end times for each summarisation window
  predict_file <- predict_file %>%
    mutate(window_start = floor_date(timestamp, unit = paste(summarisation_window, "min")),
           window_end = ceiling_date(timestamp, unit = paste(summarisation_window, "min")))
  
  # Group data by summarisation window and activity
  summarised_data <- predict_file %>%
    group_by(window_start, window_end, activity_predictions) %>%
    mutate(count = n()) %>%
    arrange(window_start, window_end, desc(count)) %>%
    group_by(window_start, window_end) %>%
    slice(1) %>%
    ungroup()
  
  summarised_data$timestamp <- format(summarised_data$window_start, "%H:%M:%S")
  summarised_data$day <- as.Date(summarised_data$window_start)
  summarised_data <- summarised_data %>% select(-c(window_start, window_end, count))
  
  return(summarised_data)
}



behaviour_grid <- function(prediction_outcome){
  
  ## VISUALISE IT
  custom_palette <- c("#ff69b4", "#ba55d3", "#8da0cb", "#66c2a5", "#3A7C75", "#f08080", "#ffa07a", "#cd5c5c", "#e78ac3", "#e5c494", "#ffd92f", "#fc8d62")
  
  # order them so midnight is in the middle
  before_midnight <- sort(unique(prediction_outcome$timestamp[prediction_outcome$timestamp < "12:00:00"]))
  after_midnight <- sort(unique(prediction_outcome$timestamp[prediction_outcome$timestamp >= "12:00:00"]))
  ordered_timestamps <- c(after_midnight, before_midnight)
  prediction_outcome$timestamps <- factor(prediction_outcome$timestamp, levels = ordered_timestamps)
  
  # plot it
  p <- ggplot(prediction_outcome, aes(x = timestamps, y = day, fill = activity_predictions)) + 
    geom_tile() +
    scale_fill_manual(values = custom_palette) +
    labs(y = "Day", x = "Timestamp", fill = "Activity") +
    scale_x_discrete(breaks = ordered_timestamps[seq(1, length(ordered_timestamps), by = 240)]) +
    scale_y_date(breaks = unique(prediction_outcome$day))+
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0.5), 
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5)
    )
  
  return (p)
}




#### PLOT THE UNLABELLED TRACE ####

plotTrace <- function(combined_data){
  
  trace <- ggplot(combined_data, aes(x = time)) +
    geom_line(aes(y = X_accel, color = "X_accel"), show.legend = FALSE) +
    geom_line(aes(y = Y_accel, color = "Y_accel"), show.legend = FALSE) +
    geom_line(aes(y = Z_accel, color = "Z_accel"), show.legend = FALSE) +
    labs(x = "Time") +
    scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"), guide = "none") +
    theme_minimal() +
    theme(panel.grid = element_blank())

  return(trace)
}