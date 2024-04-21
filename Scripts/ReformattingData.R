# Reformatting data based on the user input variables

format_movement_data <- function(data, columnSubset, timeFormat, num_individuals = NULL, current_Hz = NULL, desired_Hz = NULL) {
  
  # select and rename the relevant columns
  data <- subset_and_rename(data, columnSubset)
  
  # only select the test individuals # this only works if not NA
  if (!is.na(num_individuals) && !is.null(num_individuals)) {
    selected_ids <- unique(data$ID)[1:num_individuals]
    data <- subset(data, ID %in% selected_ids)
  }
  
  # format time
  # relevant only to fractional matlab days
  if (timeFormat == "matlab"){
    data$time <- as.POSIXct((data$time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
  }
  
  # potentially downsample the data
  if (!is.null(desired_Hz) && !is.null(current_Hz)) {
    if (desired_Hz < current_Hz) {
      skip <- current_Hz / desired_Hz
      data <- data[seq(1, nrow(data), by = skip), ]
    } else if (desired_Hz > current_Hz) {
      message("desired_Hz is higher than the current_Hz. Cannot upsample.")
    }
    # If desired_Hz == current_Hz, no action is needed
  } else {
    message("Desired_Hz and current_Hz not defined.")
  }
  
  # return the data
  return(data)
}
