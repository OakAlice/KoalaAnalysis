# Reformatting data based on the user input variables

format_movement_data <- function(MoveData, columnSubset, num_individuals = NULL, desired_Hz = NULL, current_Hz = NULL, selectedBehaviours) {
  
  # select and rename the relevant columns
  MoveData <- subset_and_rename(MoveData, columnSubset)
  
  # only select the test individuals # this only works if not NA
  if (!is.na(num_individuals) && !is.null(num_individuals)) {
    selected_ids <- unique(MoveData$ID)[1:num_individuals]
    MoveData <- subset(MoveData, ID %in% selected_ids)
  }
  
  # format time
  # relevant only to fractional matlab days
  if (timeFormat == "matlab"){
    MoveData$time <- as.POSIXct((MoveData$time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
  }
  
  # potentially downsample the data
  if (!is.null(desired_Hz) && !is.null(current_Hz)) {
    if (desired_Hz < current_Hz) {
      skip <- current_Hz / desired_Hz
      MoveData <- MoveData[seq(1, nrow(MoveData), by = skip), ]
    } else if (desired_Hz > current_Hz) {
      message("desired_Hz is higher than the current_Hz. Cannot upsample.")
    }
    # If desired_Hz == current_Hz, no action is needed
  } else {
    message("Desired_Hz and current_Hz not both defined.")
  }
  
  # select only the chosen behaviours
  MoveData <- MoveData[MoveData$activity %in% selectedBehaviours, ]
  
  # return the data
  return(MoveData)
}
