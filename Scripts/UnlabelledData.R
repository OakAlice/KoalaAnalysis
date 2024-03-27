# Assuming that the raw .csv for for each individual is in the folder you specify

# mode function
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]  }
  
# PART ONE: PROCESSING THE RAW DATA ####
  
formattingUnlabelled <- function(unlabelled_file, columnSubsetUnlabelled, current_Hz, optimal_Hz, columnSubset){
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
  # extract the name from the file name
  # save as a column

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
  
  return (list(activity_predictions = activity_predictions,
              details = ID_details))
}


predictionsFile <- function(predictions){
  
  
}



