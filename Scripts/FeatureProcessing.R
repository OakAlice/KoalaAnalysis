# Given the window sizes, overlaps, behaviours, and list of features specified on the main page
# process the data by that specification


# relabel the single behaviours into generalised classes
relabel_activities <- function(formatted_data, relabelledBehaviours){
  relabelled_data <- formatted_data %>%
    mutate(activity = sapply(activity, function(act) {
      for (new_activity in names(relabelledBehaviours)) {
        if (act %in% relabelledBehaviours[[new_activity]]) {
          return(new_activity)
        }
      }
      return(as.character(act))
    }))
  
  return(relabelled_data)
}

# specific functions for some specific variables
calculate_autocorrelation <- function(axis_data, lag) {
  acf_result <- acf(axis_data, lag.max = lag, plot = FALSE)
  autocorrelation <- acf_result$acf[lag + 1]
  return(autocorrelation)
}

calculate_entropy <- function(axis_data) {
  freq <- table(axis_data) / length(axis_data)
  entropy <- -sum(freq * log2(freq))
  return(entropy)
}

calculate_zero_crossing <- function(window_data) {
  # Calculate the sign of each element in the windowed data
  signs <- sign(window_data) # as in literally whether its positive or negative
  # Calculate the number of zero crossings by counting sign changes
  zero_crossings <- sum(abs(diff(signs)) > 0)
  return(zero_crossings)
}

compute_features <- function(window_chunk, features_list) {
  
  # Determine the available axes from the dataset
  all_axes <- c("X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro")
  available_axes <- intersect(colnames(window_chunk), all_axes) # the ones we actually have
  
  result <- data.frame(row.names = 1)
  
  for (axis in available_axes) {
    
    # axis = "X_accel"
    
    if ("mean" %in% features_list) {
      result[paste0("mean_", axis)] <- mean(window_chunk[[axis]])
    }
    
    if ("max" %in% features_list) {
      result[paste0("max_", axis)] <- max(window_chunk[[axis]])
    }
    
    if ("min" %in% features_list) {
      result[paste0("min_", axis)] <- min(window_chunk[[axis]])
    }
    
    if ("sd" %in% features_list) {
      result[paste0("sd_", axis)] <- sd(window_chunk[[axis]])
    }
    
    if ("sk" %in% features_list){
      result[paste0("sk_", axis)] <- e1071::skewness(window_chunk[[axis]], na.rm = TRUE)
    }
    if ("entropy" %in% features_list){
      result[paste0("entropy_", axis)] <- calculate_entropy(window_chunk[[axis]])
    }
    if ("auto" %in% features_list){
      result[paste0("auto_", axis)] <- calculate_autocorrelation(window_chunk[[axis]])
    }
    if ("zero" %in% features_list){
      result[paste0("zero_", axis)] <- calculate_zero_crossing(window_chunk[[axis]])
    }
  }
  
  accel_axes <- intersect(available_axes, c("X_accel", "Y_accel", "Z_accel"))
  
  if (length(accel_axes) > 1 && ("SMA" %in% features_list)) {
    result$SMA <- sum(rowSums(abs(window_chunk[, accel_axes]))) / nrow(window_chunk)
  }
  
  if (length(accel_axes) > 1 && ("minODBA" %in% features_list || "maxODBA" %in% features_list)) {
    ODBA <- rowSums(abs(window_chunk[, accel_axes]))
    result$minODBA <- min(ODBA)
    result$maxODBA <- max(ODBA)
  }
  
  if (length(accel_axes) > 1 && ("minVDBA" %in% features_list || "maxVDBA" %in% features_list)) {
    VDBA <- sqrt(rowSums(window_chunk[, accel_axes]^2))
    result$minVDBA <- min(VDBA)
    result$maxVDBA <- max(VDBA)
  }
  
  if (length(accel_axes) > 1 && ("cor" %in% features_list)) {
    for (i in 1:(length(accel_axes) - 1)) {
      for (j in (i + 1):length(accel_axes)) {
        axis1 <- accel_axes[i]
        axis2 <- accel_axes[j]
        
        vec1 <- window_chunk[[axis1]]
        vec2 <- window_chunk[[axis2]]
        
        # Check for NA variance and non-zero variance in both vectors
        var_vec1 <- var(vec1, na.rm = TRUE)
        var_vec2 <- var(vec2, na.rm = TRUE)
        
        if (!is.na(var_vec1) && var_vec1 != 0 && !is.na(var_vec2) && var_vec2 != 0) {
          # Check for complete cases
          complete_cases <- complete.cases(vec1, vec2)
          if (any(complete_cases)) {
            result[paste0("cor_", axis1, "_", axis2)] <- cor(vec1[complete_cases], vec2[complete_cases])
          } else {
            result[paste0("cor_", axis1, "_", axis2)] <- NA  # No complete pairs
          }
        } else {
          result[paste0("cor_", axis1, "_", axis2)] <- NA  # No variability or NA returned
        }
      }
    }
  }
  
  result$activity <- names(which.max(table(window_chunk$activity)))
  result$time <- window_chunk$time[1]
  result$ID <- window_chunk$ID[1]
  
  return(result)
}

process_data <- function(relabelled_data, features_list, window_length, overlap_percent, down_Hz, feature_normalisation) {
  # this section will be done with parallel processing
  processed_windows <- list()
  
  # activate the cores
  num_cores <- detectCores()
  cl <- makeCluster(num_cores-2)
  registerDoParallel(cl)
  clusterExport(cl, c("calculate_autocorrelation", "calculate_entropy", "calculate_zero_crossing",
                      "compute_features"))
  
  # Calculate window size in samples
  window_samples <- window_length * down_Hz
  
  # Calculate overlap size in samples
  overlap_samples <- if (overlap_percent > 0) (ceiling(overlap_percent / down_Hz)/10 * window_samples) else 0
  
  # Initialize an empty list to store the processed data chunks
  processed_windows <- foreach(st = seq(1, nrow(relabelled_data), by = (window_samples - overlap_samples)),
                               .combine = 'rbind') %dopar% {
                                 fn <- min(st + window_samples - 1, nrow(relabelled_data))
                                 window_chunk <- relabelled_data[st:fn, ]
                                 compute_features(window_chunk, features_list)
                               }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine all the processed chunks into a single data frame
  processed_data <- data.frame(processed_windows)
  
  # normalisation,if selected
  features_to_normalise <- setdiff(colnames(processed_data), c("time", "ID", "activity"))
  if (feature_normalisation == "MinMaxScaling") {
    # Normalize the selected columns
    processed_data[features_to_normalise] <- lapply(processed_data[features_to_normalise], function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
  } else if (feature_normalisation == "Standarisation") {
    processed_data[features_to_normalise] <- lapply(processed_data[features_to_normalise], function(x) {
      (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })
  }
  
  return(processed_data)
}
