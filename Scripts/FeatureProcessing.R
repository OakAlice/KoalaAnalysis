# Given the window sizes, overlaps, and list of features specified on the main page
# process the data by that specification

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


compute_features <- function(window_chunk, featuresList) {
  
  # Determine the available axes from the dataset
  all_axes <- c("X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro")
  available_axes <- intersect(colnames(window_chunk), all_axes) # the ones we actually have
  
  result <- data.frame(row.names = 1)
  
  # Compute statistics for each axis
  for (axis in available_axes) {
    axis_data <- window_chunk[[axis]]
    result[paste0("mean_", axis)] <- mean(axis_data)
    result[paste0("max_", axis)] <- max(axis_data)
    result[paste0("min_", axis)] <- min(axis_data)
    result[paste0("sd_", axis)] <- sd(axis_data)
    result[paste0("sk_", axis)] <- e1071::skewness(axis_data, na.rm = TRUE)
    result[paste0("RMS_", axis)] <- sqrt(abs(mean(axis_data, na.rm = TRUE)))
    result[paste0("auto_", axis)] <- calculate_autocorrelation(axis_data, 20)
    result[paste0("entropy_", axis)] <- calculate_entropy(axis_data)
    result[paste0("zero_", axis)] <- calculate_zero_crossing(axis_data)
  }
  
  # Compute SMA, minODBA, maxODBA, minVDBA, maxVDBA
  accel_axes <- intersect(available_axes, c("X_accel", "Y_accel", "Z_accel"))
  if (length(accel_axes) > 0) {
    result$SMA <- sum(rowSums(abs(window_chunk[, accel_axes]))) / nrow(window_chunk)
    ODBA <- rowSums(abs(window_chunk[, accel_axes]))
    result$minODBA <- min(ODBA)
    result$maxODBA <- max(ODBA)
    VDBA <- sqrt(rowSums(window_chunk[, accel_axes]^2))
    result$minVDBA <- min(VDBA)
    result$maxVDBA <- max(VDBA)
  }
  
  # Compute correlations
  if ("cor" %in% featuresList) {
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
  
  
  # Add activity, ID, and time
  result$activity <- names(which.max(table(window_chunk$activity)))
  result$time <- window_chunk$time[1]
  result$ID <- window_chunk$ID[1]
  
  return(result)
}



process_data <- function(formatted_data, featuresList, window_length, overlap_percent, desired_Hz) {
  # this section will be done with parallel processing
  
  # activate the cores
  num_cores <- detectCores()
  cl <- makeCluster(num_cores-2)
  registerDoParallel(cl)
  clusterExport(cl, c("calculate_autocorrelation", "calculate_entropy", "calculate_zero_crossing",
                      "compute_features"))
  
  # Calculate window size in samples
  window_samples <- window_length * desired_Hz
  
  # Calculate overlap size in samples
  overlap_samples <- if (overlap_percent > 0) (overlap_percent / desired_Hz) * window_samples else 0
  
  # Initialize an empty list to store the processed data chunks
  processed_windows <- foreach(st = seq(1, nrow(formatted_data), by = window_samples - overlap_samples),
                               .combine = 'rbind') %dopar% {
                                 fn <- min(st + window_samples - 1, nrow(formatted_data))
                                 window_chunk <- formatted_data[st:fn, ]
                                 compute_features(window_chunk, featuresList)
                               }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine all the processed chunks into a single data frame
  processed_data <- do.call(rbind, processed_windows)
  processed_data <- data.frame(processed_data)
  
  # Transpose the processed_data dataframe and rename the columns
  transposed_data <- t(processed_data)
  colnames(transposed_data) <- rownames(processed_data)
  rownames(transposed_data) <- NULL
  processed_data <- data.frame(transposed_data)
  
  return(processed_data)
}
