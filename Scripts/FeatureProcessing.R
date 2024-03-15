# Given the window sizes, overlaps, and list of features specified on the main page
# process the data by that specification

# compute features based on the features list provided
compute_features <- function(window_chunk, featuresList) {
  
  # Determine the available axes from the dataset
  all_axes <- c("X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro")
  available_axes <- intersect(colnames(window_chunk), all_axes) # the ones we actually have
  
  result <- data.frame(row.names = 1)
  
  for (axis in available_axes) {
    
    # axis = "X_accel"
    
    if ("mean" %in% featuresList) {
      result[paste0("mean_", axis)] <- mean(window_chunk[[axis]])
    }
    
    if ("max" %in% featuresList) {
      result[paste0("max_", axis)] <- max(window_chunk[[axis]])
    }
    
    if ("min" %in% featuresList) {
      result[paste0("min_", axis)] <- min(window_chunk[[axis]])
    }
    
    if ("sd" %in% featuresList) {
      result[paste0("sd_", axis)] <- sd(window_chunk[[axis]])
    }
    
    if ("sk" %in% featuresList){
      result[paste0("sk_", axis)] <- e1071::skewness(window_chunk[[axis]], na.rm = TRUE)
    }
  }
  
  accel_axes <- intersect(available_axes, c("X_accel", "Y_accel", "Z_accel"))
  
  if (length(accel_axes) > 0 && ("SMA" %in% featuresList)) {
    result$SMA <- sum(rowSums(abs(window_chunk[, accel_axes]))) / nrow(window_chunk)
  }
  
  if (length(accel_axes) > 0 && ("minODBA" %in% featuresList || "maxODBA" %in% featuresList)) {
    ODBA <- rowSums(abs(window_chunk[, accel_axes]))
    result$minODBA <- min(ODBA)
    result$maxODBA <- max(ODBA)
  }
  
  if (length(accel_axes) > 0 && ("minVDBA" %in% featuresList || "maxVDBA" %in% featuresList)) {
    VDBA <- sqrt(rowSums(window_chunk[, accel_axes]^2))
    result$minVDBA <- min(VDBA)
    result$maxVDBA <- max(VDBA)
  }
  
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
  
  result$activity <- names(which.max(table(window_chunk$activity)))
  result$time <- window_chunk$time[1]
  result$ID <- window_chunk$ID[1]
  
  return(result)
}

process_data <- function(MoveData, featuresList, window, overlap) {
  
  # Initialize an empty list to store the processed data chunks
  processed_windows <- list()
  
  # Update starting and ending points for the next chunk
  window_samples = window * desired_Hz
  
  # Define the starting and ending points for the chunks
  st <- 1
  fn <- st + window_samples -1
  
  # Iterate over the chunks of data
  while (fn <= nrow(MoveData)) {
    
    # Extract the current chunk
    window_chunk <- MoveData[st:fn, ]
    
    # Compute features for the chunk
    features_data <- compute_features(window_chunk, featuresList)
    
    # Add the processed chunk to the list
    processed_windows <- c(processed_windows, list(features_data))
    
    if (overlap == 0) { # if no overlap, advance by a full window length
      st <- st + window_samples
      fn <-  fn + window_samples
    } else { # if there is some overlap, calculate it
      overlapped_samples <- (overlap / 100) * window_samples
      st <- st + (window_samples - overlapped_samples)
      fn <- fn + (window_samples - overlapped_samples)
      
    }
  }
  
  # Combine all the processed chunks into a single data frame
  processed_data <- do.call(rbind, processed_windows)
  
  return(processed_data)
}


key_behaviours <- c("Grooming", "Walking")
extractFeatureInformation <- function(processed_data, key_behaviours){
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
  selected_cols <- grep("X", numeric_cols, value = TRUE)
  selected_cols <- grep("accel", numeric_cols, value = TRUE)
  
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
            panel.grid.minor = element_blank())
    
    plots[[col]] <- p
  }
  
  # Combine all plots into a single image
  multiplot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 5))
  
  return(activityPlot = multiplot)
}

