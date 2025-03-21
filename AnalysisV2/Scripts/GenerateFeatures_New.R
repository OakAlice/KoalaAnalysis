
# Generate Features -------------------------------------------------------
# taken directly from my AnomalyDetection OCC work
# functions first and then the code

# main function that calls the others
generateFeatures <- function(window_length, sample_rate, overlap_percent, raw_data, features_type) {
  
  # multiprocessing   
  #plan(multisession, workers = availableCores())  # Use parallel processing 
  
  # Split raw_data by 'ID' # was by = "ID" before
  raw_data_by_id <- split(raw_data, raw_data$ID)
  
  # Process each ID's raw_data
  features_by_id <- list()
  for (id in unique(raw_data$ID)) {
    # I changed the way this was subsetted. Was previously raw_data_by_id[[as.character(id)]]
    features_by_id[[id]] <- processDataPerID(
      id_raw_data = raw_data_by_id[[id]],
      features_type,
      window_length,
      sample_rate,
      overlap_percent
    )
  }
  all_features <- do.call(rbind, features_by_id)
  
  #plan(sequential)  # Return to sequential execution
  
  all_features <- rbindlist(features_by_id)
  
  return(all_features)
}

# Function to process data for each ID
processDataPerID <- function(id_raw_data, features_type, window_length, sample_rate, overlap_percent) {
  
  # Calculate window length and overlap
  samples_per_window <- window_length * sample_rate
  overlap_samples <- if (overlap_percent > 0) ((overlap_percent / 100) * samples_per_window) else 0
  num_windows <- ceiling((nrow(id_raw_data) - overlap_samples) / (samples_per_window - overlap_samples))
  
  # Function to process each window for this specific ID
  process_window <- function(i) {
    print(i)
    start_index <- max(1, round((i - 1) * (samples_per_window - overlap_samples) + 1))
    end_index <- min(start_index + samples_per_window - 1, nrow(id_raw_data))
    window_chunk <- id_raw_data[start_index:end_index, ]
    
    # Initialize output features
    window_info <- tibble(Time = NA, ID = NA, Activity = NA)
    statistical_features <- tibble() 
    single_row_features <- tibble()  
    
    # Extract statistical features
    if ("statistical" %in% features_type) {
      statistical_features <- generateStatisticalFeatures(window_chunk = window_chunk, down_Hz = sample_rate)
    }
    
    # Extract timeseries features and flatten
    if ("timeseries" %in% features_type) {
      time_series_features <- tryCatch({
        generateTsFeatures(data = window_chunk)
      }, error = function(e) {
        message("Error in tsfeatures: ", e$message)
        return(tibble())  # Return an empty tibble on error
      })
      
      if (nrow(time_series_features) > 0) {
        single_row_features <- time_series_features %>%
          ### NOTE: changed this here to add the gyro but didn't test it ####
          mutate(axis = rep(c("Accel.X", "Accel.Y", "Accel.Z", "Gyro.X", "Gyro.Y", "Gyro.Z"), length.out = n())) %>%
          pivot_longer(cols = -axis, names_to = "feature", values_to = "value") %>%
          unite("feature_name", axis, feature, sep = "_") %>%
          pivot_wider(names_from = feature_name, values_from = value)
      } else {
        message("No rows in time_series_features. Returning empty tibble.")
        single_row_features <- tibble(matrix(NA, nrow = 1, ncol = length(unique(paste0(rep(c("X", "Y", "Z"), each = length(time_series_features)), "_", names(time_series_features))))))  # Fill with NAs
        colnames(single_row_features) <- unique(paste0(rep(c("X", "Y", "Z"), each = length(time_series_features)), "_", names(time_series_features)))  # Match the column names
      }
    }
    
    if (nrow(window_chunk) > 0) {
      window_info <- window_chunk %>% 
        summarise(
          Time = first(Time),
          ID = first(ID),
          Activity = if ("Activity" %in% names(.)) {
            as.character(names(sort(table(Activity), decreasing = TRUE))[1])
          } else {
            NA
          }
        ) %>% 
        ungroup()
    }
    
    # Ensure that blank inputs are handled by replacing them with placeholders
    window_info <- if (is.null(window_info) || nrow(window_info) == 0) data.frame(matrix(NA, nrow = 1, ncol = 0)) else window_info
    single_row_features <- if (is.null(single_row_features) || nrow(single_row_features) == 0) data.frame(matrix(NA, nrow = 1, ncol = 0)) else single_row_features
    statistical_features <- if (is.null(statistical_features) || nrow(statistical_features) == 0) data.frame(matrix(NA, nrow = 1, ncol = 0)) else statistical_features
    
    # Combine the data frames
    combined_features <- cbind(window_info, single_row_features, statistical_features) %>%
      mutate(across(everything(), ~replace_na(., NA)))  # Ensure all columns are present
    
    return(combined_features)
  }
  
  # Use lapply to process each window for the current ID
  window_features_list <- lapply(1:num_windows, process_window)
  
  # Combine all the windows for this ID into a single data frame
  features <- bind_rows(window_features_list)
  return(features)
}

# generate time series tsfeatures ####
generateTsFeatures <- function(data) {
  ts_list <- list(
    X = data[["Accelerometer.X"]],
    Y = data[["Accelerometer.Y"]],
    Z = data[["Accelerometer.Z"]]
  )
  
  # List of features to calculate
  features_to_calculate <- c(
    "acf_features", "arch_stat", "autocorr_features", "crossing_points", "dist_features",
    "entropy", "firstzero_ac", "flat_spots", "heterogeneity", "hw_parameters", "hurst",
    "lumpiness", "stability", "max_level_shift", "max_var_shift", "max_kl_shift", 
    "nonlinearity", "pacf_features", "pred_features", "scal_features", "station_features", 
    "stl_features", "unitroot_kpss", "zero_proportion"
  )
  
  # Initialise an empty list to store features
  time_series_features <- list()
  
  # Loop through each feature and calculate it
  for (feature in features_to_calculate) {
    tryCatch({
      feature_values <- tsfeatures(
        tslist = ts_list,
        features = feature,
        scale = FALSE,
        multiprocess = TRUE
      )
      time_series_features[[feature]] <- feature_values
    }, error = function(e) {
      message("Skipping feature ", feature, " due to error: ", e$message)
    })
  }
  
  # Combine all features into a single tibble
  if (length(time_series_features) > 0) {
    time_series_features <- bind_cols(time_series_features)
  } else {
    time_series_features <- tibble()
  }
  
  return(time_series_features)
}

# generate statistical features ####
# Fast Fourier Transformation based features
extractFftFeatures <- function(window_data, down_Hz) {
  n <- length(window_data)
  
  # Compute FFT
  fft_result <- fft(window_data)
  
  # Compute frequencies
  freq <- (0:(n/2 - 1)) * (down_Hz / n)
  
  # Compute magnitude
  magnitude <- abs(fft_result[1:(n/2)])
  
  # Calculate features
  mean_magnitude <- mean(magnitude)
  max_magnitude <- max(magnitude)
  total_power <- sum(magnitude^2)
  peak_frequency <- freq[which.max(magnitude)]
  
  # Return features
  return(list(Mean_Magnitude = mean_magnitude,
              Max_Magnitude = max_magnitude,
              Total_Power = total_power,
              Peak_Frequency = peak_frequency))
}


# making this faster using := which modifies in place rather than copying and modifying
generateStatisticalFeatures <- function(window_chunk, down_Hz) {
  
  result <- data.table()
  
  window_chunk <- setDT(window_chunk)
  
  for (axis in available_axes) {
    axis_data <- window_chunk[[axis]]  # Extract the data for the window
    
    # Compute stats
    stats <- lapply(list(mean = mean, max = max, min = min, sd = sd), 
                    function(f) f(axis_data, na.rm = TRUE))
    
    # Assign stats to result
    result[, paste0(c("mean_", "max_", "min_", "sd_"), axis) := stats]
    
    # Calculate skewness
    result[, paste0("sk_", axis) := e1071::skewness(axis_data, na.rm = TRUE)]
    
    # Extract FFT features
    fft_features <- extractFftFeatures(axis_data, down_Hz)
    
    # Add FFT features to result as well
    result[, paste0(c("mean_mag_", "max_mag_", "total_power_", "peak_freq_"), axis) := 
             list(fft_features$Mean_Magnitude, fft_features$Max_Magnitude, 
                  fft_features$Total_Power, fft_features$Peak_Frequency)]
  }
  
  # calculate SMA, ODBA, and VDBA
  result[, SMA := sum(rowSums(abs(window_chunk[, available_axes, with = FALSE]))) / nrow(window_chunk)]
  ODBA <- rowSums(abs(window_chunk[, available_axes, with = FALSE]))
  result[, `:=`(
    minODBA = min(ODBA, na.rm = TRUE),
    maxODBA = max(ODBA, na.rm = TRUE)
  )]
  VDBA <- sqrt(rowSums(window_chunk[, available_axes, with = FALSE]^2))
  result[, `:=`(
    minVDBA = min(VDBA, na.rm = TRUE),
    maxVDBA = max(VDBA, na.rm = TRUE)
  )]
  
  return(result)
}

# Code here ---------------------------------------------------------------


# Processing features for the training data -------------------------------
if (file.exists(file.path(base_path, "Data", "FeatureOtherData.csv"))){
  print("training features already generated")
} else {
  
  data1 <- fread(file.path(base_path, "Data", "RawOtherData.csv"))
  #data1 <- data1 %>% group_by(Activity, ID) %>% filter(ID == "Elsa") %>% slice(1:100)
  
  for (id in unique(data1$ID)){
    data <- data1 %>% 
      filter(ID == id) %>% 
      filter(!Activity == "") %>% 
      as.data.table()
    
    feature_data_other <- generateFeatures(window_length, sample_rate, overlap_percent, 
                                           raw_data = data, 
                                           features_type = c("statistical", "timeseries"))
    
    fwrite(feature_data_other, file.path(base_path, "Data", paste0(id, "_FeatureOtherData.csv")))
  }
  feature_files <- list.files(file.path(base_path, "Data"), "*_FeatureOtherData.csv", full.names = TRUE) 
  feature_data_other <- lapply(feature_files, read_csv, show_col_types = FALSE)
  feature_data_other <- bind_rows(feature_data_other)
  fwrite(feature_data_other, file.path(base_path, "Data", "FeatureOtherData.csv"))
}


# Processing features for the test data -----------------------------------
if (file.exists(file.path(base_path, "Data", "FeatureTestData_Clusters.csv"))){
  print("test features already generated")
} else {
  
  data1 <- fread(file.path(base_path, "Data", "RawTestData.csv"))
  #data1 <- data1 %>% group_by(Activity, ID) %>% filter(ID == "Elsa") %>% slice(1:100)
  
  for (id in unique(data1$ID)){
    data <- data1 %>% 
      filter(ID == id) %>% 
      filter(!Activity == "") %>% 
      as.data.table()
    
    feature_data_test <- generateFeatures(window_length, sample_rate, overlap_percent, 
                                           raw_data = data, 
                                           features_type = c("statistical", "timeseries"))
    
    fwrite(feature_data_test, file.path(base_path, "Data", paste0(id, "_FeatureTestData.csv")))
  }
  feature_files <- list.files(file.path(base_path, "Data"), "*_FeatureTestData.csv", full.names = TRUE) 
  feature_data_test <- lapply(feature_files, read_csv, show_col_types = FALSE)
  feature_data_test <- bind_rows(feature_data_test)
  
  # apply the same renamings as for the training data
  feature_data_test <- reclustering_behaviours(feature_data_test)
  
  fwrite(feature_data_test, file.path(base_path, "Data", "FeatureTestData_Clusters.csv"))
}

# Processing features for the generalising data -----------------------------------
if (file.exists(file.path(base_path, "Data", "FeatureGenData_Clusters.csv"))){
  print("gen features already generated")
} else {
  
  data1 <- fread(file.path(base_path, "Data", "RawGeneralisationData.csv"))
  #data1 <- data1 %>% group_by(Activity, ID) %>% filter(ID == "Elsa") %>% slice(1:100)
  
  for (id in unique(data1$ID)){
    data <- data1 %>% 
      filter(ID == id) %>% 
      filter(!Activity == "") %>% 
      as.data.table()
    
    feature_data_test <- generateFeatures(window_length, sample_rate, overlap_percent, 
                                          raw_data = data, 
                                          features_type = c("statistical", "timeseries"))
    
    fwrite(feature_data_test, file.path(base_path, "Data", paste0(id, "_FeatureGenData.csv")))
  }
  feature_files <- list.files(file.path(base_path, "Data"), "*_FeatureGenData.csv", full.names = TRUE) 
  feature_data_test <- lapply(feature_files, read_csv, show_col_types = FALSE)
  feature_data_test <- bind_rows(feature_data_test)
  
  # apply the same renamings as for the training data
  feature_data_test <- reclustering_behaviours(feature_data_test)
  
  fwrite(feature_data_test, file.path(base_path, "Data", "FeatureGenData_Clusters.csv"))
}


