
# Testing optimal models --------------------------------------------------
# Function to apply column selection changes to both training and testing data
update_feature_data <- function(data, multi) {
  
  cols_to_remove <- c("Activity", "GeneralisedActivity")
  # classes to remove logic
 if (multi == "GeneralisedActivity") {
    col_to_rename <- "GeneralisedActivity"
  } else if (multi == "Activity") {
    col_to_rename <- "Activity"
  }
  
  data <- data %>% select(-(setdiff(cols_to_remove, col_to_rename))) %>%
    rename(Activity = col_to_rename)
  
  return(data)
}

# load in the best parameters (presuming you made them into a csv)
hyperparamaters <- fread(file.path(base_path, "OptimalHyperparameters.csv"))

# Iterate through each activity in hyperparameters
for (i in seq_len(nrow(hyperparamaters))) {
  parameter_row <- hyperparamaters[i, ]
  
  # Load and process training data
  training_data <- fread(file.path(base_path, "Data", "FeatureOtherData_Clusters.csv")) %>%
    update_feature_data(parameter_row$Behaviour) %>%
    filter(Activity != "") %>%
    select(-Time, -ID)
  
  clean_cols <- removeBadFeatures(training_data, var_threshold = 0.5, corr_threshold = 0.9)
  clean_feature_data <- training_data %>%
    select(all_of(c(clean_cols, "Activity"))) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity))
  
  # Train the model
  RF_model <- ranger(
    dependent.variable.name = "Activity",
    data = clean_feature_data,
    num.trees = parameter_row$number_trees,
    mtry = parameter_row$mtry,
    max.depth = parameter_row$max_depth,
    classification = TRUE,
    importance = "impurity"
  )
  
  saveRDS(RF_model, file = file.path(base_path, "Output", paste0(parameter_row$Behaviour, "_model.rds")))
  
  # Process testing data
  clean_testing_data <- fread(file.path(base_path, "Data", "FeatureTestData_Clusters.csv")) %>%
    update_feature_data(parameter_row$Behaviour) %>%
    filter(Activity != "") %>%
    select(all_of(colnames(clean_feature_data))) %>%
    na.omit()
  
  numeric_testing_data <- as.matrix(clean_testing_data[, !names(clean_testing_data) %in% c("Activity", "ID"), with = FALSE])
  ground_truth_labels <- factor(clean_testing_data$Activity)
  
  if (anyNA(numeric_testing_data)) message("Validation data contains missing values!")
  
  # Make predictions
  predictions <- predict(RF_model, data = numeric_testing_data)
  predicted_classes <- factor(predictions$predictions, levels = levels(ground_truth_labels))
  
  # Compute confusion matrix
  confusion_matrix <- table(predicted_classes, ground_truth_labels)
  all_classes <- union(levels(predicted_classes), levels(ground_truth_labels))
  conf_matrix_padded <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                               dimnames = list(all_classes, all_classes))
  conf_matrix_padded[rownames(confusion_matrix), colnames(confusion_matrix)] <- confusion_matrix
  
  # Calculate F1 score
  confusion_mtx <- confusionMatrix(conf_matrix_padded)
  
  # Extract precision, recall, and F1-score
  metrics <- data.frame(
    Behaviour = rownames(confusion_mtx$byClass),  # Behavior names
    Precision = confusion_mtx$byClass[, "Precision"],
    Recall = confusion_mtx$byClass[, "Recall"],
    F1 = confusion_mtx$byClass[, "F1"],
    Accuracy = confusion_mtx$byClass[, "Balanced Accuracy"],
    Prevelance = confusion_mtx$byClass[, "Prevalence"] * length(predicted_classes)
  )
  
  # Add macro-averaged metrics as the last row
  metrics <- rbind(
    metrics,
    data.frame(
      Behaviour = "Macro-Average",
      Precision = mean(metrics$Precision, na.rm = TRUE),
      Recall = mean(metrics$Recall, na.rm = TRUE),
      F1 = mean(metrics$F1, na.rm = TRUE),
      Accuracy = mean(metrics$Accuracy, na.rm = TRUE),
      Prevelance = NA
    )
  )
  
  # Write to CSV
  write.csv(metrics, file = file.path(base_path, "Output", paste0(parameter_row$Behaviour, "_performance_metrics.csv")), row.names = FALSE)
}
