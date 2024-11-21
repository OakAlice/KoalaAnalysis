# evaluation of validation
# PART THREE: EVALUATE RESULTS
# do the test, and extract the results for the overall performance as well as the target behaviours
# target behaviours set to NULL as default

evaluate_model <- function(test_predictions, tstDat, target_behaviours = NULL) {
  
  test_actual <- factor(tstDat$activity)
  unique_classes <- union(levels(test_actual), levels(test_predictions))
  
  confusion_matrix <- table(factor(test_actual, levels = unique_classes), 
                            factor(test_predictions, levels = unique_classes))
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)
  
  # Calculate specificity for all classes
  num_classes <- ncol(confusion_matrix)
  specificity_all <- numeric(num_classes)
  for (i in 1:num_classes) {
    TN <- sum(confusion_matrix) - sum(confusion_matrix[i, ]) - sum(confusion_matrix[, i]) + confusion_matrix[i, i]
    FP <- sum(confusion_matrix[, i]) - confusion_matrix[i, i]
    specificity_all[i] <- TN / (TN + FP)
  }
  specificity <- mean(specificity_all)
  
  F1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  # Calculate Matthews Correlation Coefficient (MCC)
  TP <- diag(confusion_matrix)
  FP <- colSums(confusion_matrix) - TP
  FN <- rowSums(confusion_matrix) - TP
  TN <- sum(confusion_matrix) - TP - FP - FN
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  metrics <- data.frame(
    General_accuracy = accuracy, 
    Mean_recall = mean(recall), 
    Mean_precision = mean(precision), 
    General_specificity = specificity, 
    Mean_F1 = mean(F1),
    Mean_MCC = mean(mcc)
  )
  
  # If target_behaviours are specified, calculate metrics for them
  if (!is.null(target_behaviours)) {
    for (behaviour in target_behaviours) {
      behaviour_index <- which(unique_classes == behaviour)
      if (length(behaviour_index) > 0) {
        TP <- confusion_matrix[behaviour_index, behaviour_index]
        FN <- sum(confusion_matrix[behaviour_index, ]) - TP
        FP <- sum(confusion_matrix[, behaviour_index]) - TP
        TN <- sum(confusion_matrix) - TP - FP - FN
        behaviour_accuracy <- (TP + TN) / (TP + TN + FP + FN)
        behaviour_recall <- TP / (TP + FN)
        behaviour_precision <- TP / (TP + FP)
        behaviour_F1 <- 2 * (behaviour_precision * behaviour_recall) / (behaviour_precision + behaviour_recall)
        behaviour_mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
        
        metrics[[paste(behaviour, "accuracy", sep = "_")]] <- behaviour_accuracy
        metrics[[paste(behaviour, "recall", sep = "_")]] <- behaviour_recall
        metrics[[paste(behaviour, "precision", sep = "_")]] <- behaviour_precision
        metrics[[paste(behaviour, "F1", sep = "_")]] <- behaviour_F1
        metrics[[paste(behaviour, "MCC", sep = "_")]] <- behaviour_mcc
      }
    }
  }
  
  return(metrics)
}

# PART FOUR: SAVE ALL THE RESULTS
save_model_results <- function(
    metrics_df, dataset_tag, train_individuals, num_behs, 
    downsampling_Hz, window, overlap, normalisation, selection, featuresList, 
    threshold, trees_number, kernel, cost) {
  
  # Prepare hyperparameters
  hyperparameters <- list(
    Dataset = as.character(dataset_tag),
    Hz = as.numeric(downsampling_Hz),
    individuals = as.numeric(train_individuals),
    number_behaviours = as.numeric(num_behs),
    number_features = as.numeric(length(featuresList)),
    balanced_threshold = as.numeric(threshold),
    window_length = as.numeric(window),
    overlap_percent = as.numeric(overlap),
    feature_normalised = as.character(normalisation),
    feature_selection = as.character(selection),
    ntree = as.numeric(trees_number),
    kernel = as.character(kernel),
    cost = as.numeric(cost)
  )
  
  # Ensure dynamic metrics are coerced to numeric where possible, else character
  results_metrics <- lapply(metrics_df[1, ], function(x) {
    numeric_x <- suppressWarnings(as.numeric(x))
    if(any(!is.na(numeric_x))) {
      return(numeric_x)
    } else {
      # Return NA if conversion to numeric fails
      return(NA)
    }
  })
  
  # Combine static and dynamic metrics
  final_metrics_list <- c(hyperparameters, results_metrics) 
  
  # Convert the combined list to a dataframe ensuring no list-type columns
  summary_df <- setNames(data.frame(matrix(unlist(final_metrics_list), nrow=1, byrow=TRUE), stringsAsFactors=FALSE), names(final_metrics_list))
  
  return(summary_df)
}
