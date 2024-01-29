# Applying the Random Forest

# PART ONE: Training
# select the predictors and the target, train with variable ntree number
train_rf_model <- function(trDat, trees) {

  predictors <- trDat %>% 
    ungroup() %>%
    select(-activity) %>%
    mutate(across(everything(), as.numeric))
  
  target <- factor(trDat$activity)
  
  rf_model <- randomForest(x = predictors, y = target, ntree = trees, importance = TRUE)
  return(rf_model)
}

# PART TWO: TESTING
# prepare the test data in the same way and then extract the test data
predict_rf_model <- function(rf_model, tstDat) {

  test_predictors <- tstDat %>%
    ungroup() %>%
    select(-activity) %>%
    mutate(across(everything(), as.numeric))
  
  test_predictions <- predict(rf_model, test_predictors)
  return(test_predictions)
}

# PART THREE: EVALUATE RESULTS
# do the test, and extract the results

evaluate_rf_model <- function(test_predictions, tstDat) {
  test_actual <- factor(tstDat$activity)
  unique_classes <- union(levels(test_actual), levels(test_predictions))
  
  confusion_matrix <- table(factor(test_actual, levels = unique_classes), 
                            factor(test_predictions, levels = unique_classes))
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)
  # Calculate specificity
  num_classes <- ncol(confusion_matrix)
  specificity <- numeric(num_classes)
  for (i in 1:num_classes) {
    TN <- sum(confusion_matrix) - sum(confusion_matrix[i, ]) - sum(confusion_matrix[, i]) + confusion_matrix[i, i]
    FP <- sum(confusion_matrix[, i]) - confusion_matrix[i, i]
    specificity[i] <- TN / (TN + FP)
  }
  specificity <- mean(specificity)
  
  F1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  metrics_df <- data.frame(accuracy, recall, precision, specificity, F1)
  
  return(metrics_df)
}

save_rf_model <- function(
    rf_model, metrics_df, ExperimentNumber, test_individuals, 
    desired_Hz, selectedBehaviours, featuresList, threshold, window_length, 
    overlap_percent, split, trees
) {
  # Extract model metrics
  num_variables_split <- rf_model$mtry
  oob_error <- rf_model$err.rate[which.max(rf_model$err.rate[, "OOB"]), "OOB"]
  accuracy <- metrics_df$accuracy[1]
  precision <- mean(metrics_df$precision, na.rm = TRUE)
  recall <- mean(metrics_df$recall, na.rm = TRUE)
  specificity <- metrics_df$specificity[1]
  F1 <- mean(metrics_df$F1, na.rm = TRUE)
  
  # Create summary dataframe
  summary_df <- data.frame(
    # experiment variables
    ExperimentNumber = ExperimentNumber, 
    TestIndividuals = test_individuals, 
    DesiredHz = desired_Hz,
    numBehaviours = length(selectedBehaviours),
    NumFeatures = length(featuresList),
    Balancing = threshold, 
    WindowLength = window_length, 
    OverlapPercent = overlap_percent, 
    SplitMethod = split,
    # model variables
    ntree = trees, 
    NumVariablesSplit = num_variables_split, 
    # performance variables
    OOBEstimate = oob_error, 
    Accuracy = accuracy, 
    Recall = recall, 
    Precision = precision, 
    Specificity = specificity, 
    F1Score = F1
  )
  
  
  return(summary_df)
}