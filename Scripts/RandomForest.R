# Applying the Random Forest

# PART ONE: Training
# select the predictors and the target, train with variable ntree number
train_rf_model <- function(trDat, ntree) {
  library(randomForest)
  library(dplyr)
  
  predictors <- trDat %>% 
    ungroup() %>%
    select(-activity) %>%
    mutate(across(everything(), as.numeric))
  
  target <- factor(trDat$activity)
  
  rf_model <- randomForest(x = predictors, y = target, ntree = ntree, importance = TRUE)
  return(rf_model)
}

# PART TWO: TESTING
# prepare the test data in the same way and then extract the test data
predict_rf_model <- function(rf_model, tstDat) {
  library(dplyr)
  
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
  
  confusion_matrix <- table(test_actual, test_predictions)
  accuracy <- mean(test_predictions == test_actual)
  
  cat("Confusion Matrix:\n")
  print(confusion_matrix)
  cat("\nAccuracy:", accuracy, "\n")
}
