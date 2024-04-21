# Applying the Random Forest

# select the predictors and the target, train with variable ntree number
train_rf_model <- function(trDat, trees) {

  predictors <- trDat %>% 
    ungroup() %>%
    select(-any_of(c("activity", "ID", "max_rows"))) %>%
    mutate(across(everything(), as.numeric))
  
  target <- factor(trDat$activity)
  
  rf_model <- randomForest(x = predictors, y = target, ntree = trees, importance = TRUE)
  return(rf_model)
}

# PART TWO: TESTING
# prepare the test data in the same way and then extract the test data
predict_rf_model <- function(rf_model, testingData) {

  test_predictors <- testingData %>%
    ungroup() %>%
    select(-any_of(c("activity", "ID", "row_num"))) %>%
    mutate(across(everything(), as.numeric))
  
  test_predictions <- predict(rf_model, test_predictors)
  return(test_predictions)
}