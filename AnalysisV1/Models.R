# Applying the Random Forest

# select the predictors and the target, train with variable ntree number
train_model <- function(trDat, model_architecture,
                           num_trees = NULL, # RF
                           kernel = NULL, cost = NULL, # SVM
                           hidden = NULL # MLP
                        ) {

  predictors <- trDat %>% 
    ungroup() %>%
    select(-any_of(c("activity", "ID", "max_rows"))) %>%
    mutate(across(everything(), as.numeric))
  
  target <- factor(trDat$activity)
  numberOfClasses <- length(unique(target))
  
  if (model_architecture == "RF"){
      model <- randomForest::randomForest(x = predictors, y = target, ntree = num_trees, importance = TRUE)
  
  } else if (model_architecture == "SVM"){
      model <- e1071::svm(x = predictors, y = target, kernel = kernel, cost = cost)
      
  } else if (model_architecture == "kNN"){
      skip() # does it all in one step in the test section
    
  } else if (model_architecture == "DT") {
      model <- rpart::rpart(formula = target ~ ., data = predictors)
      
  } else if (model_architecture == "XGB") {
      skip()
      
  } else if (model_architecture == "MLP") {
      #model <- neuralnet::neuralnet(target ~ ., data = predictors, hidden = hidden, linear.output = FALSE)
      # taking too long, haven't tested
      skip()
  }

  return(model)
}

# PART TWO: TESTING
# prepare the test data in the same way and then extract the test data
predict_model <- function(model, model_architecture, testingData) {
  
  test_predictors <- testingData %>%
    ungroup() %>%
    select(-any_of(c("activity", "ID", "row_num"))) %>%
    mutate(across(everything(), as.numeric))
  
  if (model_architecture == "RF" | model_architecture == "SVM"){
    test_predictions <- predict(model, test_predictors)
    
  } else if (model_architecture == "kNN"){
    test_predictions <- class::knn(train = predictors, test = test_predictors, cl = target, k = k)
    
  } else if (model_architecture == "XGB") {
    skip() # couldnt figure it out yet
    
  } else if (model_architecture == "DT") {
    test_predictions <- rpart::predict(model, newdata = test_predictors)
    
  } else if (model_architecture == "MLP") {
    #test_predictions <- neuralnet::compute(model, test_predictors)
    skip()
  }
  
  
  return(test_predictions)
}
