# Perform the optimal model


train_optimal_model <- function(formatted_data, featuresList, optimal_window, optimal_overlap, 
                                  threshold, optimal_split, trainingPercentage, validationPercentage, optimal_ntree, test_individuals){
  
  # process the data with these parameters
  processed_data <- process_data(formatted_data, featuresList, optimal_window, optimal_overlap)
  
  # split into training and testing
  list_train_test <- split_condition(processed_data, modelArchitecture, threshold, optimal_split, trainingPercentage, validationPercentage, test_individuals)
  
  # extract the training and testing data
  trDat <- na.omit(list_train_test$train)
  valDat <- na.omit(list_train_test$validate)
  tstDat <- na.omit(list_train_test$test)
  
  # train the RF
  rf_model <- train_rf_model(trDat, optimal_ntree)
  
  return(list(hold_out_data = tstDat, model = rf_model))
}

test_optimal_model <- function(model, tstDat){
  # predict onto the testing data
  test_predictions <- predict_rf_model(model, tstDat)
  
  # extract the actual classes
  test_actual <- factor(tstDat$activity)
  
  # create a confusion matrix
  unique_classes <- union(levels(test_actual), levels(test_predictions))
  confusion_matrix <- table(factor(test_actual, levels = unique_classes), 
                            factor(test_predictions, levels = unique_classes))
  
  # compare the predicted and actual results side by side
  dataframe <- data.frame(Predicted = test_predictions, Actual = test_actual)
  dataframe$Predicted <- as.character(dataframe$Predicted)
  dataframe$Actual <- as.character(dataframe$Actual)
  
  # compare in the form of a plot
  actual_counts <- dataframe %>% 
    count(Actual) %>% 
    rename(Behaviour = Actual) %>% 
    mutate(Type = "Actual")
  
  predicted_counts <- dataframe %>% 
    count(Predicted) %>% 
    rename(Behaviour = Predicted) %>% 
    mutate(Type = "Predicted")
  
  # Combine actual and predicted counts
  combined_counts <- rbind(actual_counts, predicted_counts)
  
  # Plotting
  plot <- ggplot(combined_counts, aes(x = Behaviour, y = n, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Actual" = "cadetblue", "Predicted" = "lightsalmon")) +
    theme_minimal() +
    labs(x = "Class", y = "Count", title = "Actual vs. Predicted Counts per Class") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(confusion = confusion_matrix, plot = plot))
}
