## Plots and stuff for the final version 
plot_confusion <- function(confusion_matrix) {
  # Convert confusion matrix to data frame
  conf_df <- as.data.frame(as.table(confusion_matrix))
  colnames(conf_df) <- c("Actual", "Predicted", "Count")
  conf_df$Count[conf_df$Count == 0] <- NA
  conf_df$Result <- conf_df$Actual == conf_df$Predicted
  conf_df$Result[is.na(conf_df$Count)] <- NA
  
  # Normalize counts within each actual behavior
  conf_df <- conf_df %>%
    group_by(Actual) %>%
    mutate(total_count = sum(Count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Count_norm = Count / total_count)
  
  # Create confusion matrix plot
  confusion_plot <- ggplot(conf_df, aes(y = Predicted, x = Actual, fill = Result, alpha = Count_norm)) +
    geom_tile(color = "white", linewidth = 1) +
    scale_fill_manual(values = c("FALSE" = "salmon", "TRUE" = "steelblue", "NA" = "white"), na.value = "white") +
    scale_alpha_continuous(range = c(0.1, 1)) +  
    theme_minimal() +
    labs(x = "Actual",
         y = "Predicted",
         fill = "Result",
         alpha = "Count") +
    guides(alpha = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(confusion_plot)
}


plot_stacked <- function(confusion_matrix, test_predictions, test_actual) {
  # Convert confusion matrix to data frame
  confusion_df <- as.data.frame(as.table(confusion_matrix))
  names(confusion_df) <- c("Actual", "Predicted", "Count")
  
  # Plot
  custom_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3", "#ff69b4", "#ba55d3", "#cd5c5c", "#ffa07a", "#f08080", "#4682b4", "#20b2aa", "#3A7C75", "#00ff00")
  alternative_plot <- ggplot(confusion_df, aes(x = Predicted, y = Count, fill = Actual)) +
    geom_bar(stat = "identity") +
    labs(x = "Predicted Activity",
         y = "Count") +
    scale_fill_manual(values = custom_palette) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  # and then a visualisation of what was lost into the NAs
  dataframe <- data.frame(Predicted = as.character(test_predictions), Actual = as.character(test_actual))
  NAdataframe_counts <- dataframe %>%
    filter(is.na(Predicted)) %>%
    count(Actual) %>%
    rename(Frequency = n)
  
  # Plot the bar graph
  NAplot <- ggplot(NAdataframe_counts, aes(x = Actual, y = Frequency, fill = Actual)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = custom_palette) +
    labs(x = "Actual behaviour classified as NA", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  return(list(stacked_plot = alternative_plot, 
              Na_leftovers = NAplot))
}


display_metrics <- function(confusion_matrix) {
  TP <- diag(confusion_matrix)
  TN <- sum(confusion_matrix) - rowSums(confusion_matrix) - colSums(confusion_matrix) + 2 * diag(confusion_matrix)
  FP <- colSums(confusion_matrix) - TP
  FN <- rowSums(confusion_matrix) - TP
  
  accuracy <- sum(TP) / sum(confusion_matrix)
  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  F1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  # Calculate specificity for all classes
  num_classes <- ncol(confusion_matrix)
  specificity_all <- numeric(num_classes)
  for (i in 1:num_classes) {
    TN_class <- sum(confusion_matrix) - sum(confusion_matrix[i, ]) - sum(confusion_matrix[, i]) + confusion_matrix[i, i]
    FP_class <- sum(confusion_matrix[, i]) - confusion_matrix[i, i]
    specificity_all[i] <- TN_class / (TN_class + FP_class)
  }
  specificity <- mean(specificity_all)
  
  metrics <- data.frame(General_accuracy = accuracy, Mean_recall = mean(recall), Mean_precision = mean(precision), General_specificity = specificity, Mean_F1 = mean(F1), MCC = mean(MCC))
  
  return(metrics)
}


test_optimal_model <- function(model, tstDat, probabilityReport, probabilityThreshold){
  # predict onto the testing data
  test_actual <- factor(tstDat$activity) # extract the actual names 
  test_predictors <- tstDat %>% # extract the predictors
    ungroup() %>%
    select(-any_of(c("activity", "ID", "row_num", "max_rows"))) %>%
    mutate(across(everything(), as.numeric))
  
  if (probabilityReport == FALSE){
    
    test_predictions <- predict(model, test_predictors) # predict
    
    # convert to chanracter and change formatting
    test_predictions <- as.character(test_predictions)
    test_predictions <- gsub("[/\\.]", " ", test_predictions)
    
    test_predictions <- as.factor(test_predictions)
    
    }else{ # probability reports then get handled differently
    
    test_predictions <- predict(model, newdata = test_predictors, type = "prob")
        # this gives me the probability of all classes. 
        # now I need to extract the class above the probabilityThreshold
  
    test_predictions <- data.frame(test_predictions)
    test_predictions <- test_predictions %>%
      mutate(likelyActivity = apply(test_predictions[, -1], 1, function(x) {
        if (all(is.na(x))) {
          NA
        } else {
          max_index <- which.max(x)
          if (!is.na(max_index) && x[max_index] > probabilityThreshold) {
            names(x)[max_index]
          } else {
            NA
          }
        }
      }))
    
    # Merge predicted probabilities with actual behaviors
    test_predictions <- test_predictions %>%
      mutate(likelyActivity = gsub("\\.", " ", likelyActivity))
    test_predictions <- as.factor(test_predictions$likelyActivity)
  }
  
  # extract the actual classes and change their formatting to match
  test_actual <- as.character(tstDat$activity)
  test_actual <- gsub("[/\\.]", " ", test_actual)
  test_actual <- as.factor(test_actual)
  
  unique_classes <- union(levels(test_actual), levels(test_predictions))
  
  confusion_matrix <- table(factor(test_actual, levels = unique_classes), 
                            factor(test_predictions, levels = unique_classes))
  
  return(test_outputs = list(confusion_matrix = confusion_matrix,
         test_predictions = test_predictions,
         test_actual = test_actual))
}


## apply the optimal settings and verify with graphs, etc.
verify_optimal_results <- function(data, featuresList, window_length, overlap_percent, desired_Hz,
                                   split_method, ntree, threshold, training_percentage, 
                                   validation_percentage, test_individuals, good_individuals,
                                   test_type, probabilityReport, probabilityThreshold) {
  # Process data in parallel
  processed_data <- process_data(data, featuresList, window_length, overlap_percent, desired_Hz)

  # Split data
  list_train_test <- split_condition(processed_data, modelArchitecture, threshold, 
                                     split_method, training_percentage, validation_percentage, 
                                     test_individuals, good_individuals)
  trDat <- na.omit(list_train_test$train)
  
  # Train model
  rf_model <- train_rf_model(trDat, ntree)
  
  # extract the test data type (for controlled comparisons)
  if(test_type == "test") {
    tstDat <- na.omit(list_train_test$test)
  } else if(test_type == "validation") {
    tstDat <- na.omit(list_train_test$validate)
  } else if(test_type == "random") {
    tstDat <- na.omit(list_train_test$test)
    tstDat$activity <- tstDat$activity[sample(nrow(tstDat))]
  }
  
  # Test model
  test_output <- test_optimal_model(rf_model, tstDat, probabilityReport, probabilityThreshold)
  confusion_matrix <- test_output$confusion_matrix
  test_predictions <- test_output$test_predictions
  test_actual <- test_output$test_actual
  
  ## make the plots from the confusion matrix
  confusion_plot <- plot_confusion(confusion_matrix)
  stacked <- plot_stacked(confusion_matrix, test_predictions, test_actual)
  metrics <- display_metrics(confusion_matrix)
  
  testReturns <- list(trained_model = rf_model,
                      confusion_matrix = confusion_matrix, 
                      confusion_plot = confusion_plot, 
                      stacked_plot = stacked$stacked_plot,
                      NA_loss_plot = stacked$Na_leftovers, 
                      metrics = metrics)
}



