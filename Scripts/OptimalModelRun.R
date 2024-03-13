# Perform the optimal model


train_optimal_model <- function(formatted_data, featuresList, optimal_window, optimal_overlap, 
                                  threshold, optimal_split, trainingPercentage, validationPercentage, optimal_ntree, test_individuals, good_individuals){
  
  # process the data with these parameters
  processed_data <- process_data(formatted_data, featuresList, optimal_window, optimal_overlap)
  
  # split into training and testing
  list_train_test <- split_condition(processed_data, modelArchitecture, threshold, optimal_split, trainingPercentage, validationPercentage, test_individuals, good_individuals)
  
  # extract the training and testing data
  trDat <- na.omit(list_train_test$train)
  valDat <- na.omit(list_train_test$validate)
  tstDat <- na.omit(list_train_test$test)
  
  trDat <- trDat %>%
    ungroup() %>%
    select(-any_of(c("ID", "row_num")))
  
  # train the RF
  rf_model <- train_rf_model(trDat, optimal_ntree)
  
  return(list(hold_out_data = tstDat, model = rf_model))
}



test_optimal_model <- function(model, tstDat, probabilityReport, probabilityThreshold){
  # predict onto the testing data
  
  # whether you want normal classification or probability reported
  if (probabilityReport == FALSE){
    test_predictions <- predict_rf_model(model, tstDat)
    # convert to chanracter and change formatting
    test_predictions <- as.character(test_predictions)
    test_predictions <- gsub("[/\\.]", " ", test_predictions)
    
    test_predictions <- as.factor(test_predictions)
    
    }else{ # probability reports then get handled differently
    
    test_predictions <- predict(model, newdata = tstDat, type = "prob")
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
  
  
  # plot the confusion matrix
  
  # change it into a table
  conf_df <- as.data.frame(as.table(confusion_matrix))
  colnames(conf_df) <- c("Actual", "Predicted", "Count")
  conf_df$Count[conf_df$Count == 0] <- NA
  conf_df$Result <- conf_df$Actual == conf_df$Predicted
  conf_df$Result[is.na(conf_df$Count)] <- NA
  
  # normalise this within each of the actual behaviours
  
  conf_df <- conf_df %>%
    group_by(Actual) %>%
    mutate(total_count = sum(Count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Count_norm = Count / total_count)
  
  confusion_matrix_plot <- ggplot(conf_df, aes(y = Predicted, x = Actual, fill = Result, alpha = Count_norm)) +
    geom_tile(color = "white", size = 1) +  # Add black border
    scale_fill_manual(values = c("FALSE" = "salmon", "TRUE" = "steelblue", "NA" = "white"), na.value = "white") +
    scale_alpha_continuous(range = c(0.1, 1)) +  
    theme_minimal() +
    labs(x = "Actual",
         y = "Predicted",
         fill = "Result",
         alpha = "Count") +
    guides(alpha = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # here's another plot that better shows me the breakdown
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
  
  return(list(confusion = confusion_matrix, confusion_plot = confusion_matrix_plot, 
              plot_pred_act = plot, plot_stacked = alternative_plot,
              NA_loss_plot = NAplot))
}



