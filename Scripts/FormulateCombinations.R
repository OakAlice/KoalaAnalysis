## Formulate all possible options

# create all options of the model specific hyperparameters
expand_hyperparameters <- function(model_hyperparameters_list) {
  hyperparameters_df <- data.frame(model_architecture = character(0))

  # Iterate over each model hyperparameters
  for (model_name in names(model_hyperparameters_list)) {
    model_hyperparameters <- model_hyperparameters_list[[model_name]]
    param_names <- names(model_hyperparameters)
    all_combinations <- expand.grid(model_hyperparameters)
    num_combinations <- nrow(all_combinations)
    
    # Create a dataframe for the current model's hyperparameters
    model_hyperparameters_df <- data.frame(matrix(rep(NA, length(param_names) * num_combinations), nrow = num_combinations))
    colnames(model_hyperparameters_df) <- param_names
    for (i in 1:length(param_names)) {
      model_hyperparameters_df[, param_names[i]] <- all_combinations[[i]]
    }
    model_hyperparameters_df$model_architecture <- model_name

    # Merge with existing hyperparameters dataframe
    hyperparameters_df <- merge(hyperparameters_df, model_hyperparameters_df, by = "model_architecture", all = TRUE)
  }

  return(hyperparameters_df)
}

# bind this with the normal parameter options
create_extended_options <- function(model_hyperparameters_list, options_df) {
  # Expand hyperparameters for each model
  all_hyperparameters_df <- expand_hyperparameters(model_hyperparameters_list)

  # Merge only the model_architectures that appear in options_df
  extended_options_df <- merge(
    options_df,
    all_hyperparameters_df[all_hyperparameters_df$model_architecture %in% unique(options_df$model_architecture), ],
    by = "model_architecture",
    all = TRUE
  )

  extended_options_df
}
