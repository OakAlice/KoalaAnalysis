# Balancing the samples from over represented individuals -----------------

if (file.exists(file.path(base_path, "Data", "BalancedOtherData.csv"))){
  balanced_data_other <- fread(file.path(base_path, "Data", "BalancedOtherData.csv"))
} else {
  data1 <- fread(file.path(base_path, "Data", "RawOtherData.csv"))
  
  # visualise the data volumes
  ActivityByID <- plotActivityByID(data = data1, frequency = sample_rate)
  ActivityByID$plot
  
  # visualise particualr behaviours in their entirity to see if they are representitive
  subdata <- data1 %>% filter(ID == "Meeka")
  plot_behaviour(behaviour = "Tree Movement", n_samples = 10000, data = subdata)
  
  
  plot_behaviour <- function(behaviour, n_samples, data) {
    df <- data %>%
      filter(Activity == behaviour) %>%
      group_by(ID, Activity) %>%
      slice(1:n_samples) %>%
      mutate(relative_time = row_number())
    
    # Check if the filtered dataframe is empty
    if (nrow(df) == 0) {
      stop("No data available for behaviour: ", behaviour)
    }
    
    ggplot(df, aes(x = relative_time)) +
      geom_line(aes(y = Accelerometer.X, color = "X"), show.legend = FALSE) +
      geom_line(aes(y = Accelerometer.Y, color = "Y"), show.legend = FALSE) +
      geom_line(aes(y = Accelerometer.Z, color = "Z"), show.legend = FALSE) +
      labs(title = paste(behaviour),
           x = NULL, y = NULL) +
      scale_color_manual(values = c(X = "salmon", Y = "turquoise", Z = "darkblue"), guide = "none") +
      facet_wrap(~ ID, nrow = 1, scales = "free_x") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  
}


data <- data1 %>% slice(1:400)
data$time <- as.POSIXct((data$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
