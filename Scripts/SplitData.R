# Code to create the training and testing data, saving them both as .rda files

# process the data
split_condition <- function(processed_data, modelArchitecture, threshold, split, 
                            trainingPercentage, validationPercentage, test_individuals) {
  
  dat <- processed_data %>% na.omit()
  #dat <- balance_data(dat, threshold) # balancing currently bad
  
  remove_columns <- function(df) {
    df %>% select(-any_of(c("time", "n", "X", "over_threshold")))
  }
  
  testingPercentage <- 1- trainingPercentage - validationPercentage
  prop <- c(trainingPercentage, validationPercentage, testingPercentage)  # Proportions for each group
  
  if (split == "random") {
    # set up valiables
    n <- nrow(dat)
    
    # Generate random indices for every row
    indices <- sample(1:3, n, replace = TRUE, prob = prop)
    
    # Split data based on those random indices
    trDat <- dat[indices == 1, ]
    valDat <- dat[indices == 2, ]
    tstDat <- dat[indices == 3, ]
    
    trDat <- remove_columns(trDat)
    valDat <- remove_columns(valDat)
    tstDat <- remove_columns(tstDat)
    
  } else if (split == "chronological") {
    if ("ID" %in% colnames(dat)) { 
        # sort by ID and time
      sorted_dat <- arrange(dat, ID, time)
      # Get unique individuals
      individuals <- unique(sorted_dat$ID)
      
      # blank dataframes
      trDat <- data.frame()
      valDat <- data.frame()
      tstDat <- data.frame()
      
      # for each of the individuals, select the individual's data
      for (i in 1:length(individuals)) {
        ind_data <- sorted_dat[sorted_dat$ID == individuals[i], ]
        
        # given the total rows for that individual
        n_ind <- nrow(ind_data)
        # Calculate rows per proportion
        ind_sample_sizes <- floor(prop * n_ind)
        
        ind_trDat <- ind_data[0:ind_sample_sizes[1], ] # the first proportion
        ind_valDat <- ind_data[ind_sample_sizes[1] : (ind_sample_sizes[1] +ind_sample_sizes[2]), ] # middle prop
        ind_tstDat <- ind_data[(n_ind - ind_sample_sizes[3]):n_ind, ] # remainder prop
        
        trDat <- rbind(ind_trDat, trDat)
        valDat <- rbind(ind_valDat, valDat)
        tstDat <- rbind(ind_tstDat, tstDat)
      }
      
      trDat <- remove_columns(trDat)
      valDat <- remove_columns(valDat)
      tstDat <- remove_columns(tstDat)
    
      } else { # when there is no ID
        dat <- dat %>% arrange(time)
        sample_sizes <- floor(nrow(dat) * prop)
        
        trDat <- dat[0:sample_sizes[1], ] %>% remove_columns() 
        valDat <- dat[sample_sizes[1] : (sample_sizes[1] +sample_sizes[2]), ]%>% remove_columns()
        tstDat <- dat[(length(dat$ID) - sample_sizes[3]):length(dat$ID), ]%>% remove_columns()
      }
    
  } else if (split == "LOIO") {
    # Ensure test_individuals is provided for LOIO split
    if (is.null(test_individuals)) {
      stop("test_individuals must be provided for LOIO split")
    } else if (test_individuals<3){
      stop("test_individuals must be greater than 3 for this condition")
    }
    
    # calculating  number of individuals in labelled sets # must be whole number, cant be 0, must add to total
    # Calculate the approximate 
    approx_individuals <- ceiling(test_individuals * prop)
    
    # Adjust any rounded 0s to be 1 - minimum necessary
    approx_individuals[approx_individuals == 0] <- 1
    
    # Adjust the largest group to meet logical conditions
    largest <- which.max(approx_individuals)
    approx_individuals[largest] <- ifelse(sum(approx_individuals) != test_individuals,
                                   approx_individuals[largest] + (test_individuals - sum(approx_individuals)),
                                   approx_individuals)
    
    # select which of the individuals will be in each group
    individuals <- unique(dat$ID)
    trainingIndividuals <- sample(individuals, size = approx_individuals[1])
    remaining_individuals <- setdiff(individuals, trainingIndividuals)
    validationIndividuals <- sample(remaining_individuals, size = approx_individuals[2])
    
    # save these as data groups
    trDat <- dat %>% filter(ID %in% trainingIndividuals) %>% remove_columns()
    valDat <- dat %>% filter(ID %in% validationIndividuals) %>% remove_columns()
    tstDat <- dat %>% filter(!(ID %in% trainingIndividuals | ID %in% validationIndividuals)) %>% remove_columns()
  }
  
  # Formatting the data for the SOM
  trSamp2 <- function(x) {
    # Assuming columns 2 to 21 are the relevant features
    features <- x[,2:21]
    activities <- as.factor(x$activity)
    list(measurements = as.matrix(features), activity = activities)
  }
  
  # Save data based on model architecture
  if (modelArchitecture == "SOM") {
    trDat <- trSamp2(trDat)
    tstDat <- trSamp2(tstDat)
    
    return(list(train = trDat, validation = valDat, test = tstDat))
    
    #rda_extension <- ".rda"
    #save(trDat, file = file.path(file_path, splitt, paste0('TrainingData', rda_extension)))
    #save(tstDat, file = file.path(file_path, splitt, paste0('TestingData', rda_extension)))
    
  } else {
    #csv_extension <- ".csv"
    #write_csv(trDat, file.path(file_path, splitt, paste0('TrainingData', csv_extension)))
    #write_csv(tstDat, file.path(file_path, splitt, paste0('TestingData', csv_extension)))
    
    return(list(train = trDat, validate = valDat, test = tstDat))
  }
}
