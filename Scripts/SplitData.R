# Code to create the training and testing data

source("Balancing.R")

# process the data
split_condition <- function(formatted_data, splitMethod, 
                            trainingPercentage, validationPercentage, 
                            num_individuals, test_individuals = NA) {
  
  dat <- formatted_data# %>% na.omit()
  
  remove_columns <- function(df) {
    df %>% select(-any_of(c("time", "n", "X", "over_threshold", "max_rows")))
  }
  
  testingPercentage <- 1 - trainingPercentage - validationPercentage
  prop <- c((1- testingPercentage), testingPercentage)  # Proportions for each group
  
  if (splitMethod == "random") {
    # set up valiables
    n <- nrow(dat)
    
    # Generate random indices for every row
    indices <- sample(1:2, n, replace = TRUE, prob = prop)
    
    # splitMethod data based on those random indices
    tstDat <- dat[indices == 1, ] %>% remove_columns()
    otherDat <- dat[indices == 2, ] %>% remove_columns()
    
  } else if (splitMethod == "chronological") {
    if ("ID" %in% colnames(dat)) { 
      sorted_dat <- arrange(dat, ID, time)
      individuals <- unique(sorted_dat$ID)
      
      tstDat <- otherDat <- data.frame()
      
      for (i in 1:length(individuals)) {
        ind_data <- sorted_dat[sorted_dat$ID == individuals[i], ]
        
        n_ind <- nrow(ind_data)
        ind_sample_sizes <- floor(prop * n_ind)
        
        ind_sample_sizes[2] <- max(1, ind_sample_sizes[2])  # Ensure at least one row is selected for tstDat
        
        ind_tstDat <- ind_data[(n_ind - ind_sample_sizes[2] + 1):n_ind, ]
        ind_otherDat <- ind_data[1:(n_ind - ind_sample_sizes[2]), ]
        
        otherDat <- rbind(ind_otherDat, otherDat)
        tstDat <- rbind(ind_tstDat, tstDat)
      }
      
      otherDat <- remove_columns(otherDat)
      tstDat <- remove_columns(tstDat)
      
    } else { 
      dat <- dat %>% arrange(time)
      sample_sizes <- floor(nrow(dat) * prop)
      
      otherDat <- dat[0:sample_sizes[1], ] %>% remove_columns() 
      tstDat <- dat[(length(dat$ID) - sample_sizes[2]):length(dat$ID), ] %>% remove_columns()
    }
  } else if (splitMethod == "LOIO" | splitMethod == "SparkesKoalaValidation") {
    # Ensure test_individuals is provided for LOIO splitMethod
    if (is.null(test_individuals)) {
      stop("test_individuals must be provided for LOIO splitMethod")
    }
    
    if (splitMethod == "SparkesKoalaValidation"){
      selected_ids <- test_individuals # have to manually set it for some daatsets
      
    } else {
      # how many individuals to be left out
      approx_individuals <- ceiling(test_individuals * prop)
      
      # Adjust any rounded 0s to be 1 - minimum necessary
      approx_individuals[approx_individuals == 0] <- 1
      
      # randomly select that number of IDs
      unique_ids <- unique(dat$ID)
      selected_ids <- sample(unique_ids, size = approx_individuals)
    }
      
    test_data <- dat[dat$ID %in% selected_ids, ]
    other_ids <- setdiff(unique(dat$ID), selected_ids)
    other_data <- dat[dat$ID %in% other_ids, ]
    
    # make these into the test and other set
    tstDat <- test_data %>% remove_columns()
    otherDat <- other_data %>% remove_columns()
    }

  return(list(test = tstDat, other = otherDat))
}
