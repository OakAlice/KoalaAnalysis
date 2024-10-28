# Assessing the validity of Behavioural Clusters using the unsupervised SOM
# run every grid shape and k to find the optimal number of behavioural clusters

# load the packages specific for this script
p_load(entropy, infotheo)


# Function for performing SOM clustering for a given width, height, and k
perform_som_clustering <- function(data, grid_width, grid_height, k, threshold) {
  
  # Remove the non-numeric columns
  data_scaled <- data %>% ungroup() %>% select(-ID, -activity) %>% scale()
  
  # Create a SOM grid
  som_grid <- somgrid(xdim = grid_width, ydim = grid_height, topo = "hexagonal")
  
  # Train the SOM
  som_model <- som(data_scaled, grid = som_grid, rlen = 100)
  
  # Map original data to SOM nodes
  mapping <- som_model$unit.classif
  
  # Cluster the SOM units using hierarchical clustering
  som_dists <- dist(som_model$codes[[1]])
  hclust_res <- hclust(som_dists)
  
  clusters <- cutree(hclust_res, k = k) # hierarchical cluster into k major groups
  
  # Assign a cluster to each original data point based on SOM mapping
  data$SOMCluster <- clusters[mapping]
  
  # Aggregate activities by SOM cluster
  activity_clusters <- aggregate(activity ~ SOMCluster, data = data, FUN = function(x) toString(unique(x)))
  
  # Calculate activity proportions within each cluster
  activity_proportions <- data %>%
    group_by(SOMCluster, activity) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    left_join(data %>% group_by(SOMCluster) %>% summarise(Total = n(), .groups = 'drop'), by = "SOMCluster") %>%
    mutate(Proportion = Count / Total)
  
  # Filter to identify dominant activities based on a threshold
  thresh <- threshold
  dominant_activities <- filter(activity_proportions, Proportion > thresh)
  
  # For each cluster, list dominant activities
  dominant_activity_clusters <- aggregate(activity ~ SOMCluster, data = dominant_activities, 
                                          FUN = function(x) toString(unique(x)))
  
  # Return a list containing various elements
  list(
    SOMModel = som_model,
    ActivityClusters = activity_clusters,
    DominantActivityClusters = dominant_activity_clusters,
    ActivityProportions = activity_proportions
  )
}

barPlotClustering <- function(ActivityProportions){
  # create a colour palette
  num_activities <- length(unique(ActivityProportions$activity))
  distinct_colors <- qualitative_hcl(n = num_activities, palette = "Dynamic")
  
  # Now use these colors in your ggplot
  ggplot(ActivityProportions, aes(x = as.factor(SOMCluster), y = Proportion, fill = activity)) + 
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = distinct_colors) + # Use the generated distinct colors
    theme_minimal() +
    labs(x = "SOM Cluster", y = "Proportion", title = "Activities by SOM Cluster") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=2))
}

generateCombinations <- function(w, h, k){
  combinations <- expand.grid(width = 2:w, height = 2:h, clusters = 2:k)
  return(combinations)
}

# determine the behaviours shared between clusters using entropy
# how split each individual behaviour is # metric I made up myself
myClusteringOutcome <- function(outcome, k){
  
  N <- length(unique(outcome$SOMCluster))
  # Calculating maximum entropy for N clusters
  max_entropy <- log(k)
  
  # how much are the behaviours split between the categories, create an entropy score for each behaviour
  splitProportions <- outcome %>%
    group_by(activity) %>%
    mutate(TotalForActivity = sum(Count), # Total counts for each activity
           PropForActivity = Count / TotalForActivity) %>% # Proportion of each count within its activity
    ungroup() %>%
    group_by(activity) %>%
    summarise(entropy_score = -sum(PropForActivity * log(PropForActivity))) %>% # Calculate entropy
    ungroup()

  splitProportions <- splitProportions %>%
    mutate(entropy_ratio = entropy_score / max_entropy)
  
  # Calculate the overall quality score as the average of entropy ratios
  myClusterScore <- mean(splitProportions$entropy_ratio)
  return(myClusterScore) # where a high score means that clusters are bad
}

# assessing the cluster usefulness using Mutual Information theory
mutualClusteringOutcome <- function(outcome) {
  # Calculate mutual information score between SOMCluster and activity
  mutinfo_score <- mutinformation(discretize(outcome$SOMCluster), discretize(outcome$activity))
  
  # Calculate entropies of each variable
  entropy_som_cluster <- entropy(discretize(outcome$SOMCluster))
  entropy_activity <- entropy(discretize(outcome$activity))
  
  # Calculate the average entropy of the two variables
  avg_entropy <- (entropy_som_cluster + entropy_activity) / 2
  
  # Calculate normalized mutual information (NMI)
  nmi_score <- mutinfo_score / avg_entropy
  
  # Return the NMI score
  return(nmi_score) # where a higher score means better clusters
}



### Find the optimal clustering
exampleData <- trDat
combinations <- generateCombinations(9, 9, 10) # create as many as you want to test

allClusteringOutcomes <- data.frame() # store results

for (index in 1:nrow(combinations)){
  print(index) # just show me where we're up tp
  
  # extract this iterations parameters
  width <- as.numeric(combinations[index, 1, drop = TRUE])
  height <- as.numeric(combinations[index, 2, drop = TRUE])
  clusters <- as.numeric(combinations[index, 3, drop = TRUE])

  # catch when k is too large
  tryCatch({
    # Perform the clustering with these parameters
    result <- perform_som_clustering(exampleData, width, height, clusters, 0.1)
    outcome <- result$ActivityProportions
    
    # Assess the result I want 
    myClusterScore <- myClusteringOutcome(outcome, clusters)
    mutualClusterScore <- mutualClusteringOutcome(outcome)
    
    # Put all of these values into a dataframe
    clusteringOutcome <- cbind(width, height, clusters, myClusterScore, mutualClusterScore)
    allClusteringOutcomes <- rbind(allClusteringOutcomes, clusteringOutcome) # Combine iteration results
  }, error = function(e) {
    cat("Error in iteration", index, ": ", e$message, "\n")
  })
}

summaryOutcome <- allClusteringOutcomes
# low myClusterScore is better
# high mutualCluster Score is better

## logic review the best clusterings by plotting it
result <- perform_som_clustering(exampleData, 15, 15, 20, 0.1)
barPlotClustering(result$ActivityProportions)
# piece of poop bad results ugh.



