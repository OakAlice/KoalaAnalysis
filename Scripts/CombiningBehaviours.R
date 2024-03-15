## Reduce behaviours
# this is the script that goes before ReformattingData.R to change the behaviour names and mapping

relabel_activities <- function(formatted_data, relabelledBehaviours){
  relabelled_data <- formatted_data %>%
    mutate(activity = sapply(activity, function(act) {
      for (new_activity in names(relabelledBehaviours)) {
        if (act %in% relabelledBehaviours[[new_activity]]) {
          return(new_activity)
        }
      }
      return(as.character(act))
    }))
  
  return(relabelled_data)
}
