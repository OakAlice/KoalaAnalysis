## Dictionaries
## Creating dictionaries for all the specific experiments I'm running

#  referenceName <- list(
#    species = "species",
#    data_location = "c:/path/to/data/data.csv",
#    test_individuals = #,
#    current_hz = #,
#    column_subset = c(list = list),
#    time_format = "matlab" or "other",
#    behaviours_1 = c("list"),
#    behaviours_2 = c("old behaviour" = "new behaviour"),
#    target_behaviour = c("list")

VerkjojaDog <- list(
  notes = "Dog data taken from a paper",
  data_location = "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/DogMoveData.csv",
  test_individuals = 45,
  current_hz = 50,
  column_subset = c("DogID" = "ID", "t_sec" = "time", 
                    "ANeck_x" = "X_accel", "ANeck_y" = "Y_accel", "ANeck_z" = "Z_accel",
                    "GNeck_x" = "X_gyro", "ANeck_y" = "Y_gyro", "ANeck_z" = "Z_gyro",
                    "Behavior_2" = "activity"),
  time_format = "other",
  behaviours_1 = c("Eating", "Walking", "Jumping", "Tugging", "Galloping",     
                   "Carrying object", "Standing", "Panting", "Lying chest", "Sitting", "Playing",        
                   "Bowing", "Trotting", "Shaking", "Pacing"),
  target_behaviour = c("Walking")
)

SparkesKoala <- list(
  notes = "Gabby koala data",
  data_location = "C:/Users/oakle/Documents/PhD docs/Redoing Honours/Gabby Data/AllIndBalancedTrainingData.csv",
  test_individuals = 12, # only 2 with a lot of data
  current_hz = 100,
  column_subset = c("Time" = "time", 
                    "X" = "X_accel", "Y" = "Y_accel", "Z" = "Z_accel",
                    "GX" = "X_gyro", "GY" = "Y_gyro", "GZ" = "Z_gyro",
                    "ID" = "ID", "activity" = "activity"),
  time_format = "matlab",
  behaviours_1 = c("Tree Sitting", "Sleeping/Resting", "Foraging/Eating",
                   "Shake", "Grooming", "Bellowing", "Branch Walking",
                   "Climbing Up", "Climbing Down", "Rapid Climbing", "Swinging/Hanging",
                   "Tree Movement", "Walking", "Trot", "Gallop", "Bound/Half-Bound",
                   "Ground Sitting"),
  behaviours_2 <- c("Tree Sitting" = "Sitting", # write in the changes
                    "Climbing Up" = "Climbing",
                    "Climbing Down" = "Climbing",
                    "Ground Sitting" = "Sitting"),
  target_behaviour = c("Walking")
)

WilsonKoala <- list(
  notes = "Koala data collected during my honours",
  data_location = "C:/Users/oakle/Documents/GitHub/KoalaAnalysis/Data/TrainingData2.csv",
  test_individuals = 8,
  current_hz = 50,
  column_subset = c("Time" = "time", 
                    "X" = "X_accel", "Y" = "Y_accel", "Z" = "Z_accel",
                    "GX" = "X_gyro", "GY" = "Y_gyro", "GZ" = "Z_gyro",
                    "Activity" = "activity"),
  time_format = "matlab",
  behaviours_1 = c("Climb_1", "Climb_2", "Climb_3", "Climb_4", 
                   "Tree_movement", "Tree_still", 
                   "Walking_1", "Walking_2", "Walking_3", "Walking_4", 
                   "Ground_Movement", "Ground_Still"),
  target_behaviour = c("Walking_1", "Walking_2", "Walking_3", "Walking_4")
)

DiCiccoPerentie <- list(
  notes = "Perentie data from Jordan's honours (original data)", # needs to be redone
  data_location = "C:/Users/oakle/Documents/PhD docs/CHapter_Three_Perentie/TrainingData2.csv",
  test_individuals = 4,
  current_hz = 50,
  column_subset = c("Time" = "time", 
                    "X" = "X_accel", "Y" = "Y_accel", "Z" = "Z_accel",
                    "ID" = "ID", "activity" = "activity"),
  time_format = "matlab",
  behaviours_1 = c("resting", "walking", "sleeping", "climbing",
                   "climbing_down", "running", "defensive", "eating", "accel_hit"),
  target_behaviour = c("walking")
)



