## Exploring the data in a messy way when I discovered that the inactive data
## contained some activity that was throwing off the parameter selection model
## not particularly clean, but just for proof of reference

### Just realised I should have changed this at the source data, fulltraining.csv or
## whatever, because now I can't trust that all dataframes use the correctly 
## labelled data.

FinalTrainingData <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FinalTrainingData.csv", header=T)

FinalTrainingData <- FinalTrainingData[, -1]

#remove old inactive behaviour from FinalTrainingData
FinalTrainingData <- FinalTrainingData %>%
  filter(!(ID %in% c("Elsa", "Meeka", "Hardy", "Nicole") & 
             activity %in% c("Tree Sitting", "Sleeping/Resting", "Ground Sitting")))

## jumped down below to reformat the new inactive data for the training teds
## now back to here to merge dataframes, then write.csv the new TrainingData
FinalTrainingData <- merge(FinalTrainingData, nicole_new, all=TRUE)
FinalTrainingData <- merge(FinalTrainingData, hardy_new, all=TRUE)
FinalTrainingData <- merge(FinalTrainingData, elsa_newinactive, all=TRUE)
FinalTrainingData <- merge(FinalTrainingData, meeka_newinactive, all=TRUE)

## add new tree movement data
FinalTrainingData <- merge(FinalTrainingData, elsa_newtree, all=TRUE)
FinalTrainingData <- merge(FinalTrainingData, meeka_newtree, all=TRUE)

write.csv(FinalTrainingData, file="C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/FinalTrainingData.csv", row.names=FALSE)


### checking inactive and tree movement data 
# first, make a copy of the formatted_data that will remain untouched.
# from here, formatted_data will be manipulated to investigate the data issues
formatted_datacopy <- formatted_data

# subset inactive and tree movement data for all teds
inactive_data <- formatted_data[formatted_data$activity %in% c("Tree Sitting", 
"Sleeping/Resting", "Ground Sitting"), ]
tree_movement_data <- formatted_data[formatted_data$activity %in% c("Tree Movement", 
"Swinging/Hanging", "Grooming", "Climbing Down", "Climbing Up", "Rapid Climbing"), ]

# Filter inactive data for Meeka, Elsa, Hardy and Nicole
meekainactive_data <- subset(inactive_data, ID == "Meeka")
elsainactive_data <- subset(inactive_data, ID == "Elsa")
hardyinactive_data <- subset(inactive_data, ID == "Hardy")
nicoleinactive_data <- subset(inactive_data, ID == "Nicole")

# add row numbers for plotting/visualisation
elsainactive_data <- elsainactive_data %>%
  mutate(row_number = row_number())

meekainactive_data <- meekainactive_data %>%
  mutate(row_number = row_number())

hardyinactive_data <- hardyinactive_data %>%
  mutate(row_number = row_number())

nicoleinactive_data <- nicoleinactive_data %>%
  mutate(row_number = row_number())


# plotting each Training Ted's inactive data to visualise where the issues are
# just change the name of the plot and the data you're calling for each ted

plot_meekainactive <- ggplot(meekainactive_data, aes(x = row_number)) +
  geom_line(aes(y = X_accel, color = "X_accel"), size = 1) +
  geom_line(aes(y = Y_accel, color = "Y_accel"), size = 1) +
  geom_line(aes(y = Z_accel, color = "Z_accel"), size = 1) +
  scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"),
                     name = "Axis", labels = c("X_accel", "Y_accel", "Z_accel")) +
  labs(title = "Acceleration Data",
       x = "Sample Number",
       y = "Acceleration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

## So now we've worked out that the inactive data is pretty fucky for all 4 teds.
## Let's prepare the data for relabelling in matlab, remove unneccessary rows
# and change time format to matlab format
meekainactive_data$time <- as.numeric(meekainactive_data$time) / 86400 + 719529  # 86400 seconds in a day, 719529 is the offset for MATLAB datenum
meekainactive_data <- meekainactive_data %>%
  select(-ID, -activity, -row_number)
names(meekainactive_data) <- NULL

elsainactive_data$time <- as.numeric(elsainactive_data$time) / 86400 + 719529  
elsainactive_data <- elsainactive_data %>%
  select(-ID, -activity, -row_number)
names(elsainactive_data) <- NULL

hardyinactive_data$time <- as.numeric(hardyinactive_data$time) / 86400 + 719529  
hardyinactive_data <- hardyinactive_data %>%
  select(-ID, -activity, -row_number)
names(hardyinactive_data) <- NULL

nicoleinactive_data$time <- as.numeric(nicoleinactive_data$time) / 86400 + 719529  
nicoleinactive_data <- nicoleinactive_data %>%
  select(-ID, -activity, -row_number)
names(nicoleinactive_data) <- NULL

# export data ready for relabelling in matlab
write.csv(meekainactive, file="C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/meekainactive.csv", row.names = FALSE, col.names = FALSE)
write.csv(elsainactive, file="C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/elsainactive.csv", row.names = FALSE)
write.csv(hardyinactive, file="C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/hardyinactive.csv", row.names = FALSE,col.names = FALSE)
write.csv(nicoleinactive, file="C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/nicoleinactive.csv", row.names = FALSE,col.names = FALSE)

## Now I've relabelled the data in MATLAB
## read the new csv's back into R
meeka_new <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/relabel round 3/meekarelabelled.csv", header=T)
meeka_new <- meeka_new %>% rename(X_accel = x, Y_accel = y, Z_accel = z, 
                                                  X_gyro = gx, Y_gyro = gy, Z_gyro = gz)
meeka_newinactive <- subset(meeka_new, activity == c("Tree Sitting","Sleeping/Resting"))
meeka_newtree <- subset(meeka_new, activity == "Tree Movement")

elsa_new <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/relabel round 3/elsarelabelled.csv", header=T)
elsa_new <- elsa_new %>% rename(X_accel = x, Y_accel = y, Z_accel = z, 
                                X_gyro = gx, Y_gyro = gy, Z_gyro = gz)
elsa_newinactive <- subset(elsa_new, activity == c("Tree Sitting","Sleeping/Resting"))
elsa_newtree <- subset(elsa_new, activity == "Tree Movement")

## I exported the old Hardy and Nicole inactive data, but didn't end up relabelling
## because it was just straight trash. So instead I took a section of inactive
## from their wild data, labelled it inactive and imported this in instead.

hardy_new <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/relabel round 3/hardyinactivenew.csv", header=T)
hardy_new <- hardy_new %>% rename(X_accel = x, Y_accel = y, Z_accel = z, 
                                  X_gyro = gx, Y_gyro = gy, Z_gyro = gz)

nicole_new <- read.csv("C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/Training Data/relabel round 3/nicoleinactivenew.csv", header=T)
nicole_new <- nicole_new %>% rename(X_accel = x, Y_accel = y, Z_accel = z, 
                                    X_gyro = gx, Y_gyro = gy, Z_gyro = gz)

## to re-visualise the new relabelled data, you need to add the row_number column again
## And replace the data with the one you want to look at.

meeka_newinactive <- meeka_newinactive %>%
  mutate(row_number = row_number())
elsa_newinactive <- elsa_newinactive %>%
  mutate(row_number = row_number())
hardy_new <- hardy_new %>%
  mutate(row_number = row_number())
nicole_new <- nicole_new %>%
  mutate(row_number = row_number())


ggplot(elsa_newinactive, aes(x = row_number))+
  geom_line(aes(y = X_accel, color = "X_accel"), linewidth = 1) +
  geom_line(aes(y = Y_accel, color = "Y_accel"), linewidth = 1) +
  geom_line(aes(y = Z_accel, color = "Z_accel"), linewidth = 1) +
  scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"),
                     name = "Axis", labels = c("X_accel", "Y_accel", "Z_accel")) +
  labs(title = "Acceleration Data by Axis (elsa)",
       x = "Time",
       y = "Acceleration") +
  theme_minimal()

#### OKAY! Now we've relabelled and checked the plots of the new inactive data
## it's time to add the new inactive data to the formatted_data
## first we need to remove the old inactive data
## remember that at the top we made a copy of the formatted_data so that it's
## there as a back up if we fuck this up. From now on, formatted_data will be
## changing with removal and re-adding data.

formatted_data <- formatted_data %>%
  filter(!(ID %in% c("Elsa", "Meeka", "Hardy", "Nicole") & 
             activity %in% c("Tree Sitting", "Sleeping/Resting", "Ground Sitting")))

# remove the beh_num row because we don't need it in formatted_data

nicole_new <- nicole_new %>%
  select(-behnum)

hardy_new <- hardy_new %>%
  select(-behnum)

elsa_newinactive <- elsa_newinactive %>%
  select(-behnum)

meeka_newinactive <- meeka_newinactive %>%
  select(-behnum)

# add a column of ID
nicole_new$ID <- 'Nicole'
hardy_new$ID <- 'Hardy'
elsa_newinactive$ID <- 'Elsa'
meeka_newinactive$ID <- 'Meeka'

# reorder the columns so they're the same as formatted_data.
# probably a better way to do the last few steps but alas.
nicole_new <- nicole_new[,c(1,2,3,4,5,6,7,9,8)]
hardy_new <- hardy_new[,c(1,2,3,4,5,6,7,9,8)]
elsa_newinactive <- elsa_newinactive[,c(1,2,3,4,5,6,7,9,8)]
meeka_newinactive <- meeka_newinactive[,c(1,2,3,4,5,6,7,9,8)]

# converting matlab time back to POSIXct
days_between_origins <- as.numeric(as.Date("1970-01-01")-as.Date("0000-01-01"))
nicole_new$time <- as.POSIXct((nicole_new$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")
hardy_new$time <- as.POSIXct((hardy_new$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")
elsa_newinactive$time <- as.POSIXct((elsa_newinactive$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")
meeka_newinactive$time <- as.POSIXct((meeka_newinactive$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")

## ADD NEW RELABELLED INACTIVE DATA BACK INTO FORMATTED_DATA!
formatted_data <- merge(formatted_data, nicole_new, all=TRUE)
formatted_data <- merge(formatted_data, hardy_new, all=TRUE)
formatted_data <- merge(formatted_data, elsa_newinactive, all=TRUE)
formatted_data <- merge(formatted_data, meeka_newinactive, all=TRUE)

# success!

## final thing is that I relabelled some of the inactive data from elsa and 
## meeka as tree movement, so I'll clean up those data frames and add them to 
## formatted_data, then that should be everything

elsa_newtree <- elsa_newtree %>%
  select(-behnum)
meeka_newtree <- meeka_newtree %>%
  select(-behnum)

# add a column of ID
elsa_newtree$ID <- 'Elsa'
meeka_newtree$ID <- 'Meeka'

# reorder the columns so they're the same as formatted_data.
# probably a better way to do the last few steps but alas.
elsa_newtree <- elsa_newtree[,c(1,2,3,4,5,6,7,9,8)]
meeka_newtree <- meeka_newtree[,c(1,2,3,4,5,6,7,9,8)]

# converting matlab time back to POSIXct
days_between_origins <- as.numeric(as.Date("1970-01-01")-as.Date("0000-01-01"))
elsa_newtree$time <- as.POSIXct((elsa_newtree$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")
meeka_newtree$time <- as.POSIXct((meeka_newtree$time - days_between_origins)* 86400, origin="1970-01-01", tz="UTC")

formatted_data <- merge(formatted_data, elsa_newtree, all=TRUE)
formatted_data <- merge(formatted_data, meeka_newtree, all=TRUE)

# TREE MOVEMENT
# Filter tree movement data for Meeka, Elsa, Hardy and Nicole
meekatree_data <- subset(tree_movement_data, ID == "Meeka")
elsatree_data <- subset(tree_movement_data, ID == "Elsa")
hardytree_data <- subset(tree_movement_data, ID == "Hardy")
nicoletree_data <- subset(tree_movement_data, ID == "Nicole")

elsatree_data <- elsatree_data %>%
  mutate(row_number = row_number())

meekatree_data <- meekatree_data %>%
  mutate(row_number = row_number())

hardytree_data <- hardytree_data %>%
  mutate(row_number = row_number())

nicoletree_data <- nicoletree_data %>%
  mutate(row_number = row_number())

# plotting each Training Ted's inactive data to visualise where the issues are
# just change the name of the plot and the data you're calling for each ted

plot_meekatree <- ggplot(meekatree_data, aes(x = row_number)) +
  geom_line(aes(y = X_accel, color = "X_accel"), size = 1) +
  geom_line(aes(y = Y_accel, color = "Y_accel"), size = 1) +
  geom_line(aes(y = Z_accel, color = "Z_accel"), size = 1) +
  scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"),
                     name = "Axis", labels = c("X_accel", "Y_accel", "Z_accel")) +
  labs(title = "Acceleration Data by Axis (Meeka)",
       x = "Time",
       y = "Acceleration") +
  theme_minimal()

## DOUBLE CHECKING FORAGING AND WALKING 
# I checked foraging and walking data too, just to be sure that everything is fine
# Don't run this now, unless you need to check it again.
foraging_data <- subset(formatted_data, activity == "Foraging/Eating")
walking_data <- formatted_data[formatted_data$activity %in% c("Walking", "Trot", "Gallop", "Bound/Half-Bound"), ]

## checking foraging and walking data
meeka_forage <- subset(foraging_data, ID == "Meeka")
elsa_forage <- subset(foraging_data, ID == "Elsa")
hardy_forage <- subset(foraging_data, ID == "Hardy")
nicole_forage <- subset(foraging_data, ID == "Nicole")

meeka_forage <- meeka_forage %>%
  mutate(row_number = row_number())

elsa_forage <- elsa_forage %>%
  mutate(row_number = row_number())

hardy_forage <- hardy_forage %>%
  mutate(row_number = row_number())

nicole_forage <- nicole_forage %>%
  mutate(row_number = row_number())

elsa_walk <- subset(walking_data, ID == "Elsa")
meeka_walk <- subset(walking_data, ID == "Meeka")
hardy_walk <- subset(walking_data, ID == "Hardy")
nicole_walk <- subset(walking_data, ID == "Nicole")

meeka_walk <- meeka_walk %>%
  mutate(row_number = row_number())

elsa_walk <- elsa_walk %>%
  mutate(row_number = row_number())

hardy_walk <- hardy_walk %>%
  mutate(row_number = row_number())

nicole_walk <- nicole_walk %>%
  mutate(row_number = row_number())

# once again, replace the data with whoever you want to look at
ggplot(meeka_forage, aes(x = row_number)) +
  geom_line(aes(y = X_accel, color = "X_accel"), linewidth = 1) +
  geom_line(aes(y = Y_accel, color = "Y_accel"), linewidth = 1) +
  geom_line(aes(y = Z_accel, color = "Z_accel"), linewidth = 1) +
  scale_color_manual(values = c(X_accel = "red", Y_accel = "green", Z_accel = "blue"),
                     name = "Axis", labels = c("X_accel", "Y_accel", "Z_accel")) +
  labs(title = "Acceleration Data by Axis (meeka)",
       x = "Time",
       y = "Acceleration") +
  theme_minimal()
#scale_x_continuous(limits = c(132000, 132500)) #for if you want to check a 
# subsection of the data closer up

# I guess this was just to have as back up? Don't need to run this now
write.csv(inactive_data,file= "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/inactivedata.csv", row.names = FALSE)
write.csv(tree_movement_data,file= "C:/Users/uqgspar1/OneDrive - The University of Queensland/Documents/MATLAB/Koala_Accelerometry/Scripts/Gabbys Versions/Model Running 2024/treemovementdata.csv", row.names = FALSE)


