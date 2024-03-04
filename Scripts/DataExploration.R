## Basic Data Exploration
# before beginning to build to model, generally explore the data
# this isnt automated in any way. Play around.
# originally written for Jordan's Perentie data

source("UserInput.R")
options(digits=18) # I want to be able to deal with time precisely

MoveData0 <- read.csv(MovementData)

# summarise into a table
MovementDataSummary <- MoveData0 %>%
     group_by(ID, activity) %>%
     summarise(count = n()) %>%
     spread(key = activity, value = count, fill = 0)

# remove the irrelevent columns
MovementDataSummary <- MovementDataSummary %>%
  select(-"<NA>", -accel_hit)

# plot that
# Load ggplot2 library
library(ggplot2)

# Create a data frame for plotting
MovementDataSummaryplot <- MoveData0 %>%
  filter(activity != "accel_hit" & !is.na(activity)) %>%
  group_by(ID, activity) %>%
  summarise(count = n())

# account for the HZ, convert to seconds
MovementDataSummaryplot <- MovementDataSummaryplot %>%
  mutate(seconds = count/current_Hz)

# Plot the stacked bar graph
my_colors <- c("#88958D", "#606D5D", "#BC9CB0", "#D3CDD7")

ggplot(MovementDataSummaryplot, aes(x = activity, y = seconds, fill = ID)) +
  geom_bar(stat = "identity") +
  labs(x = "Activity",
       y = "Seconds") +
  theme_minimal() +
  scale_fill_manual(values = my_colors) +
  theme(axis.line = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# plotting the trace of the under-represented behaviours
# because my clustering analysis isn't working yet, want to cluster other behaviours togetehr.
# have to check their traces to see whether they'll work together

# extract each of the behaviours
df <- MoveData0 %>%
  subset(activity == "walking")

ggplot(df, aes(x = Time)) +
  geom_line(aes(y = X, color = "X")) +
  geom_line(aes(y = Y, color = "Y")) +
  geom_line(aes(y = Z, color = "Z")) +
  labs(title = "Acceleration Data",
       x = "Time",
       y = "Acceleration") +
  scale_color_manual(values = c(X = "red", Y = "green", Z = "blue")) +
  theme_minimal()


