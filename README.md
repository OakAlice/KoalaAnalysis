# Purpose
A user-friendly workflow for designing, tuning, and validating a supervised machine learning model for accelerometer behaviour detection. Originally developed for the analysis of the UQ koala project.

NOTE: Thus far, only includes Random Forest, but will be expanded to encompass different kinds of models.

# Variables
You can adjust the degrees of freedom (i.e., axes), sampling rate, window length, window overlap, calculated features and normalisation, behavioural categories, validation split,  hyperparameter grid search from a single script and then test all possible combinations of these.

# Scripts
## Master Scripts
- Dicionaries.R -> Information from each dataset
- UserInput.R -> Specifying the parameters you want to test
- ExecuteScriptFull.R -> Run the script. Part 1 for model selection, part 2 for test on hold-out set, part 3 for application to unlabelled data.

## Processing
There are a lot of scripts. I will write a better guide later, but for now, here is a basic schematic.

## Just for me
- CleanTrainingData.R -> Cleaning and organising the training data from matlab to csv
- DataExploration.R -> Also just messing around at this point
