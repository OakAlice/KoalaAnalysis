# Purpose
R script and workflow for analysing data from the UQ koala project. I collected data for this project in my honours and it has subsequently been continued by Gabby in her PhD. I aim to create a user-friendly workflow that allows us to analyse and visualise our data. I have endeavoured to make it as transferable and generic as possible, but some of the scripts are unique to my necessary use-case (as a result of having collected realllllllyy bad data in my honours and specific goals).

# Variables
This script can be used to produce a RF from any raw accelerometry data. You can adjust the degrees of freedom (i.e., axes), sampling rate, window length, window overlap, calculated features, selected behaviours, validation split, and hyperparameter grid search from a single script.

# Scripts
## Master Scripts
- CleanTrainingData.R -> Cleaning and organising the training data from matlab to csv (this is just for me)
- UserInput.R -> Entering all variables for each of the experiments
- ExecuteScript.R -> Run each section of the script

## Preprocessing
- ReformattingData.R -> Reformat the data based on the user variables
- GeneralFunctions.R -> Other stuff needed elsewhere
- GeneratingFeatures.R -> Creating the features across all the data # TODO: add normalising and filtering
- SplitData.R -> Split into the training, validation, and testing sets

## Assess validity of behaviour labels
- SOMClusters.R -> Use an unsupervised SOM to cluster the training data - calculate normalised mutual information to find optimal clusters # note, this literally is so bad. Don't know if code wrong or idea wrong

## Model Application and Validation
- RandomForest.R -> Apply Random forest training and validation # currently very basic, add more hyperparameter tuning
- OptimalModelRun.R -> Generate confusion matrix and plot for any set of model parameters
