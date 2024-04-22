# Purpose
A user-friendly AutoML HPO workflow for designing, tuning, and validating a supervised machine learning model for accelerometer behaviour detection. Originally developed for the analysis of the UQ koala project but expanded for the second chapter of my PhD. Designed to run on a HPC cluster. This is the R version, though will later be expanded to python.

PROGRESS NOTE: Currently expanding the tunable hyperparameters for a larger range of model architectures.

# Variables
To add a new dataset, add relevant infomation to the Dictionaries.R and then, in UserInput.R, select the variables you want to test. Below table provides summary of what has been coded.
| Parameter            | Description                                           | Options                     |
|----------------------|-------------------------------------------------------|-----------------------------|
| Data Volume          | Total minutes                                         | Dataset dependent           |
| Individuals          | Number of unique individuals in the study             | Dataset dependent           |     
| Hz                   | Frequency of samples                                  | Any                         |
| Degrees Freedom      | Number of accelerometer dimensions                    | Any                         |     
| Balancing            | Threshold for downsampling over-represented classes   | Any                         |
| Window               | Window length for feature generation                  | Any                         |
| Overlap              | Overlap between successive windows                    | Any                         |
| Features             | Features that are generated per window                | All, Selection              |
| Feature Normalisation| Scale features around 0                              | None, Min-Max, Standardisation|
| Model Architecture   | The base ML architecture  (In Progress)              | RF, SVM, DT, kNN, XGB, MLP   |
| Hyperparameters      | Specific to the ML architecture (TODO)               | Too many to list here        |
| Search               | Method of parameter space exploration (TODO)         | Grid, Random, Baysian        |

# Scripts
## Master Scripts
- Dicionaries.R -> Information from each dataset
- UserInput.R -> Specifying the parameters you want to test
- ExecuteScriptFull.R -> Run the script. Part 1 for model selection, part 2 for test on hold-out set, part 3 for application to unlabelled data (last part isn't automated yet).

## Processing
There are a lot of scripts. I will write a better guide later, but for now, here is a basic schematic.

<img src="https://github.com/OakAlice/KoalaAnalysis/assets/127095766/72bc0072-4d1f-4c81-b5f5-09a65c3de276" width="50%">

## Just for me
- CleanTrainingData.R -> Cleaning and organising the training data from matlab to csv
- DataExploration.R -> Also just messing around at this point
- Clustering.R -> Experimenting with unsupervised methods. Should delete but haven't yet.
