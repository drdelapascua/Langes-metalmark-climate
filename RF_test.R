# RandomForest training ----
# Danielle De La Pascua
# 7-3-23

### Lingo ----

### > ensemble ----
### a modeling approach that looks at multiple models in combination to get a new prediction
### (for eg using a combo of multivariate adaptive regressions, boosted regression tree, randomforest, etc)
### for example you can take the average over a bunch of different models

### > bootstrapping ----
### Randomly selecting a bunch of datapoints from a dataset to make new datasets, using these to test models etc
### Our bootstrapping models are WITH replacement

### > features ----
### synonym for predictor variables

### > target ----
### synonym for response variables

### > hyperparameters ----
### Parameters to optimize the model - such as how many trees are in the forest, the number of splits, depth, number of 
### variables, number of variables it chooses at each split, etc

### > in/out of bag ----
### visualize a bag - each time you take a sample that is in your bag, your training sample is out of the bag
### out of bag error - this is talking about the variance of the model (RMSE)

### Process ----

### > Steps ----
 
### 1) take a sample of the data with replacement (around 2/3 of the data). It Defines the remaining 1/3 as a test dataset
### 2) build the model with the 2/3, test it with 1/3
### 3) Bootstrap sample - it grabbed one sample, but now grabs a bunch of different samples with replacement
### 4) Build RF models - of X variables, select a smaller subset (8 choose 5 for eg) and takes each variable and looks at the 
###    variable across years and looks at distribution across all the variables. it will randomly select 10 points in the 
###    distributions and walks up and down the ten different splitpoints and asks which is the best at predicting population
### 5) The model picks new variables and does the whole process again. it does this as many times as there are splits (5 for eg)
###    for each split point, model asks if its above or below, and then it gets to the end and predicts the population. 
### 6) the decision trees get made a bunch times, that is your forest (all the decision trees)
### 7) to assess which trees are better, assess predicted values to the data (root mean square error - RMSE),
###    and you can do it for the whole forest 



