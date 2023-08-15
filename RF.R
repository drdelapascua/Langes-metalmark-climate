# RandomForest (Trial 1)
# Danielle De La Pascua

# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)

# pull datas
butterflies = read.csv("~/Langes-metalmark-climate/Data/butterflies.csv")

# get rid of that ugly first column
#head(butterflies)
#butterflies <- butterflies[,-1]
#head(butterflies)

#butterflies <- butterflies[,-3]
#butterflies <- butterflies [,-2]
#utterflies <- butterflies[,-1]
#butterflies <- butterflies[-1,]

## Use the random forest algorithm to predict mpg using the other variables
## and then tune and optimize the RF model. 

# Create a training and testing set
set.seed(123)
butterfly_index <- sample(nrow(butterflies), 0.75 * nrow(butterflies))
train <- butterflies[butterfly_index, ]
test <- butterflies[-butterfly_index, ]


## Tuning using the key hyperparameters

# hyperparameters to try

# ntree = 500, 1000, 1500. This is the number of trees in the forest
# mtry = square root of number of variables in the model. Controls the number of variables to randomly sample as candidates at each split. 
# max_depth = 5, 10, 15. Command controls the maximum depth of the decision trees in the random forest.
# min_samples_split = 5. Controls the minimum number of samples required to split an internal node in a decision tree.
# min_samples_leaf = 1

## RF model 1
rf1 <- randomForest(N ~ ., data = train, ntree = 500, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf1 # 7% variation explained

# Make predictions on the test set
predictions <- predict(rf1, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 5740888

## RF model 2
rf2 <- randomForest(N ~ ., data = train, ntree = 1000, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf2 # 12% var explained

# Make predictions on the test set
predictions <- predict(rf2, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 6044105

## RF model 3
rf3 <- randomForest(N ~ ., data = train, ntree = 1500, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf3 # 14% var explained

# Make predictions on the test set
predictions <- predict(rf3, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 5768790

## Using mtree = 1000

## RF model 4, mtry - 5
rf4 <- randomForest(N ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf4 # 23.06% variation explained by model - huh?

# Make predictions on the test set
predictions <- predict(rf4, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 4454368

## RF model 5, mtry = 10
rf5 <- randomForest(N ~ ., data = train, ntree = 1000, mtry = 10,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf5 # 38% var explained - huh??

# Make predictions on the test set
predictions <- predict(rf5, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 1957109

## Use mtry = 5

## max_depth = 5, 10, 15

## RF model 6, max_depth = 10
rf6 <- randomForest(N ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 10, min_samples_split = 5, min_samples_leaf = 1)
rf6 # 23.66% var explained - huh?

# Make predictions on the test set
predictions <- predict(rf6, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 4387749

## RF model 7, max_depth = 15
rf7 <- randomForest(N ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 15, min_samples_split = 5, min_samples_leaf = 1)
rf7 # 21% var explained

# Make predictions on the test set
predictions <- predict(rf7, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$N)^2)
print(rmse) # 4711580

## Model 5 is the best model?

# Plot the variable importance for the best model, Model 7, using node purity

varImpPlot(rf5, sort = TRUE, main = "Variable Importance Plot")

# Print varImpPlot


dev.print(tiff, "varImpPlot_model3.tiff", height=4, width=6, units='in', res=300)

# RF with just N, not logN
