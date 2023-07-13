# RandomForest (Trial 1)
# Danielle De La Pascua

# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# pull datas
butterflies = read.csv("~/Langes-metalmark-climate/Data/butterflies.csv")

# get rid of that ugly first column
head(butterflies)
butterflies <- butterflies[,-1]
head(butterflies)


# population density vaiable
hist(butterflies$N) #extremely skewed 

butterflies$logN <- log(butterflies$N)

hist(butterflies$logN) #looks much better

butterflies <- butterflies[,-3]
butterflies <- butterflies [,-2]
butterflies <- butterflies[,-1]

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
rf1 <- randomForest(logN ~ ., data = train, ntree = 500, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf1 # 0.78% variation explained

# Make predictions on the test set
predictions <- predict(rf1, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 2.94

## RF model 2
rf2 <- randomForest(logN ~ ., data = train, ntree = 1000, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf2 # 1.15% var explained

# Make predictions on the test set
predictions <- predict(rf2, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 3.009

## RF model 3
rf3 <- randomForest(logN ~ ., data = train, ntree = 1500, mtry = 3,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf3 # 0.53# var explained

# Make predictions on the test set
predictions <- predict(rf3, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 2.87

## Using mtree = 1000

## RF model 4, mtry - 5
rf4 <- randomForest(logN ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf4 # -2.3% variation explained by model - huh?

# Make predictions on the test set
predictions <- predict(rf4, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 3.03

## RF model 5, mtry = 10
rf5 <- randomForest(logN ~ ., data = train, ntree = 1000, mtry = 10,
                    max_depth = 5, min_samples_split = 5, min_samples_leaf = 1)
rf5 # -6% var explained - huh??

# Make predictions on the test set
predictions <- predict(rf5, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 3.03

## Use mtry = 5

## max_depth = 5, 10, 15

## RF model 6, max_depth = 10
rf6 <- randomForest(logN ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 10, min_samples_split = 5, min_samples_leaf = 1)
rf6 # -4% var explained - huh?

# Make predictions on the test set
predictions <- predict(rf6, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 2.96

## RF model 7, max_depth = 15
rf7 <- randomForest(logN ~ ., data = train, ntree = 1000, mtry = 5,
                    max_depth = 15, min_samples_split = 5, min_samples_leaf = 1)
rf7 # -3% var explained

# Make predictions on the test set
predictions <- predict(rf7, newdata = test)

# Evaluate the model
rmse <- mean((predictions - test$logN)^2)
print(rmse) # 3.06

## Model 3 is the best model?

# Plot the variable importance for the best model, Model 7, using node purity

varImpPlot(rf3, sort = TRUE, main = "Variable Importance Plot")

# Print varImpPlot


dev.print(tiff, "varImpPlot_model3.tiff", height=4, width=6, units='in', res=300)


