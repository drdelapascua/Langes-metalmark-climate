# RandomForest (Trial 1)
# Danielle De La Pascua

# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# 


## Use the random forest algorithm to predict mpg using the other variables
## and then tune and optimize the RF model. 

# Create a training and testing set
set.seed(123) %>% 
  butterfly_index <- sample(nrow(butterflies), 0.75 * nrow(mtcars)) %>%
  train <- mtcars[butterfly_index, ] %>%
  test <- mtcars[-butterfly_index, ]
