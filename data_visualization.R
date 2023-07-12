# Data visualization & Correlation among predictor variables
# Danielle De La Pascua

# libraries ----

library(dplyr)
library(tidyverse)
library(ggplot2)

# pull data 

butterflies = read.csv("~/Langes-metalmark-climate/Data/butterflies.csv")

# get rid of that ugly first column
head(butterflies)
butterflies <- butterflies[,-1]
head(butterflies)

# histograms of variables ----

### > response variables ----

# population density
hist(butterflies$N) #extremely skewed 

butterflies$logN <- log(butterflies$N)

hist(butterflies$logN) #looks much better

#population growth rate
hist(butterflies$r) # more normal! - might be the variable to use

### > predictor variables ----

hist(butterflies$nov_min_temp) #more normal
hist(butterflies$nov_max_temp) #sort of skewed
hist(butterflies$aug_max_temp) #sort of skewed
hist(butterflies$dec_min_temp) #normal
hist(butterflies$july_max_temp) #skewed
hist(butterflies$jan_min_temp) #sorta normal
hist(butterflies$sept_max_temp) #sorta normal
hist(butterflies$fall_precip) #sorta skewed
hist(butterflies$winter_precip) #somewhat normal, almost bi-modal
hist(butterflies$winter_min_temp_sd) #kinda normal
hist(butterflies$winter_precip_sd) #sorta normal

# correlation among predictor variables ----

corr_mat <- cor(butterflies)
write.csv(corr_mat, "~/Langes-metalmark-climate/output/correlation_matrix.csv")


# simple relationships between predictor variables and responses variables ----

# > logN ~ variables ----

# logN ~ november minimum temperature
ggplot(butterflies, aes(nov_min_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #weak positive correlation

# logN ~ nov max temp
ggplot(butterflies, aes(nov_max_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #really nice negative correlation here

# > R ~ variables ----

# R ~ november minimum temperature
ggplot(butterflies, aes(nov_min_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm)

# R ~ nov max temp
ggplot(butterflies, aes(nov_max_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) # positive relationship


