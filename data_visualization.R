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

# R ~ logN 
ggplot(butterflies, aes(logN, r)) + 
  geom_point() + 
  geom_smooth(method = lm) # positive relationship - higher logN, higher R

# logN ~ Year
ggplot(butterflies, aes(logN, Year)) + 
  geom_point() + 
  geom_smooth(method = lm) # very negative, will most likely need to be removed from the RF model

# > logN ~ variables ----

# logN ~ november minimum temperature
ggplot(butterflies, aes(nov_min_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #weak positive correlation

# logN ~ nov max temp
ggplot(butterflies, aes(nov_max_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #really nice negative correlation here

#logN ~ aug max
ggplot(butterflies, aes(aug_max_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #negative relationship - when august is hot, pop is lower

# logN ~ dec min temp
ggplot(butterflies, aes(dec_min_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #negative but not as strong

# logN ~ july max temp
ggplot(butterflies, aes(july_max_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) # also negative - hotter = less population

# logN ~ january min temp
ggplot(butterflies, aes(jan_min_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) # not really a relationship - line is straight, data cloud

# logN ~ september max temp
ggplot(butterflies, aes(sept_max_temp, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #negative relationship - higher max septeber temp correlated with low N

# logN ~ Fall precipitation
ggplot(butterflies, aes(fall_precip, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) #straight line - no relationship

# logN ~ Winter precipitation
ggplot(butterflies, aes(winter_precip, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) # slightly positive relationship here

# logN ~ winter min temp sd
ggplot(butterflies, aes(winter_min_temp_sd, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) # we DO see a positive relationship here? More variation in winter temp = higher pop?

#logN ~ winter precip sd
ggplot(butterflies, aes(winter_precip_sd, logN)) + 
  geom_point() + 
  geom_smooth(method = lm) # positive relationship - more variation in precip better for pop? unexpected, but maybe low SD values are mapping onto dry winters?


# > R ~ variables ----

# R ~ november minimum temperature
ggplot(butterflies, aes(nov_min_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm)

# R ~ nov max temp
ggplot(butterflies, aes(nov_max_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) # positive relationship

# R ~ aug max temp
ggplot(butterflies, aes(aug_max_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) # weak negative relationship 

# R ~ december min temp
ggplot(butterflies, aes(dec_min_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) # kinda weak negative relationship

# R ~ july max temp
ggplot(butterflies, aes(july_max_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #negative relationship

# R ~ january min temp
ggplot(butterflies, aes(jan_min_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #not too much relationship

# R ~ september max temp
ggplot(butterflies, aes(sept_max_temp, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #wow, strong negative relationship!

# R ~ fall precipitation
ggplot(butterflies, aes(fall_precip, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #weakly negative

# R ~ winter precipitation
ggplot(butterflies, aes(winter_precip, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #not a huge relationship, pretty straight across

# R ~ winter min temperature sd
ggplot(butterflies, aes(winter_min_temp_sd, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #positive

# R ~ winter precipitation sd
ggplot(butterflies, aes(winter_precip_sd, r)) + 
  geom_point() + 
  geom_smooth(method = lm) #positive
