# Data visualization & Correlation among predictor variables
# Danielle De La Pascua

# libraries ----

library(dplyr)
library(tidyverse)
library(ggplot2)

# pull data 

butterflies = read.csv("~/Langes-metalmark-climate/Data/butterflies.csv")

# get rid of that ugly first column

#head(butterflies)
#butterflies <- butterflies[,-1]
#head(butterflies)

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
write.csv(corr_mat, "~/Langes-metalmark-climate/output/correlation_matrix_7-19-23.csv")

# relationships with main variables over time

ggplot(butterflies, aes(Year, september_max_temp_sd)) + 
  geom_point() + 
  geom_smooth(method = lm) # kinda straight across, no clear relationship

ggplot(butterflies, aes(Year, fall_precip)) + 
  geom_point() + 
  geom_smooth(method = lm) # no clear relationship with time

ggplot(butterflies, aes(Year, N.1)) + 
  geom_point() + 
  geom_smooth(method = lm) 

ggplot(butterflies, aes(Year, N)) + 
  geom_point() + 
  geom_smooth(method = lm) 


#digging into that hypothesis : relationship between winter precip ~winter sd precip
ggplot(butterflies, aes(winter_precip_sd, winter_precip)) + 
  geom_point() + 
  geom_smooth(method = lm) # No clear relationship here...


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

# new ones 7-19-23

# N ~ N-1
ggplot(butterflies, aes(N.1, N)) + 
  geom_point() + 
  geom_smooth(method = lm) #positive

# N ~ september_max_temp_sd
ggplot(butterflies, aes(september_max_temp_sd, N)) + 
  geom_point() + 
  geom_smooth(method = lm) #really nice negative correlation here

# N ~ august max temperature sd
ggplot(butterflies, aes(august_max_temp_sd, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # positive

# N ~ september max temperature sd
ggplot(butterflies, aes(september_max_temp_sd, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # negative

# N ~ february min temperature
ggplot(butterflies, aes(feb_min_temp, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # slightly positive

# N ~ june max temperature
ggplot(butterflies, aes(june_max_temp, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # negative 

# N ~ june max temperature sd
ggplot(butterflies, aes(june_max_temp_sd, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # straight line/no relationship

# N ~ july max temperature sd
ggplot(butterflies, aes(july_max_temp_sd, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # slightly positive

#N ~ fall precipitation
ggplot(butterflies, aes(fall_precip, N)) + 
  geom_point() + 
  geom_smooth(method = lm) # slightly positive


