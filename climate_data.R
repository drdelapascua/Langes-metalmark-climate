# Climate data extraction
# Danielle De La Pascua

# libraries

library(dplyr)
library(tidyverse)

# pull data

dat = read.csv("~/Langes-metalmark-climate/Data/climate_data/pump_data.csv")

# separate the date out 

dat = dat %>%
  separate(data = dat, col = Date, into = c("Year", "Month", "Day"), sep = c(4, 6), remove = TRUE, convert = TRUE)

head(dat)

### variables

### November min temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 11) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Nov_min=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df1 <- agg_tbl %>% as.data.frame()
df1

### November maximum temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 11) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Nov_max=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()
df2

### August maximum temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 8) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Aug_max=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df3 <- agg_tbl %>% as.data.frame()
df3

### Dec min temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 12) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Dec_min=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df4 <- agg_tbl %>% as.data.frame()
df4

### January min temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 1) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Jan_min=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df5 <- agg_tbl %>% as.data.frame()
df5

### September max temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 9) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Sept_max=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df6 <- agg_tbl %>% as.data.frame()
df6

### July max temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 7) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(July_max=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df7 <- agg_tbl %>% as.data.frame()
df7

# Fall precipitation

# Winter precipitation

#Winter temperature standard deviation

#winter precipitation standard variation