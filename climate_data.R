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

# converting to "last november"
min_temp <- df1$Nov_min
min_temp <- head(min_temp, -1)
years <- c(1986:2022)
last_nov_min_temp <- data.frame(years, min_temp)

### November maximum temperature

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 11) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df2 <- agg_tbl %>% as.data.frame()
df2

# converting to "last november"
max_temp <- df2$max_temp
max_temp <- head(max_temp, -1)
years <- c(1986:2022)
last_nov_max_temp <- data.frame(years, min_temp)

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
august_max_temp <- df3

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

# converting to "last december"
min_temp <- df4$Dec_min
min_temp <- head(min_temp, -1)
years <- c(1986:2022)
last_dec_min_temp <- data.frame(years, min_temp)

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
january_min_temp <- df5

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

# converting to "last september"
max_temp <- df6$Sept_max
max_temp <- head(max_temp, -1)
years <- c(1986:2022)
last_nov_min_temp <- data.frame(years, max_temp)

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
july_max_temp <- df7

# Fall precipitation

# > September precipitation

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 9) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Fall_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_sept <- agg_tbl %>% as.data.frame()
precip_sept

# > October precipitation

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 10) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Fall_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_oct <- agg_tbl %>% as.data.frame()
precip_oct

# > November precipitation

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 11) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(Fall_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_nov <- agg_tbl %>% as.data.frame()
precip_nov

# Add september, october, and november precipitation values
fall_precip<- precip_sept$Fall_precip + precip_oct$Fall_precip + precip_nov$Fall_precip
years <- c(1985:2022)
precip_fall <- data.frame(years, fall_precip)

#make 'last fall' variable
precip_fall
values <- precip_fall$fall_precip
years <- c(1986:2023)
last_fall_precip <- data.frame(years, values)  

# Winter precipitation

# > December precipitation

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 12 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(winter_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_dec <- agg_tbl %>% as.data.frame()
precip_dec 

# > January
# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 1 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(winter_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_jan <- agg_tbl %>% as.data.frame()
precip_jan 

# > January
# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 2 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sum(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(winter_precip=sum(Precip), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
precip_feb <- agg_tbl %>% as.data.frame()
precip_feb 

## fixing december to be "last december"

precip_dec

values <- precip_dec$winter_precip
years <- c(1986:2023)

precip_last_dec <- data.frame(years, values)

#add all values
winter_precip <- c(precip_last_dec$values + precip_jan$winter_precip + precip_feb$winter_precip)
winter_precip <- head(winter_precip, -1)
years <- c(1986:2022)
precip_winter <- data.frame(years, winter_precip)

#Winter temperature standard deviation



#winter precipitation standard variation