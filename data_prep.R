# Climate data extraction
# Danielle De La Pascua

# libraries ----

library(dplyr)
library(tidyverse)

# pull data ----

dat = read.csv("~/Langes-metalmark-climate/Data/climate_data/pump_data.csv")

# separate the date out 

dat = dat %>%
  separate(data = dat, col = Date, into = c("Year", "Month", "Day"), sep = c(4, 6), remove = TRUE, convert = TRUE)

head(dat)

### variables 

### November min temperature ----

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
nov_min_temp <- df1$Nov_min
nov_min_temp <- head(nov_min_temp, -1)
years <- c(1986:2022)
last_nov_min_temp <- data.frame(years, nov_min_temp)

### November maximum temperature ----

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
nov_max_temp <- df2$max_temp
nov_max_temp <- head(nov_max_temp, -1)
years <- c(1986:2022)
last_nov_max_temp <- data.frame(years, nov_max_temp)

### August maximum temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 8) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(aug_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df3 <- agg_tbl %>% as.data.frame()
august_max_temp <- df3
head(august_max_temp)

### Dec min temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 12) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(dec_min_temp=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df4 <- agg_tbl %>% as.data.frame()
df4

# converting to "last december"
dec_min_temp <- df4$dec_min_temp
dec_min_temp <- head(dec_min_temp, -1)
years <- c(1986:2022)
last_dec_min_temp <- data.frame(years, dec_min_temp)
head(last_dec_min_temp)

### January min temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 1) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(jan_min_temp=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df5 <- agg_tbl %>% as.data.frame()
january_min_temp <- df5

### September max temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 9) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(sept_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df6 <- agg_tbl %>% as.data.frame()
df6

# converting to "last september"
sept_max_temp <- df6$sept_max_temp
sept_max_temp <- head(sept_max_temp, -1)
years <- c(1986:2022)
last_sept_min_temp <- data.frame(years, sept_max_temp)

### July max temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 7) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(july_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df7 <- agg_tbl %>% as.data.frame()
july_max_temp <- df7

# Fall precipitation

# > September precipitation ----

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

# > October precipitation ----

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

# > November precipitation ----

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

head(last_fall_precip)

#change names
colnames(last_fall_precip) = c("years", "fall_precip")

head(last_fall_precip)

# Winter precipitation ----

### > December precipitation ----

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

### > January precipitation ----

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

### > February precipitation ----

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

#Winter yearly temperature standard deviation ----

### > January sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 1 ) %>% #filter only the month we want
  mutate(mass_norm = min / sd(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(jan_sd=sd(min), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
jan_min_temp_sd <- agg_tbl %>% as.data.frame()
jan_min_temp_sd

### > February sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 2 ) %>% #filter only the month we want
  mutate(mass_norm = min / sd(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(feb_sd=sd(min), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
feb_min_temp_sd <- agg_tbl %>% as.data.frame()
feb_min_temp_sd

### > December sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 12 ) %>% #filter only the month we want
  mutate(mass_norm = min / sd(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(dec_sd=sd(min), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
dec_min_temp_sd <- agg_tbl %>% as.data.frame()
dec_min_temp_sd

#last december 

values <- dec_min_temp_sd$dec_sd
years <- c(1986:2023)

min_temp_sd_last_dec <- data.frame(years, values)

#average across dataframes
values <- (min_temp_sd_last_dec$values + jan_min_temp_sd$jan_sd + feb_min_temp_sd$feb_sd)
values <- values/3
years <- c(1986:2023)

winter_temp_sd <- data.frame(years, values)

# changes column names

colnames(winter_temp_sd) = c("years", "winter_min_temp_sd")
head(winter_temp_sd)

# winter precipitation standard variation ----

### > january sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 1 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sd(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(jan_sd=sd(Precip), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
jan_precip_sd <- agg_tbl %>% as.data.frame()
jan_precip_sd

### > february sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 2 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sd(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(feb_sd=sd(Precip), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
feb_precip_sd <- agg_tbl %>% as.data.frame()
feb_precip_sd

### > december sd
agg_tbl <- dat %>% 
  select(Year, Month, Day, Precip) %>% #select what we need
  filter(Month == 12 ) %>% #filter only the month we want
  mutate(mass_norm = Precip / sd(Precip, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(dec_sd=sd(Precip), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
dec_precip_sd <- agg_tbl %>% as.data.frame()
dec_precip_sd

#last december 

values <- dec_precip_sd$dec_sd
years <- c(1986:2023)

last_dec_precip_sd <- data.frame(years, values)

#average across dataframes
values <- (last_dec_precip_sd$values + jan_min_temp_sd$jan_sd + feb_min_temp_sd$feb_sd)
values <- values/3
years <- c(1986:2023)

winter_precip_sd <- data.frame(years, values)

#rename columns
colnames(winter_precip_sd) = c("years", "winter_precip_sd")
head(winter_precip_sd)

# (this year) august and september max temp variation ----

### > august max temp sd ----
head(dat)
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 8 ) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(aug_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')

# convert tibble to df
agg_tbl
aug_max_temp_sd <- agg_tbl %>% as.data.frame()
aug_max_temp_sd

# rename column
colnames(aug_max_temp_sd) = c("years", "august_max_temp_sd")
aug_max_temp_sd

### > september max temp sd ----
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 9 ) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(september_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')

# convert tibble to df
agg_tbl
sept_max_temp_sd <- agg_tbl %>% as.data.frame()
sept_max_temp_sd

# rename column
colnames(aug_max_temp_sd) = c("years", "_max_temp_sd")
aug_max_temp_sd

# february min temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>% #select what we need
  filter(Month == 2) %>% #filter only the month we want
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(feb_min_temp=mean(min), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df8 <- agg_tbl %>% as.data.frame()
feb_min_temp <- df8

# february max temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 2) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(feb_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df9 <- agg_tbl %>% as.data.frame()
feb_max_temp <- df9

# june max temperature ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 6) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(june_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df10 <- agg_tbl %>% as.data.frame()
june_max_temp <- df10

# june max temp variation ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 6) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(june_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df11 <- agg_tbl %>% as.data.frame()
june_max_temp_sd <- df11

# july max temp variation ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 7) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(july_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df12 <- agg_tbl %>% as.data.frame()
july_max_temp_sd <- df12

# last december max temp ----

# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 12) %>% #filter only the month we want
  mutate(mass_norm = Air.max / mean(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(dec_max_temp=mean(Air.max), #name the new means column
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df13 <- agg_tbl %>% as.data.frame()
df13

# converting to "last december"
dec_max_temp <- df13$dec_max_temp
dec_max_temp <- head(dec_max_temp, -1)
years <- c(1986:2022)
last_dec_max_temp <- data.frame(years, dec_max_temp)
head(last_dec_max_temp)

# november max temperature variation ----

agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 11 ) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(nov_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
nov_max_temp_sd <- agg_tbl %>% as.data.frame()
nov_max_temp_sd

#last november 

nov_max_temp_sd <- nov_max_temp_sd$nov_max_temp_sd
years <- c(1986:2023)

last_nov_max_temp_sd <- data.frame(years, nov_max_temp_sd)

# december max temperature variation ----

agg_tbl <- dat %>% 
  select(Year, Month, Day, Air.max) %>% #select what we need
  filter(Month == 12 ) %>% #filter only the month we want
  mutate(mass_norm = Air.max / sd(Air.max, na.rm = TRUE)) %>% #calculate means at 'min'
  group_by(Year) %>% 
  summarise(dec_max_temp_sd=sd(Air.max), #name the new means column
            .groups = 'drop')

#convert tibble to df
agg_tbl
dec_max_temp_sd <- agg_tbl %>% as.data.frame()
dec_max_temp_sd

#last december 

dec_max_temp_sd <- dec_max_temp_sd$dec_max_temp_sd
years <- c(1986:2023)

last_dec_max_temp_sd <- data.frame(years, nov_max_temp_sd)

# last september's max temperature 
head(sept_max_temp_sd)
last_sept_max_temp_sd <- sept_max_temp_sd$september_max_temp_sd
last_sept_max_temp_sd <- head(last_sept_max_temp_sd, -1)
years <- c(1986:2022)

last_sept_max_temp_sd <- data.frame(years, last_sept_max_temp_sd)

# upload occurance data
butterflies <- read.csv("~/Langes-metalmark-climate/Data/abundance.csv")

# make n-1 variable

butterflies$N.1 <- butterflies$N

#merge dataframes
head(butterflies)


butterflies <- butterflies %>%
  left_join(last_nov_min_temp, join_by(Year == years)) %>%
  left_join(last_nov_max_temp, join_by(Year == years)) %>% 
  left_join(august_max_temp, join_by(Year)) %>%
  left_join(last_dec_min_temp, join_by(Year == years)) %>%
  left_join(july_max_temp, join_by(Year)) %>%
  left_join(january_min_temp, join_by(Year)) %>%
  left_join(last_sept_min_temp, join_by(Year == years)) %>%
  left_join(last_fall_precip, join_by(Year == years)) %>%
  left_join(precip_winter, join_by(Year == years)) %>%
  left_join(winter_temp_sd, join_by(Year == years)) %>%
  left_join(winter_precip_sd, join_by(Year == years)) %>%
  left_join(aug_max_temp_sd, join_by(Year == years)) %>%
  left_join(sept_max_temp_sd, join_by(Year)) %>%
  left_join(feb_min_temp, join_by(Year)) %>%
  left_join(feb_max_temp, join_by(Year))%>%
  left_join(june_max_temp, join_by(Year)) %>%
  left_join(june_max_temp_sd, join_by(Year)) %>% 
  left_join(july_max_temp_sd, join_by(Year)) %>%
  left_join(last_dec_max_temp, join_by(Year == years)) %>%
  left_join(last_dec_max_temp_sd, join_by(Year == years)) %>%
  left_join(last_nov_max_temp_sd, join_by(Year == years)) %>%
  left_join(last_sept_max_temp_sd, join_by(Year == years))

  
head(butterflies)
  
write.csv(butterflies, "~/Langes-metalmark-climate/Data/butterflies.csv")
