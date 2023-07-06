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

# November min temperature


# Group by mean using dplyr
agg_tbl <- dat %>% 
  select(Year, Month, Day, min) %>%
  mutate(.keep = )
  mutate(mass_norm = min / mean(min, na.rm = TRUE)) %>%
  group_by(Month) %>% 
  summarise(mean_salary=mean(Day),
            .groups = 'drop')
agg_tbl

# Convert tibble to df
df1 <- agg_tbl %>% as.data.frame()
df1

# November maximum temperature

# August maximum temperature

# Dec min temperature

# January min temperature

# September max temperature

# July max temperature

# Fall precipitation

# Winter precipitation

#Winter temperature standard deviation

#winter precipitation standard variation