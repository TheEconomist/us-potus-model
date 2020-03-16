# script to get RMSE for polls in states in the past month of the election
library(tidyverse)
library(lubridate)

# download archive
polls <- read_csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv')

# polls since 2000
#polls <- polls %>% filter(year > 2000)

# only president, at state level
polls <- polls %>% filter(type_simple == 'Pres-G')

# limit to last week
polls <- polls %>%
  mutate(daystil = difftime(mdy(electiondate),mdy(polldate),units = 'days')) %>%
  filter(daystil<=7)

# error on candidate one percent
polls %>%
  #group_by(year,national=location=='US') %>%
  summarise(median_error = median(abs(cand1_actual - cand1_pct)),
            rmse = sqrt(mean((cand1_actual - cand1_pct)^2)))


