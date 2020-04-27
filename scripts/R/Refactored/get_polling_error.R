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

# plot
polls %>%
    group_by(state=location,year) %>%
    summarise(cand1_pct = mean(cand1_pct-cand2_pct,na.rm=T),
              cand1_actual = unique(cand1_actual-cand2_actual)) %>%
  filter(state != "DC") %>%
  ggplot(.,aes(x=cand1_pct,y=cand1_actual)) +
  geom_text(aes(label=state)) +
  geom_abline() +
  geom_smooth(method='lm') +
  theme_minimal() +
  facet_wrap(~year)



# correlated in which states?
polls %>%
  expand(location,year) %>%
  left_join(polls %>%
            group_by(location,year) %>%
            summarise(error = median(((cand1_actual-cand2_actual) - (cand1_pct-cand2_pct)))) %>%
            ungroup()) %>%
  group_by(year) %>%
  mutate(error = ifelse(is.na(error),mean(error,na.rm=T),error)) %>%
  ungroup() %>%
  spread(location,error) %>%
  select(-year) %>%
  cor(.,use = 'complete.obs')  %>% View



