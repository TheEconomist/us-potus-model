# script to get RMSE for polls in states in the past month of the election
library(tidyverse)
library(lubridate)
library(parallel)

# download archive
polls <- read_csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv')

# look at dates
(mdy(polls$polldate) - mdy(polls$electiondate))  %>% as.numeric %>% hist 

# region
polls <- polls %>% 
  left_join(read_csv('data/state_region_crosswalk.csv') %>% 
              dplyr::select(location=state_abb,region))

# only president, at state level
polls <- polls %>% filter(type_simple == 'Pres-G')

# limit to last week
polls <- polls %>%
  mutate(daystil = difftime(mdy(electiondate),mdy(polldate),units = 'days')) %>%
  filter(daystil <= 21) %>%
  mutate(cand1_actual = cand1_actual / (cand1_actual + cand2_actual), cand1_pct = cand1_pct / (cand1_pct + cand2_pct))

# error on candidate one percent
polls %>%
  filter(location!='US') %>%
  #group_by(year) %>%
  summarise(median_error = median(abs(cand1_pct - cand1_actual)),
            rmse = sqrt(mean((cand1_pct - cand1_actual)^2)))

# plot
polls %>%
    group_by(state=location,year) %>%
    summarise(cand1_pct = mean(cand1_pct,na.rm=T),
              cand1_actual = unique(cand1_actual,na.rm=T)) %>%
  filter(state != "DC") %>%
  ggplot(.,aes(x=cand1_pct,y=cand1_actual)) +
  geom_text(aes(label=state)) +
  geom_abline() +
  geom_smooth(method='lm') +
  theme_minimal() +
  facet_wrap(~year)

# look at errors after subtracting national error
poll_error_decomp <- polls %>%
  filter(location != 'US') %>%
  group_by(state=location,year) %>%
  summarise(cand1_pct = mean(cand1_pct,na.rm=T),
            cand1_actual = unique(cand1_actual,na.rm=T)) %>%
  left_join(
    # plot
    polls %>%
      filter(location == 'US') %>%
      group_by(year) %>%
      summarise(cand1_pct = mean(cand1_pct,na.rm=T),
                cand1_actual = unique(cand1_actual,na.rm=T)) %>%
      mutate(national_error = cand1_pct - cand1_actual) %>%
      dplyr::select(year,national_error)
    ) %>%
  mutate(cand1_pct_adjusted = cand1_pct - national_error)
  

poll_error_decomp %>%
  #group_by(year) %>%
  ungroup() %>%
  summarise(adjusted_rmse = sqrt(mean(c(cand1_pct_adjusted - cand1_actual)^2)),
            unadjusted_rmse = sqrt(mean(c(cand1_pct - cand1_actual)^2)))

poll_error_decomp %>%
  filter(state != "DC") %>%
  ggplot(.) +
  geom_abline() +
  # raw errors
  geom_text(aes(x=cand1_pct_adjusted,y=cand1_actual,label=state,col='adjusted')) +
  geom_smooth(aes(x=cand1_pct_adjusted,y=cand1_actual,label=state,col='adjusted'),
              method='lm') +
  # after subtracting natioanl error
  geom_text(aes(x=cand1_pct,y=cand1_actual,label=state,col='raw')) +
  geom_smooth(aes(x=cand1_pct,y=cand1_actual,label=state,col='raw'),
              method='lm') +
  # theme stuff
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
  cor(.,use = 'complete.obs')  


# hierarchical decomposition ----------------------------------------------
# get state and national errors separate
state_error <- polls %>%
  filter(location!='US') %>%
  group_by(year,state=location,region) %>%
  summarise(total_error = mean(abs(cand1_actual - cand1_pct),na.rm=T))

regional_error <- polls %>%
  filter(location!='US') %>%
  group_by(year,region) %>%
  summarise(regional_error = mean(abs(cand1_actual - cand1_pct),na.rm=T))

national_error <- polls %>%
  filter(location=='US') %>%
  group_by(year) %>%
  summarise(national_error = mean(abs(cand1_actual - cand1_pct),na.rm=T))

# merge
poll_errors <- state_error %>%
  left_join(national_error)  %>%
  left_join(regional_error)

# calc local error with sum of squares formula (sds aren't additive but variances are)
errors <- poll_errors %>%
  ungroup() %>%
  summarise(national_error = sqrt(mean(national_error^2)),
            regional_error = sqrt(mean(regional_error^2)),
            total_error = sqrt(mean(total_error^2))) %>%
  mutate(regional_error = sqrt(regional_error^2 - national_error^2)) %>%
  mutate(state_error = sqrt(total_error^2 - regional_error^2 - national_error^2)) %>%
  dplyr::select(national_error, regional_error, state_error)

# simulate
errors

# difference
national_errors <- rnorm(1e04, 0, errors$national_error)
regional_errors <- replicate(1e04,rnorm(length(unique(poll_errors$region)), 0, errors$state_error))
state_errors <- replicate(1e04,rnorm(51, 0, errors$state_error))

state_and_national_errors <- pblapply(1:length(national_errors),
         cl = detectCores() -1,
         function(x){
           state_region <- tibble(state = unique(poll_errors$state)) %>%
             left_join(poll_errors %>% 
                         ungroup() %>%
                         dplyr::select(state,region) %>% distinct) %>%
             left_join(tibble(region = unique(poll_errors$region),
                              regional_error = regional_errors[,x])) %>%
             left_join(tibble(state = unique(poll_errors$state),
                              state_error = state_errors[,x]))
           
           state_region %>%
             mutate(error = state_error + regional_error + national_errors[x]) %>% return
         })


state_and_national_errors %>% 
  do.call('bind_rows',.) %>%
  group_by(state) %>%
  summarise(sd = sd(error)) %>%
  ungroup() %>%
  summarise(sd = mean(sd))

state_and_national_errors %>% 
  do.call('bind_rows',.) %>%
  dplyr::select(-c(regional_error,state_error,region)) %>%
  group_by(state) %>%
  mutate(trial = row_number()) %>%
  group_by(trial) %>%
  spread(state,error) %>%
  ungroup() %>%
  dplyr::select(-trial) %>%
  cor 

