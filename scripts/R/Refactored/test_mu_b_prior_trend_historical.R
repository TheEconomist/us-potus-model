# script to search for mu_b using state-level trends
library(tidyverse)
library(purrr)
library(urbnmapr)
library(caret)

# functions
rmse <- function(x){sqrt(mean(x^2))}



# start loop --------------------------------------------------------------
# all years to test
YEARS <- c(2000,2004,2008,2012,2016)

# empty list to store outcomes
ALL_PREDICTIONS <- vector('list',length(YEARS))

for(i in 1:length(ALL_PREDICTIONS)){
  # master vars
  RUN_YEAR <- YEARS[i]
  
  print(sprintf("Running for year %s",RUN_YEAR))
  
  # input data --------------------------------------------------------------
  # get results
  results <- politicaldata::pres_results 
  
  # make two-party
  results <- results %>% 
    mutate(two_party_votes = (dem + rep) * total_votes,
           dem_two_party_share = dem / (dem + rep)) %>%
    dplyr::select(state,year,dem_two_party_share,two_party_votes) 
  
  # get state-level lean
  results <- results %>%
    group_by(year) %>%
    mutate(national_dem_two_party_share = sum(dem_two_party_share * two_party_votes)/sum(two_party_votes)) %>%
    mutate(dem_two_party_share_lean = dem_two_party_share - national_dem_two_party_share) %>%
    ungroup()
  
  # get lean for that year
  outcome_lean_run_year <- results[results$year==RUN_YEAR,] %>% select(state,dem_two_party_share_lean)
  
  # and for the year prior
  outcome_lean_prior_year <- results[results$year==RUN_YEAR-4,] %>% select(state,dem_two_party_share_lean)
  
  # filter out everything at and after that year
  results <- results %>% filter(year < RUN_YEAR)
  
  
  # fit models in each state ------------------------------------------------
  # optimize the loess span for each state
  models <- results %>%
    #filter(state %in% c('AZ','FL','WI')) %>%
    group_by(state) %>%
    do(mod = train(dem_two_party_share_lean ~ year,
                   data=.,
                   method='gamboost',
                   trControl = trainControl(method='LOOCV',allowParallel = TRUE, verboseIter=FALSE), 
                   # tuneGrid = expand.grid(degree=1,
                   #                        span=c(0.2,0.3,0.4,0.5,0.6,0.7)),
                   tuneLength = 5,
    )
    )
  
  
  # get model error by state
  model_error <- tibble(state = models$state,
                        error = sapply(models$mod,
                                       function(x){last(x$results$RMSE)}))
  
  
  
  # make predictions for that year ------------------------------------------
  # make predictions for 2020
  predictions <- lapply(1:nrow(models),
                        function(x){
                          model <- models$mod[[x]]
                          state <- models$state[x]
                          
                          dat <- tibble(year=RUN_YEAR) %>%
                            mutate(state = state,
                                   pred = predict(object = model, newdata = .))
                          
                          return(dat)
                          
                        }) %>%
    do.call('bind_rows',.)
  
  predictions <- 
  
  # return them
  ALL_PREDICTIONS[[i]] <- predictions
  
}


# aggregate up ------------------------------------------------------------
# all predictions
ALL_PREDICTIONS <- ALL_PREDICTIONS %>%
  do.call('bind_rows',.) 

# add contest
results <- politicaldata::pres_results 

# make two-party
results <- results %>% 
  mutate(two_party_votes = (dem + rep) * total_votes,
         dem_two_party_share = dem / (dem + rep)) %>%
  dplyr::select(state,year,dem_two_party_share,two_party_votes) 

# get state-level lean
results <- results %>%
  group_by(year) %>%
  mutate(national_dem_two_party_share = sum(dem_two_party_share * two_party_votes)/sum(two_party_votes)) %>%
  mutate(dem_two_party_share_lean = dem_two_party_share - national_dem_two_party_share) %>%
  ungroup()

# generate lagged variable
context <- results %>%
  group_by(state) %>%
  mutate(actual = dem_two_party_share_lean,
         prior = lag(actual)) %>%
  select(state,year,actual,prior)


ALL_PREDICTIONS <- ALL_PREDICTIONS %>%
  left_join(context, by = c("year", "state"))

# rmse
ALL_PREDICTIONS %>%
  group_by(year) %>%
  summarise(model_error = rmse(pred - actual),
            lag_error = rmse(prior - actual))


