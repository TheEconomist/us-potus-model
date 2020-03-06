# script to search for mu_b using a national forecast and state-level 
# trends in partisan lean, all optimized with caret

library(tidyverse)
library(purrr)
library(urbnmapr)
library(caret)
library(caretEnsemble)
library(colorspace)
library(pbapply)
library(lqmm)
library(mvtnorm)

# fit national model ------------------------------------------------------
abramowitz <- read.csv('data/abramowitz_data.csv')  %>%
  left_join(read.csv('data/ANES_swing_voters.csv')) %>%
  filter(year>1948)

# train a caret model to predict demvote with incvote ~ q2gdp + juneapp + swing voters
# use a bunch of different regularization methods and combine them
model_list  <- c(
  "glmnet","brnn","blassoAveraged",
  "lars2","penalized","rqlasso","relaxo",
  "blasso","bridge"
  )

prior_model <- caretList(
  incvote ~ q2gdp*juneapp + q2gdp:ANES_share_swing_voters + juneapp:ANES_share_swing_voters,
  data = abramowitz,
  #method = "glmboost",
  methodList = model_list, 
  metric = "RMSE",
  trControl = trainControl(
    #method = "LOOCV",
    method = "repeatedcv", number = 7, repeats = 10, savePredictions = 'final',
    allowParallel = T,verboseIter = T),
  tuneLength = 10)

# combine into one regularized ensemble learner
stack <- caretStack(prior_model, 
                    method = "glmboost",
                    metric = "RMSE",
                    trControl = trainControl(
                      method = "repeatedcv", number = 7, repeats = 10, savePredictions = 'final',
                      allowParallel = T,verboseIter = T),
                    tuneLength = 10)

stack

# find the optimal parameters
# best = which(rownames(prior_model$results) == rownames(prior_model$bestTune))
# best_result = prior_model$results[best, ]
# rownames(best_result) = NULL
# best_result
# 
# coef(prior_model$finalModel)#,best_result$lambda)

# make predictions
NATIONAL_PRIOR_INCUMBENT_TWO_PARTY_SHARE <- predict(stack,
                                                    newdata = tibble(q2gdp = 1,
                                                                     juneapp = -10,
                                                                     ANES_share_swing_voters = 0.05)) / 100

# negate when a rep is president
NATIONAL_PRIOR_DEM_TWO_PARTY_SHARE <- 1 - NATIONAL_PRIOR_INCUMBENT_TWO_PARTY_SHARE

NATIONAL_MODEL_ERROR <- sqrt(mean(stack$error$RMSE^2,na.rm=T)) / 100

cat(sprintf('Prior Democratic two-party vote is %s\nWith a standard error of %s',
            round(NATIONAL_PRIOR_DEM_TWO_PARTY_SHARE,3),
            round(NATIONAL_MODEL_ERROR,3)))

# grid of predictions
expand.grid(q2gdp = c(-2,-1,0,1,2),
            juneapp = seq(-16,0),
            year=2020,
            ANES_share_swing_voters = 0.1) %>%
  mutate(incumbent_share = predict(stack,.)) %>%
  ggplot(.,aes(x=juneapp,y=incumbent_share,col=factor(q2gdp),fill=factor(q2gdp),group=factor(q2gdp))) +
  geom_hline(yintercept = 50,linetype=2,col='gray70') +
  geom_vline(xintercept = 0,linetype=2,col='gray70') +
  # error
  # geom_ribbon(aes(ymin = incumbent_share - best_result$RMSE*2,
  #                 ymax = incumbent_share + best_result$RMSE*2),
  #             col=NA,alpha=0.1,show.legend = F) +
  # prediction
  geom_line() +
  # trump share needed for two-party win
  geom_hline(yintercept = 48.6,linetype=2,col='black') +
  annotate(geom='text',x=-16,y=48.7,label='Estimated Trump share needed for electoral college victory',
            hjust=0,col='black') +
  # labels and such
  labs(x='Trump approval rating(in June)',
       y='',
       title='Predicted Trump vote share of the national popular vote, two-party %') +
  scale_color_discrete_qualitative(name='Q2 GDP growth',palette = 'Warm') +
  scale_x_continuous(breaks=seq(-20,20,2)) +
  scale_y_continuous(breaks=seq(0,100,1)) +
  theme_minimal() +
  theme(legend.position = 'top',
        legend.justification = 'left',
        panel.grid.minor = element_blank())  

# now, state. start by reading in the data --------------------------------
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
  mutate(dem_two_party_share_lean = dem_two_party_share - national_dem_two_party_share)

# graph for some states
ggplot(results %>%
         filter(state %in% c('FL','NC','AZ','WI','PA','MI','NH','MN','AZ','TX','GA')),
       aes(x=year,y=dem_two_party_share_lean,col=state)) +
  geom_point() +
  geom_smooth(method='loess',span=0.8,se=F) +
  theme_minimal()


# fit models in each state ------------------------------------------------
# optimize the loess span for each state
# models <- results %>%
#   #filter(state %in% c('AZ','FL','WI')) %>%
#   group_by(state) %>%
#   do(mod = train(dem_two_party_share_lean ~ year,
#                data=.,
#                method='gamboost',
#                trControl = trainControl(method='LOOCV',allowParallel = TRUE, verboseIter=TRUE), 
#                # tuneGrid = expand.grid(degree=1,
#                #                        span=c(0.2,0.3,0.4,0.5,0.6,0.7)),
#                tuneLength = 10,
#                )
#   )
# 
# write_rds(models,'data/state_prior_models.rds',compress = 'gz')
models <- read_rds('data/state_prior_models.rds')

beepr::beep(2)

# get model error by state
model_error <- tibble(state = models$state,
                      error = sapply(models$mod,
                                     function(x){last(x$results$RMSE)}))


# is this any better than just taking the lagged value
model_error %>%
  left_join(results %>%
             group_by(state) %>%
              mutate(error = dem_two_party_share_lean - lag(dem_two_party_share_lean)) %>%
              na.omit() %>%
              summarise(lagged_value_rmse = sqrt(mean(error^2)))
  )



# defaulting to loess model -----------------------------------------------


# # span should be 0.6-ish
# models <- results %>%
#   #filter(state %in% c('AZ','FL','WI')) %>%
#   group_by(state) %>%
#   do(mod = loess(dem_two_party_share_lean ~ year,
#                  data=.,
#                  span=0.6,
#                  control = loess.control(surface = "direct"))
#   )


# predict for 2020 and EDA ------------------------------------------------
# make predictions for 2020
predictions <- lapply(1:nrow(models),
      function(x){
        model <- models$mod[[x]]
        state <- models$state[x]
        
        dat <- tibble(year=seq(1976,2020)) %>%
          mutate(state = state,
                 dem_two_party_share_lean_hat = predict(object = model,
                                                            newdata = .))
        
        return(dat)
      
        }) %>%
  do.call('bind_rows',.)


# check historical predictions with actual
predictions <- predictions %>%
  left_join(results,by=c('year','state'))

# compare with loess
predictions %>%
  filter(state %in% c('FL','NC','AZ','WI','PA','MI','NH','MN','AZ','TX','GA')) %>%
  ggplot(.,
       aes(x=year,col=state,group=state)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(y=dem_two_party_share_lean)) + 
  scale_y_continuous(breaks = seq(-1,1,0.05), labels=function(x){round(x*100)}) +
  scale_x_continuous(breaks=seq(1900,3000,4)) +
  # model predicted
  geom_line(aes(y=dem_two_party_share_lean_hat),linetype=2) +
  geom_line(data=. %>% filter(year<=2016),
            aes(y=dem_two_party_share_lean_hat)) +
  # loess stuff for checking whether the model fits well
  # geom_smooth(aes(y=dem_two_party_share_lean)) +
  # labels
  labs(x='Year',y='',
       subtitle='State-level Democratic share of the two-party vote minus national\nDemocratic share of the two-party vote, percentage points') +
  theme_minimal() + 
  theme(legend.position = 'top',legend.justification = 'left',
        panel.grid.minor = element_blank()) +
  scale_color_viridis_d(name='State')



# make predictions for 2020 -----------------------------------------------
# this can make predictions with the model
predictions_2020 <- predictions %>%
  filter(year==2020) %>%
  mutate(dem_two_party_share_hat = dem_two_party_share_lean_hat + NATIONAL_PRIOR_DEM_TWO_PARTY_SHARE)

# but actually, just using the lagged value is better
predictions_2020 <- predictions %>%
  group_by(state) %>%
  filter(year %in% c(2016,2020)) %>%
  mutate(dem_two_party_share_hat = lag(dem_two_party_share_lean) + NATIONAL_PRIOR_DEM_TWO_PARTY_SHARE) %>%
  filter(year==2020)  %>%
  ungroup()

# plot without leaners
urbnmapr::states %>%
  left_join(predictions_2020 %>% dplyr::select(state_abbv = state,
                                        dem_two_party_share_hat)) %>%
  mutate(dem_margin = cut(-(dem_two_party_share_hat-0.5)*2,
                          breaks = c(-1,-0.2,-0.1,-0.05,
                                     #0,
                                     0.05,0.1,0.2,1),
                          labels = c("Democratic +20 or higher",
                                     "+20 to +10",
                                     "+10 to +5",
                                     'Even',
                                     "+5 to +10",
                                     "+10 to +20",
                                     "Republican +20 or higher")
                          )) %>%
  # plot
  ggplot(.,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=dem_margin),col=NA) +
  scale_fill_manual(name="",
                    values=c("Democratic +20 or higher" = "#21618C",
                             "+20 to +10"="#2E86C1",
                             "+10 to +5" = "#A9CCE3",
                             'Even' = 'gray80',
                             "+5 to +10" = "#EC7063",
                             "+10 to +20"="#CB4335",
                             "Republican +20 or higher" = "#943126")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0))

# plot with leaners
urbnmapr::states %>%
  left_join(predictions_2020 %>% dplyr::select(state_abbv = state,
                                        dem_two_party_share_hat)) %>%
  mutate(dem_margin = cut(-(dem_two_party_share_hat-0.5)*2,
                          breaks = c(-1,-0.2,-0.1,-0.05,
                                     0,
                                     0.05,0.1,0.2,1),
                          labels = c("Democratic +20 or higher",
                                     "+20 to +10",
                                     "+10 to +5",
                                     "Even to Democratic +5",
                                     "Even to Republican +5",
                                     "+5 to +10",
                                     "+10 to +20",
                                     "Republican +20 or higher")
  )) %>%
  # plot
  ggplot(.,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=dem_margin),col=NA) +
  scale_fill_manual(name="",
                    values=c("Democratic +20 or higher" = "#21618C",
                             "+20 to +10"="#2E86C1",
                             "+10 to +5" = "#A9CCE3",
                             "Even to Democratic +5" = "#D6EAF8",
                             "Even to Republican +5"= "#FADBD8", 
                             "+5 to +10" = "#EC7063",
                             "+10 to +20"="#CB4335",
                             "Republican +20 or higher" = "#943126")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0))


# simulations? ------------------------------------------------------------
# get correlation matrix
cor.mat <- predictions %>%
  filter(year >= 2000) %>%
  dplyr::select(year,state,dem_two_party_share_lean_hat) %>%
  group_by(state) %>%
  mutate(dem_two_party_share_lean_hat = dem_two_party_share_lean_hat - lag(dem_two_party_share_lean_hat)) %>%
  spread(state,dem_two_party_share_lean_hat) %>%
  dplyr::select(-year) %>%
  na.omit() %>%
  cor %>%
  as.matrix()

cor.mat <- make.positive.definite(cor.mat)

# just make everything super correlated
# cor.mat[T] <- 0.9
# diag(cor.mat) <- 1

# get 100k polling errors and add to mu
errors <- rmvnorm(1000000, 
                  mean = rep(0,nrow(predictions_2020)),
                  sigma = cor.mat) 

#plot(predictions_2020$dem_two_party_share_hat, errors[runif(1,0,nrow(errors)),])

# add thsoe errors times state-level sigma and error to the predicted mean to get trials
trials <- pblapply(1:ncol(errors),
                   cl=parallel::detectCores()-1,
      function(x){
        # national error is 0.026
        national_model_error <- NATIONAL_MODEL_ERROR # needs to be in two-party terms
        # state model error plus extrapolation error
        state_model_error <- model_error$error[x] + 0.02
        
        # prediction + (z score error * sigma)
        predictions_2020$dem_two_party_share_hat[x] + 
          (errors[,x] * 
             (national_model_error + state_model_error)) # simulate 5 pct pt natl polling error
      }) %>%
  do.call('cbind',.)

# add names
trials <- trials %>%
  as.data.frame() %>%
  setNames(.,predictions_2020$state)

# make long format
trials <- trials %>%
  gather(state,dem_two_party_share_hat) %>%
  mutate(dem_two_party_share_hat = case_when(dem_two_party_share_hat > 1 ~ 1,
                                             dem_two_party_share_hat < 0 ~ 0,
                                             TRUE ~ dem_two_party_share_hat)) %>%
  group_by(state) %>%
  mutate(trial = row_number()) %>%
  ungroup()
 
# add evs
trials <- trials %>%
  left_join(read_csv("data/state_evs.csv"))

# sum up
sim_evs <- trials %>%
  group_by(trial) %>%
  summarise(dem_ev = sum(ev * (dem_two_party_share_hat >= 0.5))) %>%
  ungroup()

p_dem_win <- nrow(sim_evs[sim_evs$dem_ev >= 270,]) / nrow(sim_evs)

p_dem_win

# base win?
predictions_2020 %>%
  left_join(read_csv("data/state_evs.csv")) %>%
  summarise(dem_ev = sum(ev * (dem_two_party_share_hat >= 0.5))) 

# plot
ggplot(sim_evs,aes(x=dem_ev,fill=ifelse(dem_ev>=270,'Democratic','Republican'))) +
  geom_vline(xintercept=270,) +
  geom_histogram(binwidth=1) +
  scale_y_continuous(labels=function(x){
    paste0(round(x/nrow(sim_evs)*100),'%')
    }) +
  scale_fill_manual(name='Winner',
                    values=c('Democratic'='blue',
                             'Republican'='red')) +
  labs(subtitle=sprintf('p(Democratic Electoral College victory) = %s',round(p_dem_win,2)),
       x='Democratic electoral votes',
       y='Probability') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top',
        legend.justification = 'left') 


# cumulative probability density
evs_pdf <- sim_evs %>%
  group_by(dem_ev) %>%
  summarise(n=n()) %>%
  mutate(prob = n/sum(n)) %>%
  arrange(desc(dem_ev)) %>%
  mutate(cumprob = cumsum(prob))

# ggplot(evs_pdf, aes(x=dem_ev,y=cumprob)) +
#   geom_line() +
#   theme_classic()


