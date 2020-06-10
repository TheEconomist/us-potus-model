## Desc
# Refactored version run file

## Setup
rm(list = ls())
options(mc.cores = 2)

## Libraries
{
  library(tidyverse, quietly = TRUE)
  library(rstan, quietly = TRUE)
  library(purrr, quietly = TRUE)
  library(stringr, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(curl, quietly = TRUE)
  library(shinystan, quietly = TRUE)
  library(rmarkdown, quietly = TRUE)
  library(survey, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(pbapply, quietly = TRUE)
  library(here, quietly = TRUE)
  library(boot, quietly = TRUE)
  library(lqmm, quietly = TRUE)
  library(caret, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(ggrepel, quietly = TRUE)
  #library(glmnet, quietly = TRUE)
}


cov_matrix <- function(n, sigma2, rho){
    m <- matrix(nrow = n, ncol = n)
    m[upper.tri(m)] <- rho
    m[lower.tri(m)] <- rho
    diag(m) <- 1
    (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

## Master variables
RUN_DATE <- min(ymd('2016-11-08'),Sys.Date())

election_day <- ymd("2016-11-08")
start_date <- as.Date("2016-03-01") # Keeping all polls after March 1, 2016


# wrangle polls -----------------------------------------------------------
# read
#setwd(here("data/"))
all_polls <- read.csv("data/all_polls.csv", stringsAsFactors = FALSE, header = TRUE)


# select relevant columns from HufFPost polls
all_polls <- all_polls %>%
  dplyr::select(state, pollster, number.of.observations, population, mode, 
                start.date, 
                end.date,
                clinton, trump, undecided, other, johnson, mcmullin)


# make sure we've got nothing from the futuree
all_polls <- all_polls %>%
  filter(ymd(end.date) <= RUN_DATE)


# basic mutations
df <- all_polls %>% 
  tbl_df %>%
  rename(n = number.of.observations) %>%
  mutate(begin = ymd(start.date),
         end   = ymd(end.date),
         t = end - (1 + as.numeric(end-begin)) %/% 2) %>%
  filter(t >= start_date & !is.na(t)
         & (population == "Likely Voters" | 
              population == "Registered Voters" | 
              population == "Adults") # get rid of disaggregated polls
         & n > 1) 

# pollster mutations
df <- df %>%
  mutate(pollster = str_extract(pollster, pattern = "[A-z0-9 ]+") %>% sub("\\s+$", "", .),
         pollster = replace(pollster, pollster == "Fox News", "FOX"), # Fixing inconsistencies in pollster names
         pollster = replace(pollster, pollster == "WashPost", "Washington Post"),
         pollster = replace(pollster, pollster == "ABC News", "ABC"),
         undecided = ifelse(is.na(undecided), 0, undecided),
         other = ifelse(is.na(other), 0, other) + 
           ifelse(is.na(johnson), 0, johnson) + 
           ifelse(is.na(mcmullin), 0, mcmullin))

# vote shares etc
df <- df %>%
  mutate(two_party_sum = clinton + trump,
         polltype = as.integer(as.character(recode(population, 
                                                   "Likely Voters" = "0", 
                                                   "Registered Voters" = "1",
                                                   "Adults" = "2"))), 
         n_respondents = round(n),
         # clinton
         n_clinton = round(n * clinton/100),
         p_clinton = clinton/two_party_sum,
         # trump
         n_trump = round(n * trump/100),
         p_trump = trump/two_party_sum,
         # third-party
         n_other = round(n * other/100),
         p_other = other/100)




# prepare stan date -----------------------------------------------------------

# create correlation matrix ---------------------------------------------

#here("data")
polls_2012 <- read.csv("data/potus_results_76_16.csv")
polls_2012 <- polls_2012 %>% 
  select(year, state, dem) %>%
  spread(state, dem) %>% select(-year)
state_correlation <- cor(polls_2012)  

#state_correlation_error <- state_correlation # covariance for backward walk
state_correlation_error <- cov_matrix(51, 0.1^2, .8) # 0.08^2
state_correlation_error <- state_correlation_error * state_correlation

#state_correlation_mu_b_T <- state_correlation # covariance for prior e-day prediction
state_correlation_mu_b_T <- cov_matrix(n = 51, sigma2 = 0.09, rho = 0.5) #1/20
state_correlation_mu_b_T <- state_correlation_mu_b_T * state_correlation

# state_correlation_mu_b_walk <- state_correlation
state_correlation_mu_b_walk <- cov_matrix(51, (0.015)^2, 0.75) 
state_correlation_mu_b_walk <- state_correlation_mu_b_walk * state_correlation

# Numerical indices passed to Stan for states, days, weeks, pollsters
df <- df %>% 
  mutate(poll_day = t - min(t) + 1,
         # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
         index_s = as.numeric(factor(as.character(state),
                                     levels = c('--',colnames(state_correlation)))),  # ensure levels are same as all 50 names in sate_correlation
         index_t = 1 + as.numeric(t) - min(as.numeric(t)),
         index_p = as.numeric(as.factor(as.character(pollster))))  

T <- as.integer(round(difftime(election_day, min(df$start.date))))

# selections
df <- df %>%
  arrange(state, t, polltype, two_party_sum) %>% 
  distinct(state, t, pollster, .keep_all = TRUE) %>%
  select(
    # poll information
    state, t, begin, end, pollster, polltype, method = mode, n_respondents, 
    # vote shares
    p_clinton, n_clinton, 
    p_trump, n_trump, 
    p_other, n_other, poll_day, index_s, index_p, index_t) %>%
  mutate(index_s = ifelse(index_s == 1, 52, index_s - 1)) # national index = 51

# Useful vectors ---------
# we want to select all states, so we comment this out
# and later declare all_polled_states to be all of them + national '--'
all_polled_states <- df$state %>% unique %>% sort

# day indices
ndays <- max(df$t) - min(df$t)
all_t <- min(df$t) + days(0:(ndays))
all_t_until_election <- min(all_t) + days(0:(election_day - min(all_t)))

# pollster indices
all_pollsters <- levels(as.factor(as.character(df$pollster)))


# Reading 2012 election data to --------- 
# (1) get state_names and EV        
# (2) set priors on mu_b and alpha,
# (3) get state_weights,           
#setwd(here("data/"))
states2012 <- read.csv("data/2012.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(score = obama_count / (obama_count + romney_count),
         national_score = sum(obama_count)/sum(obama_count + romney_count),
         delta = score - national_score,
         share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
         /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
  arrange(state) 

rownames(states2012) <- states2012$state

# get state incdices
all_states <- states2012$state
state_name <- states2012$state_name
names(state_name) <- states2012$state

# set prior differences
prior_diff_score <- states2012$delta
names(prior_diff_score) <- all_states

# set state weights
state_weights <- c(states2012$share_national_vote / sum(states2012$share_national_vote))
names(state_weights) <- c(states2012$state)

# electoral votes, by state:
ev_state <- states2012$ev
names(ev_state) <- states2012$state


##### Creating priors --------------
# read in abramowitz data
#setwd(here("data/"))
abramowitz <- read.csv('data/abramowitz_data.csv') %>% filter(year != 2016)

# train a caret model to predict demvote 
prior_model <- caret::train(
  incvote ~  juneapp , #+ year:q2gdp + year:juneapp
  data = abramowitz,
  #method = "glmnet",
  trControl = trainControl(
    method = "LOOCV"),
  tuneLength = 50)

# find the optimal parameters
best = which(rownames(prior_model$results) == rownames(prior_model$bestTune))
best_result = prior_model$results[best, ]
rownames(best_result) = NULL
best_result

# make predictions
national_mu_prior <- predict(prior_model,newdata = tibble(q2gdp = 1.1,
                                                          juneapp = 4,
                                                          year = 2016))
cat(sprintf('Prior Clinton two-party vote is %s\nWith a standard error of %s',
            round(national_mu_prior/100,3),round(best_result$RMSE/100,3)))

# on correct scale
national_mu_prior <- national_mu_prior / 100
national_sigma_prior <- best_result$RMSE / 100

# Mean of the mu_b_prior
# 0.486 is the predicted Clinton share of the national vote according to the Lewis-Beck & Tien model
# https://pollyvote.com/en/components/econometric-models/lewis-beck-tien/
mu_b_prior <- logit(national_mu_prior + prior_diff_score)

# The model uses national polls to complement state polls when estimating the national term mu_a.
# One problem until early September, was that voters in polled states were different from average voters :
# Several solid red states still hadn't been polled, the weighted average of state polls was slightly more pro-Clinton than national polls.

score_among_polled <- sum(states2012[all_polled_states[-1],]$obama_count)/
  sum(states2012[all_polled_states[-1],]$obama_count + 
        states2012[all_polled_states[-1],]$romney_count)

alpha_prior <- log(states2012$national_score[1]/score_among_polled)


y <- MASS::mvrnorm(1000, mu_b_prior, Sigma = state_correlation_error)

cbind(
  inv.logit(apply(y, MARGIN = 2, mean)),
  inv.logit(apply(y, MARGIN = 2, mean) + 1.96 * apply(y, MARGIN = 2, sd)), 
  inv.logit(apply(y, MARGIN = 2, mean) - 1.96 * apply(y, MARGIN = 2, sd)))


## adjusts
adjusters <- c("ABC News/Washington Post",
  'IBD/TIPP',
  'Ipsos',
  'Pew Research Center',
  'YouGov',
  'Civiqs',
  'NBC/WSJ')
adjusters <- c(
  "ABC",
  "Washington Post",
  "IBD",
  "Ipsos",
  "Pew",
  "YouGov",
  "NBC"
)


# First stage predictions -------
N <- nrow(df)
S <- 51
s <- df$index_s
n_respondents <- df$n_respondents
n_two_parties <- df$n_clinton + df$n_trump

data_1st <- list(
  # dimensions
  N = N,
  S = S,
  # index
  s = s,
  # data
  n_respondents = n_respondents,
  n_two_parties = n_two_parties
)
# model
m_1st <- rstan::stan_model("scripts/Stan/Refactored/poll_model_1st_stage_v3.stan")
# run
out <- rstan::sampling(m_1st, data = data_1st, iter = 1000,warmup=500, chains = 2)
# extract
two_share_mu <- apply(rstan::extract(out, pars = "two_parties_mu")[[1]], MARGIN = 2, mean)
two_share_sd <- apply(rstan::extract(out, pars = "two_parties_sd")[[1]], MARGIN = 2, mean)

# Passing the data to Stan and running the model ---------
N_state <- nrow(df %>% filter(index_s != 52))
N_national <- nrow(df %>% filter(index_s == 52))
T <- T
current_T <- max(df$poll_day)
S <- 51
P <- length(unique(df$pollster))
state <- df %>% filter(index_s != 52) %>% pull(index_s)
day_national <- df %>% filter(index_s == 52) %>% pull(poll_day) 
day_state <- df %>% filter(index_s != 52) %>% pull(poll_day) 
poll_national <- df %>% filter(index_s == 52) %>% pull(index_p) 
poll_state <- df %>% filter(index_s != 52) %>% pull(index_p) 
# data ---
n_democrat_national <- df %>% filter(index_s == 52) %>% pull(n_clinton)
n_democrat_state <- df %>% filter(index_s != 52) %>% pull(n_clinton)
n_two_share_national <- df %>% filter(index_s == 52) %>% transmute(n_two_share = n_trump + n_clinton) %>% pull(n_two_share)
n_two_share_state <- df %>% filter(index_s != 52) %>% transmute(n_two_share = n_trump + n_clinton) %>% pull(n_two_share)
pred_two_share_national_mu    <- two_share_mu[52]
pred_two_share_state_mu       <- two_share_mu[1:51]
pred_two_share_national_sigma <- two_share_sd[52]
pred_two_share_state_sigma    <- two_share_sd[1:51]
unadjusted_national <- df %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_s == 52) %>% pull(unadjusted)
unadjusted_state <- df %>% mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% filter(index_s != 52) %>% pull(unadjusted)

                                   
# priors ---
prior_sigma_measure_noise <- 0.01 ### 0.1 / 2
prior_sigma_a <- 0.03 ### 0.05 / 2
prior_sigma_b <- 0.04 ### 0.05 / 2
mu_b_prior <- mu_b_prior
prior_sigma_c <- 0.02 ### 0.1 / 2
prior_sigma_e_bias <- 0.03
prior_sigma_mu_e_bias <- 0.03
mu_alpha <- alpha_prior
sigma_alpha <- 0.2  ### 0.2
prior_sigma_eta <- 0.2

# data ---
data <- list(
  N_national = N_national,
  N_state = N_state,
  T = T,
  S = S,
  P = P,
  state = state,
  day_state = as.integer(day_state),
  day_national = as.integer(day_national),
  poll_state = poll_state,
  poll_national = poll_national,
  n_democrat_national = n_democrat_national,
  n_democrat_state = n_democrat_state,
  n_two_share_national = n_two_share_national,
  n_two_share_state = n_two_share_state,
  pred_two_share_national_mu = pred_two_share_national_mu,
  pred_two_share_state_mu = pred_two_share_state_mu,
  pred_two_share_national_sigma = pred_two_share_national_sigma,
  pred_two_share_state_sigma = pred_two_share_state_sigma,
  unadjusted_national = unadjusted_national,
  unadjusted_state = unadjusted_state,
  current_T = as.integer(current_T),
  ss_correlation = state_correlation,
  ss_corr_mu_b_T = state_correlation_mu_b_T,
  ss_corr_mu_b_walk = state_correlation_mu_b_walk,
  ss_corr_error = state_correlation_error,
  prior_sigma_measure_noise = prior_sigma_measure_noise,
  prior_sigma_a = prior_sigma_a,
  prior_sigma_b = prior_sigma_b,
  mu_b_prior = mu_b_prior,
  prior_sigma_c = prior_sigma_c,
  prior_sigma_e_bias = prior_sigma_e_bias,
  prior_sigma_mu_e_bias = prior_sigma_mu_e_bias,
  mu_alpha = mu_alpha,
  sigma_alpha = sigma_alpha,
  prior_sigma_eta = prior_sigma_eta
)

### Initialization ----

n_chains <- 2

initf2 <- function(chain_id = 1) {
  # cat("chain_id =", chain_id, "\n")
  list(raw_alpha = abs(rnorm(1)), 
       raw_mu_a = rnorm(current_T),
       raw_mu_b = abs(matrix(rnorm(T * (S)), nrow = S, ncol = T)),
       raw_mu_c = abs(rnorm(P)),
       measure_noise_national = abs(rnorm(N_national)),
       measure_noise_state = abs(rnorm(N_state)),
       raw_polling_error = abs(rnorm(S)),
       sigma_measure_noise_national = abs(rnorm(1, 0, prior_sigma_measure_noise)),
       sigma_measure_noise_state = abs(rnorm(1, 0, prior_sigma_measure_noise)),
       sigma_mu_a = abs(rnorm(1, 0, prior_sigma_a)),
       sigma_mu_b = abs(rnorm(1, 0, prior_sigma_b)),
       sigma_mu_c = abs(rnorm(1, 0, prior_sigma_c))
  )
}

init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))

### Run ----

#setwd(here("scripts/Stan/Refactored/"))

# read model code
# model <- rstan::stan_model("scripts/Stan/Refactored/poll_model_v10.stan")
model <- rstan::stan_model("scripts/Stan/Adjusted_v_unadjusted/poll_model_adj_v_nadj_v2.stan")

# run model
out <- rstan::sampling(model, data = data,
                       refresh=50,
                       chains = 2, iter = 1000, warmup=500, init = init_ll
)


# save model for today
write_rds(out, sprintf('stan_model_%s.rds',RUN_DATE),compress = 'gz')

### Extract results ----
#out <- read_rds(sprintf('models/stan_model_%s.rds',RUN_DATE))

# etc
a <- rstan::extract(out, pars = "alpha")[[1]]
hist(a)
# delta
delta <- rstan::extract(out, pars = "eta")[[1]]
hist(delta)

# poll terms
poll_terms <- rstan::extract(out, pars = "mu_c")[[1]]
non_adjusters <- df %>% 
  mutate(unadjusted = ifelse(!(pollster %in% adjusters), 1, 0)) %>% 
  select(unadjusted, index_p) %>%
  distinct() %>%
  arrange(index_p)


# mu_d
e_bias <- rstan::extract(out, pars = "e_bias")[[1]]
plt_adjusted <- lapply(1:100,
       function(x){
         tibble(e_bias_draw = e_bias[x,] 
                - mean(poll_terms[x, non_adjusters[non_adjusters$unadjusted == 0, 2]$index_p])
                + mean(poll_terms[x, non_adjusters[non_adjusters$unadjusted == 1, 2]$index_p]),
                trial = x) %>%
           mutate(date = min(df$end) + row_number()) 
       }) %>%
  do.call('bind_rows',.) %>%
  ggplot(.,aes(x=date,y=e_bias_draw,group=trial)) +
  geom_line(alpha=0.2)
plt_unadjusted <- lapply(1:100,
       function(x){
         tibble(e_bias_draw = e_bias[x,],
                trial = x) %>%
           mutate(date = min(df$end) + row_number()) 
       }) %>%
  do.call('bind_rows',.) %>%
  ggplot(.,aes(x=date,y=e_bias_draw,group=trial)) +
  geom_line(alpha=0.2)
grid.arrange(plt_adjusted, plt_unadjusted)

# extract predictions
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

p_clinton <- pblapply(1:dim(predicted_score)[3],
                       function(x){
                         # pred is mu_a + mu_b for the past, just mu_b for the future
                         p_clinton <- predicted_score[,,x]
                         
                         # put in tibble
                         tibble(low = apply(p_clinton,2,function(x){(quantile(x,0.05))}),
                                high = apply(p_clinton,2,function(x){(quantile(x,0.95))}),
                                mean = apply(p_clinton,2,function(x){(mean(x))}),
                                prob = apply(p_clinton,2,function(x){(mean(x>0.5))}),
                                state = x) 
                         
                       }) %>% do.call('bind_rows',.)

p_clinton$state = colnames(state_correlation)[p_clinton$state]

p_clinton <- p_clinton %>%
  group_by(state) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ungroup()

ex_states <- c('IA','FL','OH','WI','MI','PA','AZ','NC','NH')

# together
# national vote = vote * state weights
p_clinton <- p_clinton %>%
  bind_rows(
    p_clinton %>% 
    left_join(enframe(state_weights,'state','weight')) %>%
    group_by(t) %>%
    summarise(mean = weighted.mean(mean,weight),
              high = weighted.mean(high,weight),
              low = weighted.mean(low,weight)) %>%
    mutate(state='--')
  )

# look
p_clinton %>% filter(t == max(t),state %in% ex_states) %>% mutate(se = (high - mean)/2)


# electoral college by simulation
draws <- pblapply(1:dim(predicted_score)[3],
             function(x){
               # pred is mu_a + mu_b for the past, just mu_b for the future
               p_clinton <- predicted_score[,,x]
               
               p_clinton <- p_clinton %>%
                 as.data.frame() %>%
                 mutate(draw = row_number()) %>%
                 gather(t,p_clinton,1:(ncol(.)-1)) %>%
                 mutate(t = as.numeric(gsub('V','',t)) + min(df$begin),
                        state = colnames(state_correlation)[x]) 

           
         }) %>% do.call('bind_rows',.)


sim_evs <- draws %>%
  left_join(states2012 %>% select(state,ev),by='state') %>%
  group_by(t,draw) %>%
  summarise(dem_ev = sum(ev * (p_clinton > 0.5))) %>%
  group_by(t) %>%
  summarise(mean_dem_ev = mean(dem_ev),
            high_dem_ev = quantile(dem_ev,0.975),
            low_dem_ev = quantile(dem_ev,0.025),
            prob = mean(dem_ev >= 270))

# draws %>%
#   left_join(states2012 %>% select(state,ev),by='state') %>%
#   group_by(t,draw) %>%
#   filter(draw<500) %>%
#   summarise(dem_ev = sum(ev * (p_clinton > 0.5))) %>%
#   group_by(t) %>%
#   spread(draw,dem_ev) %>%
#   write_csv(.,'~/Desktop/500_sims_per_day.csv')

# add identifier
identifier <- paste0(Sys.Date()," || " , out@model_name)

natl_polls.gg <- p_clinton %>%
  filter(state == '--') %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = national_mu_prior,linetype=2) +
  geom_point(aes(y=p_clinton),alpha=0.5) +
  #geom_smooth(aes(y=p_clinton),method='loess',span=0.2,col='black',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  labs(subtitle='p_clinton national')

natl_evs.gg <-  ggplot(sim_evs, aes(x=t)) +
  geom_hline(yintercept = 270) +
  geom_line(aes(y=mean_dem_ev)) +
  geom_ribbon(aes(ymin=low_dem_ev,ymax=high_dem_ev),alpha=0.2) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  labs(subtitletitle='clinton evs')

state_polls.gg <- p_clinton %>%
  filter(state %in% ex_states) %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t,col=state)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_point(aes(y=p_clinton),alpha=0.5) +
  #geom_smooth(aes(y=p_clinton,col=state),method='loess',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  labs(subtitle='p_clinton state')

grid.arrange(natl_polls.gg, natl_evs.gg, state_polls.gg, 
             layout_matrix = rbind(c(1,1,3,3,3),
                                   c(2,2,3,3,3)),
             top = identifier
)


# probs v other forecasters
ggplot(sim_evs, aes(x=t)) +
  geom_hline(yintercept = 0.5) +
  geom_line(aes(y=prob))  +
  coord_cartesian(ylim=c(0,1)) +
  geom_hline(data=tibble(forecaster = c('nyt',
                                        'fivethirtyeight',
                                        'huffpost',
                                        'predictwise',
                                        'pec',
                                        'dailykos',
                                        'morris16'),
                         prob = c(0.85,0.71,0.98,0.89,0.99,0.92,0.84)),
             aes(yintercept=prob,col=forecaster),linetype=2) +
  labs(subtitle = identifier)


# now-cast probability over time all states
p_clinton %>%
  #filter(abs(mean-0.5)<0.2) %>%
  # plot
  ggplot(.,aes(x=t,y=prob,col=state)) +
  geom_hline(yintercept=0.5) +
  geom_line() +
  geom_label_repel(data = p_clinton %>% 
                     filter(t==max(t),
                            prob > 0.1 & prob < 0.9),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(subtitle = identifier)

# diff from national over time?
p_clinton[p_clinton$state != '--',] %>%
  left_join(p_clinton[p_clinton$state=='--',] %>%
              select(t,p_clinton_national=mean), by='t') %>%
  mutate(diff=mean-p_clinton_national) %>%
  group_by(state) %>%
  mutate(last_prob = last(prob)) %>%
  filter(state %in% ex_states) %>%
  ggplot(.,aes(x=t,y=diff,col=state)) +
  geom_hline(yintercept=0.0) +
  geom_line() +
  geom_label_repel(data = . %>% 
                     filter(t==max(t),
                            prob > 0.1 & prob < 0.9),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(-1,1,0.01)) +
  labs(subtitle = identifier)


# final EV distribution
final_evs <- draws %>%
  left_join(states2012 %>% select(state,ev),by='state') %>%
  filter(t==max(t)) %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev* (p_clinton > 0.5)))

ggplot(final_evs,aes(x=dem_ev,
                     fill=ifelse(dem_ev>=270,'Democratic','Republican'))) +
  geom_vline(xintercept = 270) +
  geom_histogram(binwidth=1) +
  theme_minimal() +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank()) +
  scale_fill_manual(name='Electoral College winner',values=c('Democratic'='#3A4EB1','Republican'='#E40A04')) +
  labs(x='Democratic electoral college votes',
       subtitle=sprintf("p(dem win) = %s",mean(final_evs$dem_ev>=270)))

# brier scores
# https://www.buzzfeednews.com/article/jsvine/2016-election-forecast-grades
compare <- p_clinton %>% 
  filter(t==max(t),state!='--') %>% 
  select(state,clinton_win=prob) %>% 
  mutate(clinton_win_actual = ifelse(state %in% c('CA','NV','OR','WA','CO','NM','MN','IL','VA','DC','MD','DE','NJ','CT','RI','MA','NH','VT','NY','HI','ME'),1,0),
         diff = (clinton_win_actual - clinton_win )^2) %>% left_join(enframe(ev_state) %>% set_names(.,c('state','ev'))) %>% 
  mutate(ev_weight = ev/(sum(ev))) 


tibble(outlet = c('538 polls-plus','538 polls-only','princeton','nyt upshot','kremp/slate','pollsavvy','predictwise markets','predictwise overall','desart and holbrook','daily kos','huffpost'),
       ev_wtd_brier = c(0.0928,0.0936,0.1169,0.1208,0.121,0.1219,0.1272,0.1276,0.1279,0.1439,0.1505),
       unwtd_brier = c(0.0664,0.0672,0.0744,0.0801,0.0766,0.0794,0.0767,0.0783,0.0825,0.0864,0.0892),
       states_correct = c(46,46,47,46,46,46,46,46,44,46,46)) %>% 
  bind_rows(tibble(outlet='economist (backtest)',
                   ev_wtd_brier = weighted.mean(compare$diff, compare$ev_weight),
                   unwtd_brier = mean(compare$diff),
                   states_correct=sum(round(compare$clinton_win) == round(compare$clinton_win_actual)))) %>%
  arrange(ev_wtd_brier) 


