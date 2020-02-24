## Desc
# Refactored version run file

## Setup
rm(list = ls())
options(mc.cores = parallel::detectCores())

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
  library(glmnet, quietly = TRUE)
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
state_correlation <- lqmm::make.positive.definite(state_correlation)  

# Numerical indices passed to Stan for states, days, weeks, pollsters
df <- df %>% 
  mutate(poll_day = t - min(t) + 1,
         # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
         index_s = as.numeric(as.factor(as.character(state))), 
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


# Creating priors ---------
# read in abramowitz data
#setwd(here("data/"))
abramowitz <- read.csv('data/abramowitz_data.csv') %>% filter(year != 2016)

# train a caret model to predict demvote with incvote ~ q2gdp + juneapp + year:q2gdp + year:juneapp 
prior_model <- caret::train(
  incvote ~ q2gdp + juneapp + year:q2gdp + year:juneapp,
  data = abramowitz,
  method = "glmnet",
  trControl = trainControl(
    # 3-fold CV 
    method = "cv", 
    number = 5),
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
mu_b_prior <- logit(national_mu_prior + c("--" = 0, prior_diff_score))
mu_b_prior <- logit(national_mu_prior + prior_diff_score)

# The model uses national polls to complement state polls when estimating the national term mu_a.
# One problem until early September, was that voters in polled states were different from average voters :
# Several solid red states still hadn't been polled, the weighted average of state polls was slightly more pro-Clinton than national polls.

score_among_polled <- sum(states2012[all_polled_states[-1],]$obama_count)/
  sum(states2012[all_polled_states[-1],]$obama_count + 
        states2012[all_polled_states[-1],]$romney_count)

alpha_prior <- log(states2012$national_score[1]/score_among_polled)

# Passing the data to Stan and running the model ---------
N <- nrow(df)
T <- T
S <- 52 # 51 + DC
P <- length(unique(df$pollster))
state <- df$index_s
day <- df$poll_day
poll <- df$index_p
state_weights <- state_weights

n_democrat <- df$n_clinton
n_respondents <- df$n_respondents

current_T <- max(df$poll_day)
ss_correlation <- state_correlation

prior_sigma_measure_noise <- 0.1
prior_sigma_a <- 0.1
prior_sigma_b <- 0.1
mu_b_prior <- mu_b_prior
prior_sigma_mu_c <- 0.1
mu_alpha <- alpha_prior
sigma_alpha <- 0.2
prior_sigma_mu_c <- 0.1


data <- list(
  N = N,
  T = T,
  S = S,
  P = P,
  state = state,
  day = as.integer(day),
  poll = poll,
  state_weights = state_weights,
  n_democrat = n_democrat,
  n_respondents = n_respondents,
  current_T = as.integer(current_T),
  ss_correlation = state_correlation,
  prior_sigma_measure_noise = prior_sigma_measure_noise,
  prior_sigma_a = prior_sigma_a,
  prior_sigma_b = prior_sigma_b,
  mu_b_prior = mu_b_prior,
  prior_sigma_mu_c = prior_sigma_mu_c,
  mu_alpha = mu_alpha,
  sigma_alpha = sigma_alpha
)

### Initialization ----

n_chains <- 3

initf2 <- function(chain_id = 1) {
  # cat("chain_id =", chain_id, "\n")
  list(raw_alpha = abs(rnorm(1)), 
       raw_mu_a = rnorm(current_T),
       raw_mu_b = abs(matrix(rnorm(T * (S - 1)), nrow = T, ncol = (S - 1))),
       raw_mu_c = abs(rnorm(P)),
       measure_noise = abs(rnorm(N)),
       raw_polling_error = abs(rnorm(S - 1)),
       sigma_measure_noise_national = abs(rnorm(1, 0, prior_sigma_measure_noise / 2)),
       sigma_measure_noise_state = abs(rnorm(1, 0, prior_sigma_measure_noise / 2)),
       sigma_mu_c = abs(rnorm(1, 0, prior_sigma_mu_c / 2)),
       sigma_mu_a = abs(rnorm(1, 0, prior_sigma_a / 2)),
       sigma_mu_b = abs(rnorm(1, 0, prior_sigma_b /2))
  )
}

init_ll <- lapply(1:n_chains, function(id) initf2(chain_id = id))

### Run ----

#setwd(here("scripts/Stan/Refactored/"))

# read model code
model <- rstan::stan_model("scripts/Stan/Refactored/poll_model_v3.stan")

# run model
out <- rstan::sampling(model, data = data,
                       refresh=10,
                       chains = 3, iter = 1000,warmup=500, init = init_ll
)

# save model for today
write_rds(out, sprintf('models/stan_model_%s.rds',RUN_DATE),compress = 'gz')

### Extract results ----
# national vote
mu_a <- rstan::extract(out, pars = "mu_a")[[1]]

# state deviations
mu_b <- rstan::extract(out, pars = "mu_b")[[1]]

# extend mu_a to full length of mu_b
mu_a <- cbind(mu_a,
      matrix(mu_a[,ncol(mu_a)], 
             nrow = nrow(mu_a), 
             ncol = (ncol(mu_b)-ncol(mu_a)) 
             ) )


# prediction in any state is mu_b + mu_a
draws <- lapply(1:dim(mu_b)[3],
      function(x){
        # get state mu_b
        state_mu_b_draws <- mu_b[,,x]
        
        # add national mu_a and convert to inv.logit
        mu_b_plus_a <- inv.logit(state_mu_b_draws + mu_a)
        
        # inv logit
        mu_b_plus_a %>%
          as.data.frame() %>%
          mutate(state = x,
                 draw = row_number()) %>%
          gather(date,p_clinton,(1:(ncol(.)-2))) %>%
          mutate(date = as.numeric(gsub('V','',date)),
                 date = min(df$t) + date) %>%
          return()
        
      })  %>%
  do.call('bind_rows',.)

# get mean and CI
predictions <- draws %>%
  group_by(state,date) %>%
  summarise(p_clinton_mean = mean(p_clinton),
         p_clinton_high = quantile(p_clinton,0.975),
         p_clinton_low = quantile(p_clinton,0.025)) %>%
  ungroup()

# state indices (not sure I'm getting this right?)
#predictions$state = names(mu_b_prior)[predictions$state]
state_lookup <- tibble(state = unique(df$state),
                       index_s = unique(df$index_s))

predictions$state <- state_lookup[match(predictions$state,state_lookup$index_s),]$state

# plot just means in each state
predictions %>%
  ggplot(.,aes(x=date,col=state,group=state)) +
  geom_hline(yintercept = 0.5) +
  geom_line(aes(y=p_clinton_mean)) +
  scale_x_date(date_breaks = '1 month',date_labels = '%b',limits = c(ymd('2016-03-01'),ymd('2016-11-08'))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

# plot means and CIs in swing states
predictions %>%
  filter(state %in% c('MI','PA','WI','FL','NC','OH')) %>%
  ggplot(.,aes(x=date,col=state,group=state)) +
  geom_hline(yintercept = 0.5) +
  geom_line(aes(y=p_clinton_mean)) +
  geom_ribbon(aes(ymin=p_clinton_low,ymax=p_clinton_high,fill=state),col=NA,alpha=0.2) +
  scale_x_date(date_breaks = '1 month',date_labels = '%b',limits = c(ymd('2016-03-01'),ymd('2016-11-08'))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~state)
