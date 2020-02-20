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
}
  
## Master variables
RUN_DATE <- min(ymd('2016-11-08'),Sys.Date())

election_day <- ymd("2016-11-08")
start_date <- as.Date("2016-03-01") # Keeping all polls after March 1, 2016


# wrangle polls -----------------------------------------------------------
# read
setwd(here("data/"))
all_polls <- read.csv("all_polls.csv", stringsAsFactors = FALSE, header = TRUE)


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

here("data")
polls_2012 <- read.csv("potus_results_76_16.csv")
polls_2012 <- polls_2012 %>% 
  filter(state != "DC") %>%
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
  mutate(index_s = ifelse(index_s == 1, 51, index_s - 1))

# Useful vectors ---------
all_polled_states <- df$state %>% unique %>% sort
ndays <- max(df$t) - min(df$t)
all_t <- min(df$t) + days(0:(ndays))
all_t_until_election <- min(all_t) + days(0:(election_day - min(all_t)))

all_pollsters <- levels(as.factor(as.character(df$pollster)))


# Reading 2012 election data to --------- 
# (1) set priors on mu_b and alpha,
# (2) get state_weights,           
# (3) get state_names and EV        
setwd(here("data/"))
states2012 <- read.csv("2012.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
    mutate(score = obama_count / (obama_count + romney_count),
           national_score = sum(obama_count)/sum(obama_count + romney_count),
           delta = score - national_score,
           share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
           /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
    arrange(state) %>% filter(state != "DC")

rownames(states2012) <- states2012$state

prior_diff_score <- states2012[all_polled_states[-1],]$delta
names(prior_diff_score) <- all_polled_states[-1]

state_weights <- c(states2012[all_polled_states[-1],]$share_national_vote / sum(states2012[all_polled_states[-1],]$share_national_vote))
names(state_weights) <- c(states2012[all_polled_states[-1],]$state)

all_states <- states2012$state
state_name <- states2012$state_name
names(state_name) <- states2012$state

# electoral votes, by state:
ev_state <- states2012$ev
names(ev_state) <- states2012$state

# unique date indices for days in which national polls were conducted ---------
# See the Stan model: trying to speed up computation of the likelihood by calculating
# the weighted mean of state latent vote intentions only for days national polls were conducted.

#unique_ts <- unique(df$index_t[df$state == "--"])

#natpolls_indices <- data.frame(unique_ts = unique_ts)

#df$index_t_unique <- sapply(1:nrow(df), 
#                            function(i) ifelse(df$state[i] == "--", 
#                                               which(natpolls_indices$unique_ts == df$index_t[i]), 
#                                               0))

# Creating priors ---------

# Mean of the mu_b_prior
# 0.486 is the predicted Clinton share of the national vote according to the Time for Change model.
mu_b_prior <- logit(0.511 + c(prior_diff_score))

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
S <- 51
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

setwd(here("scripts/Stan/Refactored/"))

model <- rstan::stan_model("poll_model_v2.stan")

out <- rstan::sampling(model, data = data,
            refresh=10,
            chains = 3, iter = 1000,warmup=500, init = init_ll
            )


