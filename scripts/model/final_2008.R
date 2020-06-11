## Desc
# Refactored version run file

## Setup
#rm(list = ls())
options(mc.cores = 4)

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
  library(gridExtra, quietly = TRUE)
  library(pbapply, quietly = TRUE)
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
RUN_DATE <- ymd("2008-11-03")

election_day <- ymd("2008-11-03")
start_date <- as.Date("2008-03-01") # Keeping all polls after March 1, 2008


# wrangle polls -----------------------------------------------------------
# read
#setwd(here("data/"))
all_polls <- read_csv('data/all_polls_2008.csv')

# select relevant columns from HufFPost polls
all_polls <- all_polls %>%
  dplyr::select(state, pollster, number.of.observations, mode,population,
                start.date, 
                end.date,
                obama, mccain, undecided, other)%>%
  mutate(end.date = mdy(end.date),
         start.date = mdy(start.date))


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
         & n > 1) 

# pollster mutations
df <- df %>%
  mutate(pollster = str_extract(pollster, pattern = "[A-z0-9 ]+") %>% sub("\\s+$", "", .),
         pollster = replace(pollster, pollster == "Fox News", "FOX"), # Fixing inconsistencies in pollster names
         pollster = replace(pollster, pollster == "WashPost", "Washington Post"),
         pollster = replace(pollster, pollster == "ABC News", "ABC"),
         undecided = ifelse(is.na(undecided), 0, undecided),
         other = ifelse(is.na(other), 0, other))

# vote shares etc
df <- df %>%
  mutate(two_party_sum = obama + mccain,
         polltype = as.integer(as.character(recode(population, 
                                                   "Likely Voters" = "0", 
                                                   "Registered Voters" = "1",
                                                   "Adults" = "2"))), 
         n_respondents = round(n),
         # obama
         n_obama = round(n * obama/100),
         p_obama = obama/two_party_sum,
         # mccain
         n_mccain = round(n * mccain/100),
         p_mccain = mccain/two_party_sum,
         # third-party
         n_other = round(n * other/100),
         p_other = other/100)


# prepare stan date -----------------------------------------------------------

# create correlation matrix ---------------------------------------------

## --- create correlation matrix
state_data <- read.csv("data/potus_results_76_16.csv")
state_data <- state_data %>% 
  select(year, state, dem) %>%
  group_by(state) %>%
  mutate(dem = dem ) %>% #mutate(dem = dem - lag(dem)) %>%
  select(state,variable=year,value=dem)  %>%
  ungroup() %>%
  na.omit() %>%
  filter(variable == 2016)

census <- read.csv('data/acs_2013_variables.csv')
census <- census %>%
  filter(!is.na(state)) %>% 
  select(-c(state_fips,pop_total,pop_density)) %>%
  group_by(state) %>%
  gather(variable,value,
         1:(ncol(.)-1))

state_data <- state_data %>%
  mutate(variable = as.character(variable)) %>%
  bind_rows(census)

# add urbanicity
urbanicity <- read.csv('data/urbanicity_index.csv') %>%
  dplyr::select(state,pop_density = average_log_pop_within_5_miles) %>%
  gather(variable,value,
         2:(ncol(.)))

state_data <- state_data %>%
  bind_rows(urbanicity)

# add pct white evangelical
white_evangel_pct <- read_csv('data/white_evangel_pct.csv') %>%
  gather(variable,value,
         2:(ncol(.)))

state_data <- state_data %>%
  bind_rows(white_evangel_pct)

# add region, as a dummy for each region
regions <- read_csv('data/state_region_crosswalk.csv') %>%
  select(state = state_abb, variable=region) %>%
  mutate(value = 1) %>%
  spread(variable,value)

regions[is.na(regions)] <- 0

regions <- regions %>%
  gather(variable,value,2:ncol(.))

#state_data <- state_data %>%
#  bind_rows(regions)

# scale and spread
state_cor <- state_data %>%
  group_by(variable) %>%
  # scale all varaibles
  mutate(value = (value - min(value, na.rm=T)) / 
           (max(value, na.rm=T) - min(value, na.rm=T))) %>%
  #mutate(value = (value - mean(value)) / sd(value)) %>%
  # now spread
  spread(state, value) %>% 
  na.omit() %>%
  ungroup() %>%
  select(-variable)

# test
ggplot(state_cor,aes(x=NV, y=FL)) + geom_point() + geom_smooth(method='lm')

state_cor %>% 
  dplyr::select(NV,FL,WI,MI,NH,OH,IA,NC,IN,TX,AZ) %>%  #AL,CA,FL,MN,NC,NM,RI,WI
  cor

# make matrices
state_correlation <- cor(state_cor)  
state_correlation[state_correlation < 0.3] <- 0.3 # baseline cor from national error
state_correlation <- make.positive.definite(state_correlation)

# function to find covariance coefficient for a gien standard deviation
find_sigma2_value <- function(empirical_sd){
  gen_residual <- function(par, target_sd){
    y <- MASS::mvrnorm(100000, rep(0.5,10), Sigma = cov_matrix(10, par^2, 1) ) 
    error <- mean( inv.logit(apply(y, MARGIN = 2, mean) +  apply(y, MARGIN = 2, sd)) - inv.logit(apply(y, MARGIN = 2, mean)) ) - target_sd
    return(abs(error))
  }
  optimize(f = gen_residual, interval = c(0.00001,5),target_sd = empirical_sd,tol = 0.00001)
}

# checking the amounts of error in the correlation matrices
y <- MASS::mvrnorm(100000, rep(0.5,10), Sigma = cov_matrix(10, find_sigma2_value(empirical_sd = 0.05)$minimum^2, 1) ) 
mean( inv.logit(apply(y, MARGIN = 2, mean) +  apply(y, MARGIN = 2, sd)) - inv.logit(apply(y, MARGIN = 2, mean)) ) 

# covariance for polling error
state_correlation_error <- cov_matrix(51, find_sigma2_value(empirical_sd = 0.025)$minimum^2, 0.9) # 3.4% on elec day
state_correlation_error <- state_correlation_error * state_correlation

# covariance for prior e-day prediction
target_se = read_csv("data/state_priors_08_12_16.csv") %>%
  filter(date <= RUN_DATE) %>%
  group_by(state) %>%
  arrange(date) %>%
  filter(date == max(date)) %>%
  pull(se)

state_correlation_mu_b_T <- cov_matrix(n = 51, sigma2 = find_sigma2_value(empirical_sd = median(target_se))$minimum^2, rho = 0.9) # 6% on elec day
state_correlation_mu_b_T <- state_correlation_mu_b_T * state_correlation

new_diag <- pbsapply(target_se, cl=parallel::detectCores()-1, function(x){find_sigma2_value(empirical_sd = x)$minimum})^2
diag(state_correlation_mu_b_T) <- ifelse(new_diag > diag(state_correlation_mu_b_T), new_diag, diag(state_correlation_mu_b_T))

# covariance matrix for random walks
state_correlation_mu_b_walk <- cov_matrix(51, (0.01)^2, 0.9) 
state_correlation_mu_b_walk <- state_correlation_mu_b_walk * state_correlation


# final poll wrangling ----
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
    p_obama, n_obama, 
    p_mccain, n_mccain, 
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
states2008 <- read.csv("data/2008.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(score = obama_count / (obama_count + mccain_count),
         national_score = sum(obama_count)/sum(obama_count + mccain_count),
         delta = score - national_score,
         share_national_vote = (total_count)
         /sum(total_count)) %>%
  arrange(state) 

rownames(states2008) <- states2008$state

# get state incdices
all_states <- states2008$state
state_name <- states2008$state_name
names(state_name) <- states2008$state

# set prior differences
prior_diff_score <- states2008$delta
names(prior_diff_score) <- all_states

# set state weights
state_weights <- c(states2008$share_national_vote / sum(states2008$share_national_vote))
names(state_weights) <- c(states2008$state)

# electoral votes, by state:
ev_state <- states2008$ev
names(ev_state) <- states2008$state


##### Creating priors --------------
# read in abramowitz data
#setwd(here("data/"))
abramowitz <- read.csv('data/abramowitz_data.csv') %>% 
  filter(year < 2008)

# train a caret model to predict demvote with incvote ~ q2gdp + juneapp + year:q2gdp + year:juneapp 
prior_model <- lm(
  incvote ~  juneapp + q2gdp, #+ year:q2gdp + year:juneapp
  data = abramowitz
)

# make predictions
national_mu_prior <- 100 - predict(prior_model,newdata = tibble(q2gdp = 0.6,
                                                              juneapp = -40))


# on correct scale
national_mu_prior <- national_mu_prior / 100

# Mean of the mu_b_prior
# 0.486 is the predicted obama share of the national vote according to the Lewis-Beck & Tien model
# https://pollyvote.com/en/components/econometric-models/lewis-beck-tien/
mu_b_prior <- logit(national_mu_prior + prior_diff_score)

# or read in priors if generated already
prior_in <- read_csv("data/state_priors_08_12_16.csv") %>%
  filter(date <= RUN_DATE) %>%
  group_by(state) %>%
  arrange(date) %>%
  filter(date == max(date)) %>%
  select(state,pred) %>%
  ungroup() %>%
  arrange(state)

mu_b_prior <- logit(prior_in$pred)
names(mu_b_prior) <- prior_in$state
names(mu_b_prior) == names(prior_diff_score) # correct order?

national_mu_prior <- weighted.mean(inv.logit(mu_b_prior), state_weights)

cat(sprintf('Prior Obama two-party vote is %s\nWith a standard error of %s',
            round(national_mu_prior,3),round(median(target_se),3)))

# The model uses national polls to complement state polls when estimating the national term mu_a.
# One problem until early September, was that voters in polled states were different from average voters :
# Several solid red states still hadn't been polled, the weighted average of state polls was slightly more pro-obama than national polls.

score_among_polled <- sum(states2008[all_polled_states[-1],]$obama_count)/
  sum(states2008[all_polled_states[-1],]$obama_count + 
        states2008[all_polled_states[-1],]$mccain_count)

alpha_prior <- log(states2008$national_score[1]/score_among_polled)


## adjusts
adjusters <- c(
  "ABC",
  "Washington Post",
  "Ipsos",
  "Pew",
  "YouGov",
  "NBC"
)


# Passing the data to Stan and running the model ---------
N_state <- nrow(df %>% filter(index_s != 52))
N_national <- nrow(df %>% filter(index_s == 52))
T <- as.integer(round(difftime(election_day, min(df$begin))))
current_T <- max(df$poll_day)
S <- 51
P <- length(unique(df$pollster))
state <- df %>% filter(index_s != 52) %>% pull(index_s)
day_national <- df %>% filter(index_s == 52) %>% pull(poll_day) 
day_state <- df %>% filter(index_s != 52) %>% pull(poll_day) 
poll_national <- df %>% filter(index_s == 52) %>% pull(index_p) 
poll_state <- df %>% filter(index_s != 52) %>% pull(index_p) 
# data ---
n_democrat_national <- df %>% filter(index_s == 52) %>% pull(n_obama)
n_democrat_state <- df %>% filter(index_s != 52) %>% pull(n_obama)
n_two_share_national <- df %>% filter(index_s == 52) %>% transmute(n_two_share = n_mccain + n_obama) %>% pull(n_two_share)
n_two_share_state <- df %>% filter(index_s != 52) %>% transmute(n_two_share = n_mccain + n_obama) %>% pull(n_two_share)
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

n_chains <- 4

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
#model <- rstan::stan_model("scripts/model/poll_model_2020_no_partisan_correction.stan")
model <- rstan::stan_model("scripts/model/poll_model_2020_no_mode_adjustment.stan")
#model <- rstan::stan_model("scripts/model/poll_model_2020.stan")

# run model
out <- rstan::sampling(model, data = data,
                       refresh=50,
                       chains = 4, iter = 1000, warmup=500, init = init_ll
)


# save model for today
write_rds(out, sprintf('models/backtest_2008/stan_model_%s.rds',RUN_DATE),compress = 'gz')

### Extract results ----
# out  <- read_rds(sprintf('models/backtest_2008/stan_model_%s.rds',RUN_DATE))

# etc
a <- rstan::extract(out, pars = "alpha")[[1]]
hist(a)
# sigmas
tibble(sigma_national = rstan::extract(out, pars = "sigma_a")[[1]],
       sigma_state = rstan::extract(out, pars = "sigma_b")[[1]]) %>%
  gather(parameter,value) %>%
  ggplot(.,aes(x=value)) +
  geom_histogram(binwidth=0.001) +
  facet_grid(rows=vars(parameter))
# measurement noise
measure_noise_national <- rstan::extract(out, pars = "measure_noise_national")[[1]] * mean(rstan::extract(out, pars = "sigma_measure_noise_national")[[1]])
hist(measure_noise_national)
# muc
## mu_c
mu_c_posterior_draws <- rstan::extract(out, pars = "mu_c")[[1]] 
mu_c_posterior_draws <- data.frame(draws = as.vector(mu_c_posterior_draws),
                                   index_p = sort(rep(seq(1, P), dim(mu_c_posterior_draws)[1])), 
                                   type = "posterior")
mu_c_prior_draws <- data.frame(draws = rnorm(P * 1000, 0, prior_sigma_c),
                               index_p = sort(rep(seq(1, P), 1000)), 
                               type = "prior")
mu_c_draws <- rbind(mu_c_posterior_draws, mu_c_prior_draws) 
pollster <- df %>% select(pollster, index_p) %>% distinct()
mu_c_draws <- merge(mu_c_draws, pollster, by = "index_p", all.x = TRUE)
mu_c_draws <- mu_c_draws %>%
  group_by(pollster, type) %>%
  summarize(mean = mean(draws), 
            low = mean(draws) - 1.96 * sd(draws),
            high = mean(draws) + 1.96 * sd(draws))
mu_c_plt <- mu_c_draws %>% 
  arrange(mean) %>% 
  filter(pollster %in% (df %>% group_by(pollster) %>% 
                          summarise(n=n()) %>% filter(n>=5) %>% pull(pollster))) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(pollster, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = pollster, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
#write_csv(mu_c_draws,'output/mu_c_draws_2008.csv')
# look at variation in mu_a
mu_a <- rstan::extract(out, pars = "mu_a")[[1]]
lapply(1:100,
       function(x){
         tibble(mu_a_draw = inv.logit(mu_a[x,]),
                trial = x) %>%
           mutate(date = min(df$end) + row_number()) 
       }) %>%
  do.call('bind_rows',.) %>%
  ggplot(.,aes(x=date,y=mu_a_draw,group=trial)) +
  geom_line(alpha=0.2)
# extract predictions
predicted_score <- rstan::extract(out, pars = "predicted_score")[[1]]

# states
p_obama <- pblapply(1:dim(predicted_score)[3],
                    function(x){
                      # pred is mu_a + mu_b for the past, just mu_b for the future
                      temp <- predicted_score[,,x]
                      
                      # put in tibble
                      tibble(low = apply(temp,2,function(x){(quantile(x,0.05))}),
                             high = apply(temp,2,function(x){(quantile(x,0.95))}),
                             mean = apply(temp,2,function(x){(mean(x))}),
                             prob = apply(temp,2,function(x){(mean(x>0.5))}),
                             state = x) 
                      
                    }) %>% do.call('bind_rows',.)

p_obama$state = colnames(state_correlation)[p_obama$state]

p_obama <- p_obama %>%
  group_by(state) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ungroup()

# national
p_obama_natl <- pblapply(1:dim(predicted_score)[2],
                         function(x){
                           # each row is a day for a particular draw
                           temp <- predicted_score[x,,] %>% as.data.frame()
                           names(temp) <- colnames(state_correlation)
                           
                           # for each row, get weigted natl vote
                           tibble(natl_vote = apply(temp,MARGIN = 1,function(y){weighted.mean(y,state_weights)})) %>%
                             mutate(t = row_number() + min(df$begin)) %>%
                             mutate(draw = x)
                         }) %>% do.call('bind_rows',.)

p_obama_natl <- p_obama_natl %>%
  group_by(t) %>%
  summarise(low = quantile(natl_vote,0.05),
            high = quantile(natl_vote,0.95),
            mean = mean(natl_vote),
            prob = mean(natl_vote > 0.5)) %>%
  mutate(state = '--')

# bind state and national vote
p_obama <- p_obama %>%
  bind_rows(p_obama_natl) %>%
  arrange(desc(mean))

# look
ex_states <- c('IA','FL','OH','WI','MI','PA','AZ','NC','NH','TX','GA','MN')
p_obama %>% filter(t == RUN_DATE,state %in% c(ex_states,'--')) %>% mutate(se = (high - mean)/1.68) %>% dplyr::select(-t)

urbnmapr::states %>%
  left_join(p_obama %>% filter(t == max(t)) %>%
              select(state_abbv=state,prob)) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=prob)) +
  geom_polygon()  + 
  coord_map("albers",lat0=39, lat1=45) +
  scale_fill_gradient2(high='blue',low='red',mid='white',midpoint=0.5) +
  theme_void()


# electoral college by simulation
draws <- pblapply(1:dim(predicted_score)[3],
                  function(x){
                    # pred is mu_a + mu_b for the past, just mu_b for the future
                    p_obama <- predicted_score[,,x]
                    
                    p_obama <- p_obama %>%
                      as.data.frame() %>%
                      mutate(draw = row_number()) %>%
                      gather(t,p_obama,1:(ncol(.)-1)) %>%
                      mutate(t = as.numeric(gsub('V','',t)) + min(df$begin),
                             state = colnames(state_correlation)[x]) 
                    
                    
                  }) %>% do.call('bind_rows',.)


sim_evs <- draws %>%
  left_join(states2008 %>% select(state,ev),by='state') %>%
  group_by(t,draw) %>%
  summarise(dem_ev = sum(ev * (p_obama > 0.5))) %>%
  group_by(t) %>%
  summarise(mean_dem_ev = mean(dem_ev),
            median_dem_ev = median(dem_ev),
            high_dem_ev = quantile(dem_ev,0.975),
            low_dem_ev = quantile(dem_ev,0.025),
            prob = mean(dem_ev >= 270)) %>%
  left_join(p_obama[p_obama$state != '--',] %>%
              left_join(states2008 %>% select(state,ev),by='state') %>%
              group_by(t) %>%
              summarise(sum_dem_ev = sum(ev * (prob > 0.5))) )


# add identifier
identifier <- paste0(Sys.Date()," || " , out@model_name)

natl_polls.gg <- p_obama %>%
  filter(state == '--') %>%
  left_join(df %>% select(state,t,p_obama)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = national_mu_prior,linetype=2) +
  geom_point(aes(y=p_obama),alpha=0.2) +
  geom_smooth(aes(y=p_obama),method='loess',span=0.2,col='black',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2008-03-01','2008-11-03')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.02)) +
  labs(subtitle='p_obama national')

natl_evs.gg <-  ggplot(sim_evs, aes(x=t)) +
  geom_hline(yintercept = 270) +
  geom_line(aes(y=mean_dem_ev)) +
  geom_line(aes(y=median_dem_ev),linetype=2) +
  geom_ribbon(aes(ymin=low_dem_ev,ymax=high_dem_ev),alpha=0.2) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2008-03-01','2008-11-03')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,600,100)) +
  labs(subtitletitle='obama evs')

state_polls.gg <- p_obama %>%
  filter(state %in% ex_states) %>%
  left_join(df %>% select(state,t,p_obama)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t,col=state)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_point(aes(y=p_obama),alpha=0.2) +
  #geom_smooth(aes(y=p_obama,col=state),method='loess',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2008-03-01','2008-11-03')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.05)) +
  labs(subtitle='p_obama state')

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
  geom_hline(data=tibble(forecaster = c('pec','fivethirtyeight'),
                         prob = c(0.99,0.909)),
             aes(yintercept=prob,col=forecaster),linetype=2) +
  labs(subtitle = identifier)


# now-cast probability over time all states
p_obama %>%
  #filter(abs(mean-0.5)<0.2) %>%
  # plot
  ggplot(.,aes(x=t,y=prob,col=state)) +
  geom_hline(yintercept=0.5) +
  geom_line() +
  geom_label_repel(data = p_obama %>% 
                     filter(t==max(t),state %in% ex_states),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2008-03-01','2008-11-03')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(subtitle = identifier)

# diff from national over time?
p_obama[p_obama$state != '--',] %>%
  left_join(p_obama[p_obama$state=='--',] %>%
              select(t,p_obama_national=mean), by='t') %>%
  mutate(diff=mean-p_obama_national) %>%
  group_by(state) %>%
  mutate(last_prob = last(prob)) %>%
  filter(state %in% ex_states) %>%
  ggplot(.,aes(x=t,y=diff,col=state)) +
  geom_hline(yintercept=0.0) +
  geom_line() +
  geom_label_repel(data = . %>% 
                     filter(t==max(t),
                            state %in% ex_states),
                   aes(label=state)) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2008-03-01','2008-11-03')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(-1,1,0.01)) +
  labs(subtitle = identifier)

# brier scores
# https://www.buzzfeednews.com/article/jsvine/2016-election-forecast-grades
compare <- p_obama %>% 
  filter(t==max(t),state!='--') %>% 
  select(state,obama_win = prob) %>% 
  mutate(obama_win_actual = ifelse(state %in% c('CA','NV','OR','WA','CO','NM','MN','IL','VA','DC','MD','DE','NJ','CT','RI','MA','NH','VT','NY','HI','ME','MI','IA','OH','PA','WI','FL','NC','IN'),1,0),
         diff = (obama_win_actual - obama_win )^2) %>% 
  left_join(enframe(ev_state) %>% set_names(.,c('state','ev'))) %>% 
  mutate(ev_weight = ev/(sum(ev))) 


briers.2008 <- tibble(outlet='economist (backtest)',
       ev_wtd_brier = weighted.mean(compare$diff, compare$ev_weight),
       unwtd_brier = mean(compare$diff),
       states_correct=sum(round(compare$obama_win) == round(compare$obama_win_actual)))

briers.2008

# model vs final polls vs prior
p_obama %>%
  filter(t == max(t),state %in% ex_states) %>%
  mutate(se = (high - mean)/2) %>%
  select(state,model_mean=mean,model_se=se) %>%
  left_join(df %>%
              filter(t > (max(t)-14),
                     state %in% ex_states) %>%
              group_by(state) %>%
              summarise(poll = weighted.mean(p_obama,n_respondents))) %>%
  left_join(enframe(mu_b_prior,'state','prior') %>%
              mutate(prior = inv.logit(prior))) %>%
  ggplot(.,aes(y=state)) +
  geom_point(aes(x=poll,col='poll')) +
  geom_point(aes(x=model_mean,col='model')) +
  geom_point(aes(x=prior,col='prior'))

# expected EVs roughly match the stochastic simulations?
final_states <- enframe(mu_b_prior,'state','prior') %>%
  left_join(df %>%
              filter(t > (max(t)-14),) %>%
              group_by(state) %>%
              summarise(poll = weighted.mean(p_obama,n_respondents))) %>%
  mutate(avg_poll_less_prior = mean(poll - inv.logit(prior), na.rm=T)) %>%
  mutate(poll = ifelse(is.na(poll),
                       inv.logit(prior) + avg_poll_less_prior,
                       poll)) %>%
  select(state,poll)

errors <- mvtnorm::rmvnorm(100000, sigma = state_correlation) * 0.1^2

generated_evs <- lapply(1:ncol(errors),
                        function(x){
                          tibble( errors[,x] + final_states$poll[x]) %>% setNames(.,c(final_states$state[x]))
                        }) %>%
  bind_cols() %>%
  as.data.frame() %>%
  mutate(trial = row_number()) %>%
  gather(state,p_obama,1:(ncol(.)-1)) %>%
  left_join(states2008 %>% select(state,ev)) %>%
  group_by(trial) %>%
  summarise(dem_ev = sum((p_obama>0.5)*ev))

# final EV distribution
final_evs <- draws %>%
  left_join(states2008 %>% select(state,ev),by='state') %>%
  filter(t==max(t)) %>%
  group_by(draw) %>%
  summarise(dem_ev = sum(ev* (p_obama > 0.5)))


grid.arrange(
  ggplot(final_evs,aes(x=dem_ev,
                       fill=ifelse(dem_ev>=270,'Democratic','Republican'))) +
    geom_vline(xintercept = 270) +
    geom_histogram(binwidth=1) +
    theme_minimal() +
    theme(legend.position = 'top',
          panel.grid.minor = element_blank()) +
    scale_fill_manual(name='Electoral College winner',values=c('Democratic'='#3A4EB1','Republican'='#E40A04')) +
    labs(x='Democratic electoral college votes',title='2008, Potential Electoral College outcomes',
         subtitle=sprintf("p(dem win) = %s | full stan model",round(mean(final_evs$dem_ev>=270),3))),
  

    ggplot(generated_evs,aes(x=dem_ev,
                         fill=ifelse(dem_ev>=270,'Democratic','Republican'))) +
    geom_vline(xintercept = 270) +
    geom_histogram(binwidth=1) +
    theme_minimal() +
    theme(legend.position = 'top',
          panel.grid.minor = element_blank()) +
    scale_fill_manual(name='Electoral College winner',values=c('Democratic'='#3A4EB1','Republican'='#E40A04')) +
    labs(x='Winner',
         subtitle=sprintf("p(dem win) = %s | simple rmvnorm simulations",round(mean(generated_evs$dem_ev>=270),3)))
)
