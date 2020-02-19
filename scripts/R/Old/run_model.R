rm(list = ls())
options(mc.cores = parallel::detectCores())

library(tidyverse)
library(rstan)
library(purrr)
library(stringr)
library(lubridate)
library(curl)
library(shinystan)
library(rmarkdown)
library(survey)
library(gridExtra)
library(pbapply)

# master vars
RUN_DATE <- min(ymd('2016-11-08'),Sys.Date())

election_day <- ymd("2016-11-08")
start_date <- as.Date("2016-03-01") # Keeping all polls after March 1, 2016


# Useful functions ---------
corr_matrix <- function(m){
    (diag(m)^-.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m)^-.5 * diag(nrow = nrow(m))) 
}

cov_matrix <- function(n, sigma2, rho){
    m <- matrix(nrow = n, ncol = n)
    m[upper.tri(m)] <- rho
    m[lower.tri(m)] <- rho
    diag(m) <- 1
    (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) 1/(1 + exp(-x))



# wrangle polls -----------------------------------------------------------
# read
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
           n_clinton = round(n *(two_party_sum/100)* clinton/100),
           p_clinton = (clinton/two_party_sum))

    
# Numerical indices passed to Stan for states, days, weeks, pollsters
df <- df %>% 
    mutate(week = floor_date(t - days(2), unit = "week") + days(2), # weeks start on Tuesdays.
           day_of_week = as.integer(t - week), # number of days since beginnning of the week
           index_s = as.numeric(as.factor(as.character(state))), 
           # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
           index_t = 1 + as.numeric(t) - min(as.numeric(t)),
           index_w = as.numeric(as.factor(week)),
           index_p = as.numeric(as.factor(as.character(pollster))))  


# selections
df <- df %>%
    arrange(state, t, polltype, two_party_sum) %>% 
    distinct(state, t, pollster, .keep_all = TRUE) %>%
    select(
        # poll information
        state, t, begin, end, pollster, polltype, method = mode, n_respondents, 
        # vote sahres
        p_clinton, n_clinton, 
        p_trump, n_trump, 
        p_other, n_other,
        # indecies
        week, day_of_week, starts_with("index_")) 



# Print today's snapshot! -------------------------------------------------
print("New polls today:")
df %>% filter(end >= RUN_DATE-1) %>% select(-polltype, -t, -method, -n_clinton)
cat(sprintf('%s total polls',nrow(df)))

# Removing overlapping polls ---------
df$overlap_with_prev <- TRUE

while(any(df$overlap_with_prev == TRUE)){
    # Recursively drop polls if their dates overlap
    # Perhaps not the most efficient way, but it gets the job done, and the data frame is small.
    df <- df %>% 
        arrange(state, pollster, t) %>%
        group_by(state, pollster) %>% 
        mutate(end_prev_poll = lag(end), 
               overlap_with_prev = ifelse(!is.na(end_prev_poll), 
                                          end_prev_poll > begin,
                                          FALSE),
               overlap_with_next = ifelse(!is.na(lead(overlap_with_prev)), 
                                          lead(overlap_with_prev), 
                                          FALSE),
               latest_overlap = overlap_with_prev == TRUE & 
                   overlap_with_next == FALSE,
               # Drop all polls which overlap with next poll
               #                              if   next poll is the most recent (latest) poll 
               #                                   in the series of overlapping polls.
               drop_poll = (overlap_with_next == TRUE & 
                                (lead(latest_overlap) == TRUE | is.na(lead(latest_overlap))))
        ) %>%
        filter(drop_poll == FALSE) %>%
        ungroup
    
    # The while loop keeps going until no overlap is found.
    #print(nrow(df))
}


# Useful vectors ---------
all_polled_states <- df$state %>% unique %>% sort
ndays <- max(df$t) - min(df$t)
all_t <- min(df$t) + days(0:(ndays))
all_t_until_election <- min(all_t) + days(0:(election_day - min(all_t)))
week_for_all_t <- floor_date(all_t - days(2), unit="week") + days(2)

all_pollsters <- levels(as.factor(as.character(df$pollster)))


# Correlation matrix ---------
state_correlation <- read.csv("data/state_correlation_matrix.csv"

# Reading 2012 election data to --------- 
# (1) set priors on mu_b and alpha,
# (2) get state_weights,           
# (3) get state_names and EV        

states2012 <- read.csv("data/2012.csv", 
                       header = TRUE, stringsAsFactors = FALSE) %>% 
    mutate(score = obama_count / (obama_count + romney_count),
           national_score = sum(obama_count)/sum(obama_count + romney_count),
           diff_score = score - national_score,
           share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
           /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
    arrange(state)

rownames(states2012) <- states2012$state

prior_diff_score <- states2012[all_polled_states[-1],]$diff_score
names(prior_diff_score) <- all_polled_states[-1]

state_weights <- c(0, states2012[all_polled_states[-1],]$share_national_vote / sum(states2012[all_polled_states[-1],]$share_national_vote))
names(state_weights) <- c("--", states2012[all_polled_states[-1],]$state)

all_states <- states2012$state
state_name <- states2012$state_name
names(state_name) <- states2012$state

# electoral votes, by state:
ev_state <- states2012$ev
names(ev_state) <- states2012$state

# unique date indices for days in which national polls were conducted ---------
# See the Stan model: trying to speed up computation of the likelihood by calculating
# the weighted mean of state latent vote intentions only for days national polls were conducted.

unique_ts <- unique(df$index_t[df$state == "--"])

natpolls_indices <- data.frame(unique_ts = unique_ts,
                               unique_ws = sapply(unique_ts, function(i) unique(df$index_w[df$index_t == i])))


df$index_t_unique <- sapply(1:nrow(df), 
                            function(i) ifelse(df$state[i] == "--", 
                                               which(natpolls_indices$unique_ts == df$index_t[i]), 
                                               0))

# Creating priors ---------

# Mean of the mu_b_prior
# 0.486 is the predicted Clinton share of the national vote according to the Time for Change model.
mu_b_prior <- logit(0.486 + c("--" = 0, prior_diff_score))

# The model uses national polls to complement state polls when estimating the national term mu_a.
# One problem until early September, was that voters in polled states were different from average voters :
# Several solid red states still hadn't been polled, the weighted average of state polls was slightly more pro-Clinton than national polls.

score_among_polled <- sum(states2012[all_polled_states[-1],]$obama_count)/
    sum(states2012[all_polled_states[-1],]$obama_count + 
            states2012[all_polled_states[-1],]$romney_count)

alpha_prior <- log(states2012$national_score[1]/score_among_polled)

# prior on state-level correlation of means in reverse random walk
sigma_mu_b_end <- cov_matrix(n = length(mu_b_prior) - 1, sigma2 = 1/20, rho = 0.5)

# prior on state-level correlation of means in forward random walk
sigma_walk_b_forecast <- cov_matrix(length(mu_b_prior) - 1, 7*(0.015)^2, 0.75)

# correlation of state-level polling error
sigma_poll_error <- cov_matrix(length(mu_b_prior) - 1, 0.08^2, .8) # bump the error to 2.75% and correlation to 0.8


# or, we could read in a correlation matrix for all these?
state_corr_matrix <- read_csv('data/state_correlation_matrix.csv')

# filter to states that have been polled?
names(state_corr_matrix)[names(state_corr_matrix) %in% all_polled_states]

# substitute? this isn't going to work until we add in the national vote share as varying, too.
# sigma_mu_b_end <- state_corr_matrix %>% as.matrix() 
# sigma_walk_b_forecast <- state_corr_matrix %>% as.matrix() 
# sigma_poll_error <- state_corr_matrix %>% as.matrix() 


# Passing the data to Stan and running the model ---------
out <- stan("scripts/poll_model.stan", 
            data = list(N = nrow(df),                  # Number of polls
                        S = max(df$index_s),           # Number of states
                        T = length(all_t_until_election),           # Number of days
                        W = length(all_weeks_until_election),           # Number of weeks
                        P = max(df$index_p),           # Number of pollsters
                        last_poll_T = length(all_t),
                        last_poll_W = length(all_weeks),
                        T_unique = max(df$index_t_unique), 
                        t_unique = df$index_t_unique,
                        unique_ts = natpolls_indices$unique_ts,
                        unique_ws = natpolls_indices$unique_ws,
                        s = df$index_s,
                        t = df$index_t,
                        w = df$index_w,
                        p = df$index_p,
                        n_clinton = df$n_clinton,
                        n_respondents = df$n_respondents,
                        state_weights = state_weights,
                        alpha_prior = alpha_prior,
                        mu_b_prior =  mu_b_prior,
                        sigma_mu_b_end = sigma_mu_b_end,
                        sigma_walk_b_forecast = sigma_walk_b_forecast,
                        sigma_poll_error = sigma_poll_error,
                        week = as.integer(as.factor(week_for_all_t)),
                        day_of_week = as.integer(all_t - week_for_all_t)),
            refresh=1,
            chains = 4, iter = 1000, warmup=500
            )

write_rds(out,sprintf('models/stan_model_%s.rds',RUN_DATE),compress = 'gz')
# out <- read_rds(sprintf('models/stan_model_%s.rds',RUN_DATE))


stan_summary <- capture.output(print(out, pars = c("alpha", "sigma_c", 
                                                   "sigma_u_state", "sigma_u_national",
                                                   "sigma_walk_a_past", "sigma_walk_b_past",
                                                   paste("mu_b[33,", as.character(2:max(df$index_s)),"]", 
                                                         sep =""))))
stan_summary


# Extracting results  ---------
p <- rstan::extract(out, pars = "predicted_score")[[1]]
alpha <- rstan::extract(out, pars = "alpha")[[1]]
mu_a <- rstan::extract(out, pars = "mu_a")[[1]]
mu_b <- rstan::extract(out, pars = "mu_b")[[1]]
mu_c_standardized <- rstan::extract(out, pars = "mu_c")[[1]]
sigma_c <- rstan::extract(out, pars = "sigma_c")[[1]]
mu_c <- as.vector(sigma_c)*mu_c_standardized
sigma_walk_b_past <- rstan::extract(out, pars = "sigma_walk_b_past")[[1]]
sigma_walk_a_past <- rstan::extract(out, pars = "sigma_walk_a_past")[[1]]
sigma_u_state <- rstan::extract(out, pars = "sigma_u_state")[[1]]
sigma_u_national <- rstan::extract(out, pars = "sigma_u_national")[[1]]
poll_error <- rstan::extract(out, pars = "poll_error")[[1]]

dates <- sort(c(all_t[all_t <= all_weeks[length(all_weeks)]], 
                unique(setdiff(all_weeks_until_election + days(3), all_weeks + days(3)))))
dates <- c(dates[-length(dates)], election_day)

# dates = all dates until last Tuesday; followed by:       # for mu_a daily + interpolated mu_b weekly components
#         all Fridays until election day; followed by:     # because mu_b weekly components are centered on Fridays 
# (weeks start on Tuesdays; the midpoint between 2 successive Tuesdays is Friday).
#         election day.

dimnames(p) <- list(1:nrow(p), as.character(dates), all_polled_states)
dimnames(mu_a) <- list(1:nrow(mu_a), as.character(all_t))
dimnames(mu_b) <- list(1:dim(mu_b)[1],
                       as.character(all_weeks_until_election + days(3)),
                       all_polled_states)
dimnames(mu_c) <- list(1:nrow(mu_c), all_pollsters) #1:ncol(mu_c)

# hist(apply(p[,as.character(election_day),-1], 1, function(vec) cor(vec, inv_logit(mu_b_prior[-1]))))

pred <- data.frame(t = rep(dates, length(all_polled_states)),
                   state = as.character(rep(all_polled_states, each = length(dates))),
                   p =    apply(p, c(2,3), median) %>% as.vector,
                   p_sd = apply(p, c(2,3), sd) %>% as.vector,
                   high = apply(p, c(2,3), function(x) quantile(x, .95)) %>% as.vector,
                   low =  apply(p, c(2,3), function(x) quantile(x, .05))  %>% as.vector,
                   clinton_win = apply(p, c(2,3), function(x) mean(x > .5))  %>% as.vector)

# graphs.R will need indexes for last poll day. But the value is not computed by Stan
# in the generated quantities block in Stan.
# Solution: recovering values for pred[pred$t == all_t[length(all_t)],] by interpolation

now <- all_t[length(all_t)]
before <- dates[max(which(dates <= now))]
after  <- dates[min(which(dates >= now))]
days_before <- days(now-before)@day
days_after <-days(after-now)@day
w_before <- days_after/(days_before + days_after)
w_after <-  days_before/(days_before + days_after)
w <- data.frame(w = c(w_before, w_after), t = c(before, after))

interpolated_rows <- pred %>% filter(t == before | t == after) %>% 
    left_join(w, by = "t") %>% 
    group_by(state) %>%
    summarize(p = weighted.mean(p, w), 
              p_sd = weighted.mean(p_sd, w), 
              high = weighted.mean(high, w), 
              low = weighted.mean(low, w), 
              clinton_win = weighted.mean(clinton_win, w)) %>% 
    mutate(t = now) 

pred <- bind_rows(pred, interpolated_rows)


# cov_logit_p <- lapply(as.character(all_t), function(t) cov(logit(p[,t,-1])))
# names(cov_logit_p) <- as.character(all_t)

# median_logit_forecast <- logit(pred$p[pred$t == election_day & pred$state != "--"])
# names(median_logit_forecast) <- pred$state[pred$t == election_day & pred$state != "--"]
# cov_logit_forecast <- cov(logit(p[,as.character(election_day),-1]))
# cov_forecast <- cov(p[,as.character(election_day),-1])

# Predicted electoral votes for each simulation ---------


sim_forecast <- p[,as.character(election_day),-1]


sim_win <- sim_forecast > 0.5

all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
non_polled_win        <- states2012[all_non_polled_states,]$score > .5
names(non_polled_win) <- all_non_polled_states

non_polled_win_matrix <- rep(non_polled_win, nrow(sim_win)) %>%
    matrix(nr = nrow(sim_win), byrow = TRUE,
           dimnames = list(1:nrow(sim_win), all_non_polled_states))

sim_win_all_states <- cbind(sim_win, non_polled_win_matrix)

result_ev_all_states <- sim_win_all_states %*% ev_state[colnames(sim_win_all_states)]

# P(win electoral college)
mean(result_ev_all_states >= 270)

# P(win national vote)
mean(p[, as.character(election_day), "--"] > .5)


# get prediction for every day
days <- names(as.data.frame(p[,,2]))

dem_ev_overtime <- pblapply(days,
         function(day_string){
             #print(day_string)
             
             sim_forecast <- tryCatch(p[,as.character(day_string),-1])
             if(class(sim_forecast)=='try-error'){return(NULL)}

             sim_win <- sim_forecast > 0.5
             
             all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
             non_polled_win        <- states2012[all_non_polled_states,]$score > .5
             names(non_polled_win) <- all_non_polled_states
             
             non_polled_win_matrix <- rep(non_polled_win, nrow(sim_win)) %>%
                 matrix(nr = nrow(sim_win), byrow = TRUE,
                        dimnames = list(1:nrow(sim_win), all_non_polled_states))
             
             sim_win_all_states <- cbind(sim_win, non_polled_win_matrix)
             
             result_ev_all_states <- sim_win_all_states %*% ev_state[colnames(sim_win_all_states)]
             
             # P(win electoral college)
             tibble(date = day_string,
                    dem_mean = mean(result_ev_all_states),
                    dem_upper = quantile(result_ev_all_states,0.975),
                    dem_lower = quantile(result_ev_all_states,0.025),
                    dem_upper80 = quantile(result_ev_all_states,0.9),
                    dem_lower80 = quantile(result_ev_all_states,0.1),
                    prob = mean(result_ev_all_states >= 270)
             )

             
         }) %>% do.call('bind_rows',.)


# get 100 draws for each timeline
hundred_sims_day <- pblapply(days,
                            function(day_string){
                                #print(day_string)
                                
                                sim_forecast <- tryCatch(p[,as.character(day_string),-1])
                                if(class(sim_forecast)=='try-error'){return(NULL)}
                                
                                sim_win <- sim_forecast > 0.5
                                
                                all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
                                non_polled_win        <- states2012[all_non_polled_states,]$score > .5
                                names(non_polled_win) <- all_non_polled_states
                                
                                non_polled_win_matrix <- rep(non_polled_win, nrow(sim_win)) %>%
                                    matrix(nr = nrow(sim_win), byrow = TRUE,
                                           dimnames = list(1:nrow(sim_win), all_non_polled_states))
                                
                                sim_win_all_states <- cbind(sim_win, non_polled_win_matrix)
                                
                                
                                # tidy up
                                hundred_sims_context <- t(sim_win_all_states[1:100,]) %>% 
                                    as.data.frame() %>% 
                                    mutate(date=day_string,
                                           state=rownames(.)) %>%
                                    left_join(enframe(ev_state[colnames(sim_win_all_states)],name='state',value='ev')) %>%
                                    gather(trial,win,1:100)
                                
                                
                                # return first 100 draws
                                return(hundred_sims_context)
                                
                                
                            }) %>% do.call('bind_rows',.)

hundred_sims_day <- hundred_sims_day %>% 
    group_by(date,trial) %>%
    summarise(ev = sum(ev*win))

# Sorting predictions for every state, ready to be passed to ggplot for dotchart ---------

# for states with polls
pr_polled_states <- pred %>% filter(t == election_day & state != "--") %>%
    arrange(-clinton_win) %>%
    transmute(state, p=100*clinton_win, polled = TRUE)
all_non_polled_states <- setdiff(all_states, all_polled_states[-1])
all_non_polled_states <- ifelse(length(all_non_polled_states) == 0, NA, all_non_polled_states)

# for states without
non_polled_win <- states2012[all_non_polled_states,]$score > .5
names(non_polled_win) <- all_non_polled_states
pr_other_states <- data.frame(state = names(non_polled_win), 
                              p = 100*non_polled_win, polled = FALSE)

# come together, right now
pr_all_states <- rbind(na.omit(pr_other_states), na.omit(pr_polled_states))

pr_all_states$position <- ifelse(pr_all_states$p == 100 & !pr_all_states$polled,
                                 0, ifelse(pr_all_states$p == 0 & !pr_all_states$polled,
                                           2,1))
pr_all_states <- pr_all_states %>% arrange(-p, position)

pr_all_states$state_name <- state_name[as.character(pr_all_states$state)]
pr_all_states$order_states <- nrow(pr_all_states):1
pr_all_states$state_name <- factor(pr_all_states$state_name, levels = pr_all_states$state_name[pr_all_states$order_states])


# Take a subset of 100 simulations of predicted scores to display in ggplot  ---------


p_subset <- p[sample(1:nrow(p), 100, replace = FALSE),,]


# Info on last polls ---------
# Recording date/time of last model update and recording time zone.
time_lastrun <- RUN_DATE
attributes(time_lastrun)$tzone <- Sys.timezone()

last_polls <- df %>% 
    arrange(desc(entry_date)) %>% 
    filter(entry_date >= RUN_DATE -1) %>%
    mutate(p_clinton = round(p_clinton*100, 1), 
           p_trump = 100-p_clinton, 
           N=n_respondents) %>% 
    select(entry_date, pollster, state, p_clinton, p_trump, N)

# save forecasts from today
save(list = c("sim_forecast", "ev_state"), file = sprintf("output/sims/sims_%s.RData",RUN_DATE))


rm(out)
rm(p)

# saving workspace for graphics
# this is bad practice byt allows us to recreate graphics and reports without rerunning the model
save.image("output/out.RData")

# render report?
load('output/out.RData')
render("report.Rmd",output_file = sprintf("output/reports/report_%s",RUN_DATE))
rm('output/out.RData') # for now, removing so file size is smaller
