# @knitr read_data_create_functions

library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(maps)
library(mapproj)
library(knitr)
library(mvtnorm)
library(DT)
library(reshape2)
library(ggrepel)

rm(list = ls())
load('output/out.RData')

# load workspace from last run
logit <- function(p) log(p/(1-p))

inv_logit <- function(x) 1/(1+exp(-x))

state_name_df <- data.frame(state = names(state_name), state_name = state_name, stringsAsFactors = FALSE)
pred$state <- as.character(pred$state)


plot_score <- function(state_abbr_vec, show_sim = FALSE, from = start_date){
    ncolumns <- min(5, length(state_abbr_vec))
    # Ugly hacks to force ggplot2::facet_wrap to order states by predicted Clinton score.
    # Creating state_pos factor variables, with levels ordered by predicted Cliton share.
    state_ordering <- order(-pred$p[pred$state %in% state_abbr_vec & pred$t == election_day])
    allstate_ordering <- order(-pred$p[pred$state %in% all_polled_states & pred$t == election_day])
    pred$state_pos <- factor(pred$state, levels = all_polled_states[allstate_ordering])
    df$state_pos <- factor(df$state, levels = all_polled_states[allstate_ordering])
    
    ordered_states <- state_abbr_vec[state_ordering]
    winprob <- pred %>% 
        filter(t == election_day & state %in% state_abbr_vec) %>% 
        arrange(state) %>% select(state, clinton_win) %>% 
        mutate(clinton_win = round(100*clinton_win)) %>%
        left_join(state_name_df, by = 'state')
    winprob$state_name <- ifelse(winprob$state != "--", winprob$state_name, "National Vote")
    state_labels <- paste(winprob$state_name, 
                          "\nPr(Clinton wins) = ", 
                          winprob$clinton_win, "%", sep = "")
    names(state_labels) <- winprob$state
    
    g <- ggplot(data = pred[pred$state %in% state_abbr_vec & pred$t >= from,])  
    
    if (show_sim == TRUE){
        for (i in 1:100){
            g <- g + geom_line(data = data.frame(p = as.vector(p_subset[i,,state_abbr_vec]), 
                                                 t = rep(dates, length(state_abbr_vec)), 
                                                 state = rep(state_abbr_vec, each = length(dates)),
                                                 state_pos = rep(factor(state_abbr_vec, 
                                                                        levels = state_abbr_vec[state_ordering]), 
                                                                 each = length(dates))) %>% filter(t >= from), 
                               aes(x = t, y = 100*p), 
                               col = "blue", alpha = .2, size = .1)
        }
    }
    g <- g + 
        geom_ribbon(aes(x = t, ymin = 100*low, ymax = 100*high), 
                    alpha = 0.2, fill = "blue") + 
        geom_hline(yintercept = 50) +
        geom_hline(data = data.frame(state_pos = factor(state_abbr_vec, 
                                                        levels = state_abbr_vec[state_ordering]), 
                                     prior = 100*inv_logit(mu_b_prior[state_abbr_vec])), 
                   color = "black", linetype = "dotted",
                   aes(yintercept=prior)) +
        geom_vline(xintercept = as.numeric(election_day)) +
        geom_point(data = df[df$state %in% state_abbr_vec & df$t >= from & !grepl('Econ',df$pollster),],
                   aes(x = t, y = 100*p_clinton, alpha = -sqrt(p_clinton*(1-p_clinton)/n_respondents)),
                   size = 1/min(2, 2+length(state_abbr_vec)))  +
        scale_alpha(range = c(.1, 1)) +
        geom_line(data = pred[pred$state %in% state_abbr_vec & pred$t >= max(all_t) & pred$t >= from,], 
                  aes(x = t, y = 100*p), color = "white", linetype = "solid", 
                  size = ifelse(length(state_abbr_vec) <= 2, .8, .6), alpha = .5) +
        geom_line(data = pred[pred$state %in% state_abbr_vec & pred$t >= max(all_t) & pred$t >= from,], 
                  aes(x = t, y = 100*p), color = "darkblue", linetype = "11", 
                  size = ifelse(length(state_abbr_vec) <= 2, .8, .6)) +
        geom_line(data = pred[pred$state %in% state_abbr_vec & pred$t <= max(all_t) & pred$t >= from,], 
                  aes(x = t, y = 100*p), color = "white", size = ifelse(length(state_abbr_vec) <= 2, 1, .8)) +
        geom_line(data = pred[pred$state %in% state_abbr_vec & pred$t <= max(all_t) & pred$t >= from,], 
                  aes(x = t, y = 100*p), color = "darkblue", size = ifelse(length(state_abbr_vec) <= 2, .8, .6)) +
        geom_point(data = pred[pred$state %in% state_abbr_vec & pred$t == election_day,],
                   aes(x = t, y = 100*p), size = 1/min(2, 2+length(state_abbr_vec))) +
        guides(color = FALSE, alpha = FALSE, linetype = FALSE) + 
        # scale_linetype_discrete(guide=FALSE) +
        facet_wrap(~ state_pos, ncol = ncolumns, labeller = as_labeller(state_labels, multi_line = TRUE)) +
        ylab("Clinton Share of the Clinton + Trump Vote (in %)") +
        xlab("") + 
        scale_x_date(date_breaks = ifelse(length(state_abbr_vec) <= 2, "1 month", "2 month"), date_labels = "%b") +
        theme_minimal() +
        theme(strip.text.y = element_text(angle = 0),
              panel.grid.minor = element_blank())
    return(g)
}

##################################
# Plot state and national trends #
##################################

# @knitr plot_national

plot_score("--", show_sim = TRUE)
# plot_score("--")

# @knitr plot_states

sorted_states <- (pred %>% filter(t == election_day & state != "--") %>% 
                      arrange(-p) %>% 
                      select(state))[,1] %>% 
                  as.character


if(length(sorted_states)<51){
    cutoffs <- seq(0,length(sorted_states),10)
    
    for(start_idx in cutoffs){
        print(plot_score(sorted_states[(start_idx+1):(start_idx+10)],  show_sim = F))
    }
    
}else{
    print(plot_score(sorted_states[2:11]))
    print(plot_score(sorted_states[12:21]))
    print(plot_score(sorted_states[22:31]))
    print(plot_score(sorted_states[32:41]))
    print(plot_score(sorted_states[42:51]))
}




###############################
# Shot pollster house effects #
###############################

# @knitr plot_house_effects

dp <- data.frame(polls = colnames(mu_c),
                 house_effect_P50 = 100*(inv_logit(as.vector(apply(mu_c, 2, median)))-.5),
                 house_effect_P05 = 100*(inv_logit(as.vector(apply(mu_c, 2, function(x) quantile(x, 0.05))))-.5),
                 house_effect_P95 = 100*(inv_logit(as.vector(apply(mu_c, 2, function(x) quantile(x, 0.95))))-.5))

ggplot(data = dp) + 
    geom_histogram(data = dp[dp$house_effect_P50 > 0,], aes(x = house_effect_P50), 
                   fill = "#6E90F8", color = "blue", 
                   boundary = 0,
                   bins = 20) +
    geom_histogram(data = dp[dp$house_effect_P50 <= 0,], aes(x = house_effect_P50), 
                   fill = "#FF6666", color = "red", 
                   boundary = 0,
                   bins = 20) +
    # geom_histogram adds a horizontal line at y=0 where count == 0; redrawing it to get the right color.
    geom_line(data = data.frame(x = c(min(dp$house_effect_P50), 0), y = c(0,0)), aes(x,y), color = "red") +
    geom_line(data = data.frame(x = c(max(dp$house_effect_P50), 0), y = c(0,0)), aes(x,y), color = "blue") +
    xlab("Approximate Pollster House Effect in Percentage Points") + 
    ylab("Number of pollsters") + 
    scale_y_continuous(breaks = seq(0,100, 2))

dp[,2:4] <- round(dp[,2:4], 1)

# @knitr polls_most_pro_clinton

dp %>% arrange(-house_effect_P50)  %>% filter(house_effect_P50 >= 1) %>% head(., n=10) %>% kable(., col.names = c("Poll Origin", "Median", "P95", "P05"))

# @knitr polls_most_pro_trump

dp %>% arrange(house_effect_P50) %>% filter(house_effect_P50 <= -1) %>% head(., n=10) %>% kable(., col.names = c("Poll Origin", "Median", "P95", "P05"))

#########
# alpha #
#########

# @knitr alpha

ggplot(data.frame(alpha = 100*(inv_logit(alpha + 
                                         logit(pred$p[pred$state == "--" & pred$t == all_t[length(all_t)]]))-
                                               pred$p[pred$state == "--" & pred$t == all_t[length(all_t)]]) )) + 
    geom_histogram(aes(x = alpha), bins = 50, fill = "grey", color = "darkgrey") + 
    xlab("Estimated National/States Discrepancy of Clinton Scores (in % Points)") +
    ylab("Number of simulations")

################################
# State-by-state probabilities #
################################

# @knitr plot_state_probabilities

ggplot(data = pr_all_states) + 
    geom_point(aes(x = p, y = state_name, order = order_states, color = p, alpha = ifelse(polled, 1, 1/10))) +
    xlab("Pr(Clinton wins)") +
    ylab("") +
    ggtitle("State Probabilities") + 
    guides(color = FALSE, alpha = FALSE) + 
    scale_color_gradient2(low = "#FF6666", mid = "purple", high = "#6E90F8", midpoint = 50, na.value = "grey50")

# @knitr map

states_map <- map_data("state")
ggplot() + 
    geom_map(data = states_map, 
             map = states_map, aes(x=long, y=lat, map_id = region)) +
    geom_map(data = pr_all_states, 
             map = states_map, aes(fill= p, map_id = tolower(state_name))) +
    scale_fill_gradient2("Pr(Clinton\nwins)", low = "red3", mid = "white", high = "royalblue", midpoint = 50) + 
    theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), legend.position="bottom") + 
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(x = NULL, y = NULL) 

###############################
# Graph of Distribution of EV #
###############################

# Not accounting for the EV allocation rules of Nebraska and Maine

# @knitr plot_ev

ggplot() + 
    geom_histogram(data = data.frame(ev = result_ev_all_states), aes(ev, fill = ifelse(ev >= 270,"Clinton","Trump")), binwidth = 1) + 
    scale_fill_manual(values=c("#6E90F8", "#FF6666"), guide = guide_legend(title = "Winner")) +
    xlab("Electoral Votes for Clinton")  +
    theme_minimal() + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.title.y = element_blank()) +
    ggtitle(paste("EV distribution - Pr(Clinton wins) = ", round(mean(result_ev_all_states >= 270)*100, 0), "%", sep = ""))


###############################
# Graph of EV probs over time #
###############################

# @knitr plot_ev_overtime
dem_ev_overtime %>%
    ggplot(.,aes(x=ymd(date),y=dem_mean)) +
    geom_line() +
    geom_hline(yintercept = 270) +
    geom_ribbon(aes(ymin=dem_lower,ymax=dem_upper),col=NA,fill='blue',alpha=0.2) +
    geom_ribbon(aes(ymin=dem_lower80,ymax=dem_upper80),col=NA,fill='blue',alpha=0.4) +
    scale_x_date(date_breaks='1 month',date_labels='%B') +
    theme_minimal() +
    labs(title='Projected Clinton electoral votes over time',
         x='Date',
         y='') +
    # little lines
    geom_line(data=hundred_sims_day,aes(x=ymd(date),y=ev,group=trial),col='blue',size=0.2,alpha=0.2)

# @knitr plot_ev_prob_overtime
dem_ev_overtime %>%
    ggplot(.,aes(x=ymd(date),y=prob)) +
    geom_hline(yintercept = 0.5) +
    geom_line() +
    theme_minimal()+
    labs(title='Projected Clinton probability of electoral college victory over time',
         x='Date',
         y='')  +
    coord_cartesian(ylim=c(0,1)) +
    scale_x_date(date_breaks='1 month',date_labels='%B')
    

###############################
# Misc #
###############################


## @knitr model_checks

print(stan_summary)

## @knitr plot_mu

median_mu_a <- apply(mu_a, c(2), median)
median_mu_b <- apply(mu_b, c(2,3), median)[,-1]

median_mu_a <- apply(mu_a, c(2), median)
median_mu_b <- apply(mu_b, c(2,3), median)[,-1]

plot(as.Date(names(median_mu_a)), median_mu_a, ylim = c(-.8, .8), 
     type = "l", col = "red", lwd = 2, 
     xlab = "Date", ylab = expression(paste("Median ",mu[a](t)," and ", mu[b](t,s), " (logit scale)")), 
     xlim = range(as.numeric(all_t_until_election)))
for(s in all_polled_states[-1]) points(as.Date(rownames(median_mu_b)), median_mu_b[,s], type = "l", col = "darkgrey")
abline(v = max(all_t))

## @knitr priors

m <- rmvnorm(1e5, mu_b_prior[-1], sigma_mu_b_end)
colnames(m) <- all_polled_states[-1]
m0 <- rmvnorm(1e5, mu_b_prior[-1], cov_matrix(length(mu_b_prior[-1]), sigma_mu_b_end[1,1], 0))
colnames(m0) <- all_polled_states[-1]

## @knitr table_predictions

table_pred <- data.frame(pred[pred$t == all_t[length(all_t)], c("state","p")], 
                         pred[pred$t == election_day, c("p", "clinton_win")])
colnames(table_pred) <- c("state", "p_now", "p_forecast", "clinton_win")
table_pred[,2:3] <- round(table_pred[,2:3], 3)
table_pred[,4] <- round(table_pred[,4],2)
table_pred$state <- table_pred$state %>% as.character
table_pred$state_name <- state_name[as.character(table_pred$state)]
table_pred$state_name[is.na(table_pred$state_name)] <- "*National Vote*"
rownames(table_pred) <- NULL
colors_red_to_blue <- colorRampPalette(c("#FF6666", "white","#6E90F8"))
table_pred <- table_pred %>%
    left_join(df %>% group_by(state) %>%
                  summarize(number_polls = n()) %>%
                  ungroup()) %>%
    arrange(state_name)
table_pred$prior <- inv_logit(mu_b_prior)[table_pred$state]
table_pred$diffprior_now <-table_pred$p_now - table_pred$prior

datatable(table_pred[,c("state_name", "number_polls", "prior", "diffprior_now", "p_now", "p_forecast", "clinton_win")],
          colnames = c("State", "Number of Polls", "Prior", "Current - Prior Difference", "Current Score", "Forecast Score", "Pr(Clinton Wins)"),
          rownames = FALSE,
          options = list(dom = c('f t'), pageLength = nrow(table_pred))) %>%
    formatStyle('clinton_win',
                backgroundColor=styleInterval(seq(.01,.99,.01), colors_red_to_blue(100))) %>%
    formatPercentage('clinton_win', 0) %>%
    formatPercentage(c('prior', 'diffprior_now', 'p_now', 'p_forecast'), 1)

# correlation_states
m <- rmvnorm(1e4, mu_b_prior[-1], sigma_mu_b_end)
rho <- apply(m, 1, function(x) cor(inv_logit(mu_b_prior[-1]), inv_logit(x)))
m0 <- rmvnorm(1e4, mu_b_prior[-1], cov_matrix(length(mu_b_prior[-1]),sigma_mu_b_end[1,1], 0))
rho0 <- apply(m0, 1, function(x) cor(inv_logit(mu_b_prior[-1]), inv_logit(x)))

# @knitr TX-VA-example

correlated_prior_va_tx <- inv_logit(rmvnorm(1e4, mu_b_prior[-1], sigma_mu_b_end)[,c("VA","TX")])
uncorrelated_prior_va_tx <- inv_logit(rmvnorm(1e4, mu_b_prior[-1], cov_matrix(length(mu_b_prior[-1]), sigma_mu_b_end[1,1], 0))[,c("VA","TX")])
    
ex_df <- rbind(data.frame(va = uncorrelated_prior_va_tx[,"VA"],
                          tx = uncorrelated_prior_va_tx[,"TX"],
                          type = "Independent Priors"),    
               data.frame(va = correlated_prior_va_tx[,"VA"],
                          tx = correlated_prior_va_tx[,"TX"],
                          type = "Correlated Priors") #,
               # data.frame(va = inv_logit(mu_b[,length(all_weeks_until_election), "VA"]),
               #          tx = inv_logit(mu_b[,length(all_weeks_until_election), "TX"]),
               #          type = "Posterior")
               )

minmax <- c(min(ex_df$va, ex_df$tx), max(ex_df$va, ex_df$tx))

ggplot(data = ex_df, aes(x=va, y=tx)) + 
    geom_point(alpha = .1, size = .5) + 
    geom_line(data = data.frame(x=minmax, y=minmax), aes(x=x, y=y), linetype = "dashed", color = "black") +
    xlim(minmax) + 
    ylim(minmax) + 
    xlab("Clinton Score (Virginia)") + 
    ylab("Clinton Score (Texas)") + 
    facet_wrap(~ type)

# @correlation_states_graph

cor_df <- rbind(data.frame(r = rho0, type = rep("Independent Priors", length(rho0))),
                data.frame(r = rho , type = rep("Correlated Priors",   length(rho )))) #,
                # data.frame(r = apply(mu_b[,length(all_weeks_until_election),-1], 1, 
                #                      function(vec) cor(inv_logit(vec), inv_logit(mu_b_prior[-1]))),
                #                      type = rep("Posterior", nrow(mu_a))))

prev_df <- data.frame(r = c(0.9822, 0.9431, 0.9762, 0.9563, 0.9297, 0.8959, 0.9347), 
                      year = c("2012-2008", "2008-2004", "2004-2000", "2000-1996", "1996-1992", "1992-1988", "1988-1984"))

ggplot(data = cor_df) + 
    geom_histogram(aes(x = r, ..density..), bins = 50, color = "darkgrey", fill = "grey") + 
    geom_point(data = prev_df, aes(y = 0, x = r), size = .75) + 
    geom_text(data = prev_df, aes(y = 0, x = r, label = year), angle = 90, size = 2.5, hjust = "left", nudge_y = 3) +
    facet_wrap(~ type) + 
    xlab("Correlation between 2016 and 2012 state scores") + 
    ylab("Density")

# @knitr sigma_walk

ggplot() + 
    geom_histogram(data = data.frame(sigma = c(sigma_walk_a_past, 
                                               sigma_walk_b_past), 
                                     name = c(rep("sigma_a", nrow(mu_a)), 
                                              rep("sigma_b", nrow(mu_a)))),
                   aes(x = sigma), bins = 50, fill = "grey", color = "darkgrey") + 
    facet_grid(name ~ .) +
    geom_vline(data = data.frame(median = c(median(sigma_walk_a_past), 
                                            median(sigma_walk_b_past)),
                                 name = c("sigma_a", "sigma_b")), 
               aes(xintercept = median), linetype = "dotted") +
    geom_text(data = data.frame(median = c(median(sigma_walk_a_past), 
                                           median(sigma_walk_b_past)),
                                name = c("sigma_a", "sigma_b")),
              aes(x = median, y = 400, label = round(median, 3))) +
    ylab("Number of simulations") +
    xlab("Backward component: reverse random walk standard deviations")

# @knitr rnd_walk

remaining_days <- as.Date("2016-08-01", origin = "1970-01-01"):election_day

plot_data <- list()
for(i in 1:3){
sim_rnd_walk <- 
    rbind(rmvnorm(1, mu_b_prior[-1], sigma_mu_b_end),
          rmvnorm(length(remaining_days)-1, rep(0, length(mu_b_prior) - 1), 
                  cov_matrix(length(mu_b_prior) - 1, 
                             sigma_walk_b_forecast[1,1]/7, 
                             rho = sigma_walk_b_forecast[2,1]/sigma_walk_b_forecast[1,1]))) %>%
    apply(., 2, function(vec) cumsum(vec)) %>%
    apply(., 2, rev) %>%
    inv_logit(.)
    rownames(sim_rnd_walk) <- as.character(remaining_days)
    sim_rnd_walk <- as.data.frame(sim_rnd_walk)
    sim_rnd_walk$date <- as.Date(remaining_days, origin= "1970-01-01")
    plot_data[[i]] <- melt(sim_rnd_walk, id.var = "date")
    plot_data[[i]]$sim <- i
}

plot_data <- rbind(plot_data[[1]], plot_data[[2]], plot_data[[3]])
plot_data <- plot_data %>% 
    rename(state = variable, clinton = value) %>% 
    group_by(state, sim) %>% 
    mutate(last = last(clinton)) %>% 
    ungroup

ggplot(data = plot_data[plot_data$date >= Sys.Date(),])  +
    facet_grid(. ~ sim) + 
    geom_line(aes(x=as.Date(date), y=clinton, group=state, color = last), alpha = .5, size = .2) + 
    scale_color_gradientn(colors = c("darkred", "red", "purple", "royalblue", "blue"), values = c(0,.4,.5,.8,1)) +
    guides(color = FALSE) +
    xlab("") + 
    ylab("Clinton vote intentions (Simulation)") +
    geom_point(data = plot_data[plot_data$date == election_day,],
               aes(x=as.Date(date), y=clinton, color = last), size = .4) + 
    geom_text(data = plot_data[plot_data$date == election_day,],
              aes(x = as.Date(date), y = clinton, label = state), 
              size = 2.5, hjust = "left", nudge_x = 1, check_overlap = TRUE) + 
    xlim(c(as.Date(remaining_days[1], origin = "1970-01-01"), as.Date("2016-11-21", origin = "1970-01-01")))


