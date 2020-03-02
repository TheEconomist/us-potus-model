# here is Kremp's code for making predictions in the generated quantities block
# 
# generated quantities{
# matrix[last_tuesday + W - last_poll_W, S] predicted_score;
# // Predicted scores have *daily* values for past dates (since they depend on mu_b AND mu_a parameters), 
# // but *weekly* values for future dates (since they only depend on mu_b).
# for (state in 2:S){
#   // Backward estimates (daily)
#   for (date in 1:last_tuesday){
#     // Linear interpolation of mu_b values between previous and current week, 
#     // or current and next week
#     // Using the following weights, depending on day_of_week:
#       //         w-1    w       w+1
#     // 0 Tue   3/7    4/7  
#     // 1 Wed   2/7    5/7  
#     // 2 Thu   1/7    6/7  
#     // 3 Fri   0      7/7  
#     // 4 Sat          6/7    1/7 
#     // 5 Sun          5/7    2/7 
#     // 6 Mon          4/7    3/7 
#     if (day_of_week[date] <= 3){ // 0=Sun, 1=Mon, 2=Tue, 3=Wed
#     predicted_score[date, state] = 
#       inv_logit(mu_a[date] + (1.0-(4+day_of_week[date])/7.0)*mu_b[max(week[date]-1, 1), state]
#                 +     ((4+day_of_week[date])/7.0)*mu_b[week[date], state]);
#     }
#     else{ // 4=Thu, 5=Fri, 6=Sat
#     predicted_score[date, state] = // Linear interpolation between current and next week
#     inv_logit(mu_a[date] +     ((10-day_of_week[date])/7.0)*mu_b[week[date], state]
#               + (1.0-(10-day_of_week[date])/7.0)*mu_b[min(week[date]+1, W), state]);
#     }
#     // predicted_score[date, state] = // Just a little bit of linear interpolation between weeks
#     //            inv_logit(mu_a[date] + (1.0-day_of_week[date]/7.0)*mu_b[week[date], state]
#                             //                                 +     (day_of_week[date]/7.0)*mu_b[min(week[date]+1, W), state]);
#   }
#   // Forward estimates (weekly)
#   for (date in (last_tuesday+1):(last_tuesday + W - last_poll_W))
#     predicted_score[date, state] = inv_logit(mu_b[last_poll_W + date - last_tuesday, state]);
# }
# for (date in 1:(last_tuesday + W - last_poll_W))
#   // National score: averaging state scores by state weights.
# predicted_score[date, 1] = predicted_score[date, 2:S] * state_weights[2:S];
# }
#

# RUN_DATE <- "2016-11-08"
# 
# out <- read_rds(sprintf('models/stan_model_%s.rds',RUN_DATE))

### Extract ----
# movement in the random walk
x <- rstan::extract(out, pars = "sigma_a")[[1]] 
y <- rstan::extract(out, pars = "sigma_b")[[1]] 

tibble(national_sigma = x,state_sigma = y) %>%
  gather(variable,value) %>%
  ggplot(aes(x=value)) +
  geom_histogram(binwidth=0.001,alpha=0.5,col='gray40') +
  facet_grid(rows=vars(variable),switch = 'y') +
  coord_cartesian(xlim=c(0,0.1))

# alpha
alpha <- rstan::extract(out, pars = "alpha")[[1]] 

hist(alpha)

# national average
sum_average_states <- rstan::extract(out, pars = "sum_average_states")[[1]] 

tibble(low = apply(sum_average_states,2,function(x){inv.logit(quantile(x,0.025))}),
       high = apply(sum_average_states,2,function(x){inv.logit(quantile(x,0.975))}),
       mean = apply(sum_average_states,2,function(x){inv.logit(mean(x))})) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ggplot(.,aes(x=t)) +
  geom_line(aes(y=mean)) +
  geom_ribbon(aes(ymin=low,ymax=high),alpha=0.2) +
  theme_minimal() +
  geom_point(data=df[df$state=='--',],
             aes(x=t,y=p_clinton),
             alpha=0.5) +
  stat_smooth(data=df[df$state=='--',],
              geom='line',method='loess',span=0.1,
              aes(x=t,y=p_clinton),linetype=2)

# state averages
mu_a <- rstan::extract(out, pars = "mu_a")[[1]] 
mu_a_mean <- colMeans(inv.logit(mu_a))

mu_b <- rstan::extract(out, pars = "mu_b")[[1]] 
mu_b_means <- pblapply(1:dim(mu_b)[2],
                       function(x){
                         # pred is mu_a + mu_b for the past, just mu_b for the future
                         mu_ab <- inv.logit(lapply(1:dim(mu_b)[3],
                                                   function(y){mu_b[,,y][,x]
                                                     }) %>% do.call('cbind',.)
                                            + 
                                              cbind(mu_a, matrix(0, nrow = nrow(mu_a), ncol = dim(mu_b)[3] - ncol(mu_a) )))
                         
                         # put in tibble
                         tibble(low = apply(mu_ab,2,function(x){(quantile(x,0.05))}),
                                high = apply(mu_ab,2,function(x){(quantile(x,0.95))}),
                                mean = apply(mu_ab,2,function(x){(mean(x))}),
                                state = x) 
                         
                       }) %>% do.call('bind_rows',.)

mu_b_means$state = colnames(state_correlation)[mu_b_means$state]

mu_b_means <- mu_b_means %>%
  group_by(state) %>%
  mutate(t = row_number() + min(df$begin)) %>%
  ungroup()

mu_b_means %>%
  filter(state %in% c('FL','NC','WI','MI','PA','OH')) %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  ggplot(.,aes(x=t,col=state)) +
  geom_hline(yintercept = 0.5) +
  geom_line(aes(y=mean)) +
  geom_point(aes(y=p_clinton)) +
  geom_ribbon(aes(ymin=low,ymax=high,fill=state),col=NA,alpha=0.2) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none')

mu_b_means %>%
  filter(state %in% c('FL','NC','WI','MI','PA','OH'),
         t >= ymd('2016-09-01')) %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  ggplot(.,aes(x=t,col=state)) +
  geom_hline(yintercept = 0.5) +
  geom_line(aes(y=mean)) +
  geom_point(aes(y=p_clinton)) +
  geom_ribbon(aes(ymin=low,ymax=high,fill=state),col=NA,alpha=0.2) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none')


mu_b_means %>%
  group_by(state) %>%
  summarise(mean=last(mean)) %>%
  left_join(enframe(inv.logit(mu_b_prior),'state','prior')) %>%
  ggplot(.,aes(x=prior,y=mean,label=state)) + # plot mean state pred vs prior
  geom_abline() +
  geom_text() +
  geom_smooth(method='lm',linetype=2) +
  coord_cartesian(xlim=c(0.45,0.55),ylim=c(0.45,0.55))

# polls
pi_dem <-  rstan::extract(out, pars = "pi_democrat")[[1]] 

ggplot(df %>% cbind(pi_dem = colMeans(inv.logit(pi_dem))),
       aes(p_clinton,pi_dem)) +
  geom_abline() +
  geom_point()

# pollster effects
mu_c <- rstan::extract(out, pars = "mu_c")[[1]] 

tibble(pollster = unique(df$pollster),
       house_effect = colMeans(mu_c)) %>%
  arrange(desc(abs(house_effect))) 


