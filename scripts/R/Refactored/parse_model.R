### Extract ----
# national average
sum_average_states <- rstan::extract(out, pars = "sum_average_states")[[1]] 

tibble(low = apply(sum_average_states,2,function(x){(quantile(x,0.025))}),
       high = apply(sum_average_states,2,function(x){(quantile(x,0.975))}),
       mean = apply(sum_average_states,2,function(x){(mean(x))})) %>%
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
mu_b_means <- pblapply(1:dim(mu_b)[3],
                       function(x){
                         # pred is mu_a + mu_b for the past, just mu_b for the future
                         mu_ab <- mu_b[,,x] + cbind(mu_a, matrix(0, nrow = nrow(mu_a), ncol = ncol(mu_b[,,1]) - ncol(mu_a) ))
                         mu_ab <- inv.logit(mu_ab) 
                         
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
  left_join(df %>% select(state,t,p_clinton)) %>%
  ggplot(.,aes(x=t,col=state)) +
  geom_line(aes(y=mean)) +
  geom_point(aes(y=p_clinton)) +
  #geom_ribbon(aes(ymin=low,ymax=high),alpha=0.2) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none')

# polls
pi_dem <-  rstan::extract(out, pars = "pi_democrat")[[1]] 

ggplot(df %>% cbind(pi_dem = colMeans(inv.logit(pi_dem))),
       aes(p_clinton,pi_dem)) +
  geom_abline() +
  geom_point()

# pollster effects
mu_c <- rstan::extract(out, pars = "mu_c")[[1]] 

tibble(pollster = unique(df$pollster),
       house_effect = colMeans(mu_c)) %>%View
arrange(desc(abs(house_effect))) %>%
  ggplot(.,aes(x=house_effect)) +
  geom_histogram(binwidth=0.01)


