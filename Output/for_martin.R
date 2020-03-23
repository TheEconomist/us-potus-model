# 1. ----------------------------------------------------------------------
# CSV of national popular vote over time
# CSV of national polls
# plot of national popular vote over time with the polls


out <- read_rds(sprintf('models/stan_model_%s.rds',RUN_DATE))


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

ex_states <- c('NC','FL','OH','MI','WI','PA','VA','NV','NH')

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



p_clinton %>%
  filter(state == '--') %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = national_mu_prior,linetype=2) +
  geom_point(aes(y=p_clinton),alpha=0.2) +
  #geom_smooth(aes(y=p_clinton),method='loess',span=0.2,col='black',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.02)) +
  labs(subtitle='p_clinton national')

ggsave('output/national_vote_overtime.png')


p_clinton %>%
  filter(state == '--') %>%
  write_csv('Output/national_polling_average.csv')
  
df %>% select(state,t,p_clinton) %>%
  write_csv("Output/national_polls.csv")
  


# 2.  ---------------------------------------------------------------------
# CSV of electoral votes over time
# plot of electoral votes over time

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
            median_dem_ev = median(dem_ev),
            high_dem_ev = quantile(dem_ev,0.975),
            low_dem_ev = quantile(dem_ev,0.025),
            prob = mean(dem_ev >= 270)) %>%
  left_join(p_clinton[p_clinton$state != '--',] %>%
              left_join(states2012 %>% select(state,ev),by='state') %>%
              group_by(t) %>%
              summarise(sum_dem_ev = sum(ev * (prob > 0.5))) )


ggplot(sim_evs, aes(x=t)) +
  geom_hline(yintercept = 270) +
  geom_line(aes(y=mean_dem_ev)) +
  geom_line(aes(y=median_dem_ev),linetype=2) +
  geom_ribbon(aes(ymin=low_dem_ev,ymax=high_dem_ev),alpha=0.2) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,600,100)) +
  labs(subtitletitle='clinton evs')

ggsave('output/electoral_votes_overtime.png')


sim_evs %>% select(-prob) %>%
  write_csv('Output/electoral_votes_overtime.csv')



# 3. ----------------------------------------------------------------------
# CSV of national forecast probability over time
# ggplot of national forecast probability over time
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
             aes(yintercept=prob,col=forecaster),linetype=2)

ggsave('output/national_forecast_probability_overtime.png')

sim_evs %>%
  select(t,prob) %>%
  write_csv("Output/national_forecast_probability_overtime.csv")



# 4. ----------------------------------------------------------------------

# CSV of florida popular vote over time
# CSV of florida polls
# ggplot of florida popular vote over time with polls

p_clinton %>%
  filter(state == 'FL') %>%
  left_join(df %>% select(state,t,p_clinton)) %>% # plot over time
  # plot
  ggplot(.,aes(x=t)) +
  geom_ribbon(aes(ymin=low,ymax=high),col=NA,alpha=0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = inv.logit(mu_b_prior[names(mu_b_prior) == 'FL']),linetype=2) +
  geom_point(aes(y=p_clinton),alpha=0.2) +
  #geom_smooth(aes(y=p_clinton),method='loess',span=0.2,col='black',linetype=2,se=F) +
  geom_line(aes(y=mean)) +
  facet_wrap(~state) +
  theme_minimal()  +
  theme(legend.position = 'none') +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.02)) +
  labs(subtitle='p_clinton fl')

ggsave('output/florida_vote_overtime.png')


p_clinton %>%
  filter(state == 'FL') %>%
  write_csv('Output/floridal_polling_average.csv')

df %>% 
  filter(state == 'FL') %>%
  select(state,t,p_clinton) %>%
  write_csv("Output/florida_polls.csv")


# fl probability over time ------------------------------------------------

# CSV of florida forecast probability over time
# ggplot of florida forecast probability over time

p_clinton %>%
  filter(state=='FL') %>%
  ggplot(.,aes(x=t,y=prob)) +
  geom_line() +
  scale_x_date(limits=c(ymd('2016-03-01','2016-11-08')),date_breaks='1 month',date_labels='%b') +
  scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1)) +
  geom_hline(yintercept = 0.5) +
  theme_minimal()  +
  theme(legend.position = 'none') 

ggsave('output/florida_forecast_probability_overtime.png')


p_clinton %>%
  filter(state=='FL') %>%
  select(t,prob) %>%
  write_csv("Output/florida_forecast_probability_overtime.csv")




