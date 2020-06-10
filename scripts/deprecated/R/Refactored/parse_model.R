models <- list.files('models/',full.names = T)

model_runs <- pblapply(models,
                       function(x){
                         # model data
                         model_date <- substr(x,20,29)
                         model <- read_rds(x)
                         
                         predicted_score <- rstan::extract(model, pars = "predicted_score")[[1]]
                         
                         state_preds <- pblapply(1:dim(predicted_score)[3],
                                                 function(y){
                                                   # pred is mu_a + mu_b for the past, just mu_b for the future
                                                   p_clinton <- predicted_score[,,y]
                                                   
                                                   # forecast on elec day
                                                   p_clinton <- p_clinton[,ncol(p_clinton)]
                                                   
                                                   # put in tibble
                                                   tibble(low = quantile(p_clinton,0.05),
                                                          high = quantile(p_clinton,0.95),
                                                          mean = mean(p_clinton),
                                                          prob = mean(p_clinton>0.50),
                                                          state = y) 
                                                   
                                                 }) %>% do.call('bind_rows',.)
                         
                         state_preds$state = colnames(state_correlation)[state_preds$state]
                         
                         # together
                         # national vote = vote * state weights
                         state_preds <- state_preds %>%
                           bind_rows(
                             state_preds %>% 
                               left_join(enframe(state_weights,'state','weight')) %>%
                               summarise(mean = weighted.mean(mean,weight),
                                         high = weighted.mean(high,weight),
                                         low = weighted.mean(low,weight)) %>%
                               mutate(state='--')
                           )
                         
                         
                         # electoral college by simulation
                         draws <- pblapply(1:dim(predicted_score)[3],
                                           function(y){
                                             # pred is mu_a + mu_b for the past, just mu_b for the future
                                             p_clinton <- predicted_score[,,y]
                                             
                                             # forecast on elec day
                                             p_clinton <- p_clinton[,ncol(p_clinton)]
                                             
                                             p_clinton <- p_clinton %>%
                                               as.data.frame() %>%
                                               mutate(draw = row_number()) %>%
                                               mutate(state = colnames(state_correlation)[y]) %>%
                                               set_names(.,c('p_clinton','draw','state'))
                                             
                                             
                                           }) %>% do.call('bind_rows',.)
                         
                         sim_evs <- draws %>%
                           left_join(states2012 %>% select(state,ev),by='state') %>%
                           group_by(draw) %>%
                           summarise(dem_ev = sum(ev * (p_clinton > 0.5))) %>%
                           summarise(mean_dem_ev = mean(dem_ev),
                                     high_dem_ev = quantile(dem_ev,0.975),
                                     low_dem_ev = quantile(dem_ev,0.025),
                                     prob = mean(dem_ev >= 270))
                         
                         return(list(state_preds %>% mutate(model_date),
                                     sim_evs %>% mutate(model_date) ))
                         
                       })

# probs v other forecasters
map_df(model_runs,function(x){x[[2]]}) %>%
  ggplot(., aes(x=ymd(model_date))) +
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

