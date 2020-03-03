out <- readRDS("~/Downloads/stan_model_2016-11-08-3.rds")
predicted_scores <- rstan::extract(out, pars = "predicted_score")[[1]]
poll_error <- rstan::extract(out, pars = "polling_error")[[1]]
mu_a <- rstan::extract(out, pars = "mu_a")[[1]]
mu_b <- rstan::extract(out, pars = "mu_b")[[1]]

## alpha
alpha <- rstan::extract(out, pars = "alpha")[[1]]
hist(alpha)

## sigma_a
sigma_a <- rstan::extract(out, pars = "sigma_a")[[1]]
hist(sigma_a)

## sigma_b
sigma_b <- rstan::extract(out, pars = "sigma_b")[[1]]
hist(sigma_b)

## mu_c
mu_c <- rstan::extract(out, pars = "mu_c")[[1]]
mu_c <- lapply(1:ncol(mu_c),
                    function(x){
                      out <- as.data.frame(cbind(mu_c[,x], x))
                      return(out)
                    }) %>% do.call('bind_rows', .)
colnames(mu_c) <- c("val", "pollster")
mu_c %>%
  group_by(pollster) %>%
  summarize(mean= mean(val)) %>%
  ggplot(., aes(x = mean)) +
    geom_histogram()


## polling error
poll_error <- rstan::extract(out, pars = "polling_error")[[1]]
poll_error <- lapply(1:51,
                    function(x){
                      out <- as.data.frame(cbind(poll_error[,x], x))
                      return(out)
                    }) %>% do.call('bind_rows', .)
poll_error %>% 
  group_by(x) %>%
  summarize(mean(V1))


mu_a <- cbind(mu_a,
      matrix(0, nrow = nrow(mu_a), ncol = (dim(mu_b)[3] - ncol(mu_a))))

draws <- lapply(1:dim(mu_b)[2],
      function(x){
        state_mu_b_draws <- mu_b[ , x, ]
        mu_b_plus_a <- inv.logit(state_mu_b_draws + mu_a)
        mu_b_plus_a %>%
          as.data.frame() %>%
          mutate(state = x,
                 draw = row_number()) %>%
          gather(date,p_clinton,(1:(ncol(.)-2))) %>%
          mutate(date = as.numeric(gsub('V','',date)),
                 date = min(df$t) + date) %>%
          return()
        })  %>% do.call('bind_rows',.)

predictions <- draws %>%
  group_by(state,date) %>%
  summarise(p_clinton_mean = mean(p_clinton),
            p_clinton_high = quantile(p_clinton,0.975),
            p_clinton_low = quantile(p_clinton,0.025)) %>%
  ungroup()

 

state_lookup <- tibble(state = colnames(state_correlation),
                       index_s = 1:51)
 
predictions$state <- state_lookup[match(predictions$state,state_lookup$index_s),]$state

 


plt <- 
  ggplot(predictions,aes(x=date,col=state,group=state)) +
  geom_hline(yintercept = 0.5) +
  geom_line(data = predictions, aes(y=p_clinton_mean)) +
  geom_ribbon(data = predictions, aes(ymin=p_clinton_low,ymax=p_clinton_high,fill=state),col=NA,alpha=0.2) +
  scale_x_date(date_breaks = '1 month',date_labels = '%b',limits = c(ymd('2016-05-01'),ymd('2016-11-08'))) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), legend.position = "none" ) +
  facet_wrap(~state) +
  geom_point(data = df, aes(x = t, y = p_clinton), size = 0.5)








