mu_c_2016 <- read_csv('output/mu_c_draws_2016.csv')
mu_c_2012 <- read_csv('output/mu_c_draws_2012.csv')
mu_c_2008 <- read_csv('output/mu_c_draws_2008.csv')

mu_c_draws <- mu_c_2008 %>%
  filter(type=='posterior') %>%
  mutate(type = 2008) %>%
  bind_rows(mu_c_2012 %>%
              filter(type=='posterior') %>%
              mutate(type = 2012)) %>%
  bind_rows(mu_c_2016 %>%
              filter(type=='posterior') %>%
              mutate(type = 2016)) %>%
  mutate(type = factor(type))

mu_c_draws %>% 
  arrange(mean) %>% 
  group_by(pollster) %>%
  filter(n()==3) %>%
  ggplot(.) +
  geom_point(aes(y = mean, x = reorder(pollster, mean), color = type), 
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = low, ymax = high, x = pollster, color = type), 
                width = 0, position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw()
