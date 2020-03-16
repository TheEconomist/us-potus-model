library(tidyverse)
library(tidycensus)

# you must have a census API key saved as a system variable
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# get list of acs variables
var_list <- tidycensus::load_variables(2017, "acs5")

# get total population for a geography
total_pop <- tidycensus::get_acs(geography = 'state',
                                 variables = "B01003_001",
                                 year = 2017,
                                 survey = 'acs5') 

# get white, non-hispanic population
white_pop <- tidycensus::get_acs(geography = 'state',
                                 variables = "B03002_003",
                                 year = 2017,
                                 survey = 'acs5') 

# get black, non-hisp population
black_pop <- tidycensus::get_acs(geography = 'state',
                                 variables = "B03002_004",
                                 year = 2017,
                                 survey = 'acs5') 

# get hispanic population
hispanic_pop <- tidycensus::get_acs(geography = 'state',
                                    variables = "B03002_012",
                                    year = 2017,
                                    survey = 'acs5') 

# other pop
other_pop <- total_pop %>%
  mutate(estimate = total_pop$estimate - (white_pop$estimate + 
                                            black_pop$estimate + 
                                            hispanic_pop$estimate) )

# combine hispanic with other
hispanic_otherpop <- hispanic_pop %>%
  mutate(estimate = estimate + other_pop$estimate)


# collect complex data, tabs resume shortly
# api request for census data
search_vars <- var_list[grepl('C1500', var_list$name),]

tidycens_data <- tidycensus::get_acs(geography = 'state',
                                     variables = search_vars$name,
                                     summary_var = 'B15002_001',
                                     year = 2017,
                                     survey = 'acs5') %>%
  left_join(search_vars %>% rename(variable = name)) %>%
  filter(!grepl('Total$|Female$|Male$', label))


race_table <- tibble(code = c('A', 'B', 'C', 'D', 'E',
                              'F', 'G', 'H', 'I'),
                     race = c('WHITE ALONE', 'BLACK OR AFRICAN AMERICAN ALONE',
                              'AMERICAN INDIAN OR ALASKAN NATIVE ALONE',
                              'ASIAN ALONE', 
                              'NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE', 
                              'SOME OTHER RACE ALONE', 'TWO OR MORE RACES',
                              'WHITE ALONE, NOT HISPANIC OR LATINO',
                              'HISPANC OR LATINO'))

tidycens_data <- tidycens_data %>%
  mutate(gender = ifelse(grepl('Male', label), 'Male', 'Female'),
         label = gsub('^Estimate.*!!', '', label),
         code = gsub('(C[0-9]+)([A-Z])(_[0-9]+.$)', 
                     '\\2', 
                     variable)) %>%
  left_join (race_table) %>%
  select(GEOID, label, gender, race, estimate:summary_moe) 

# get college percent
college_pop = tidycens_data %>% 
  filter(!race %in% c("WHITE ALONE, NOT HISPANIC OR LATINO",
                      "HISPANC OR LATINO")) %>%
  filter(label == 'Bachelor\'s degree or higher') %>%
  group_by(GEOID) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup()


# wwc stuff here:
white_noncollege_pop = tidycens_data %>% 
  filter(label != 'Bachelor\'s degree or higher' &
           #gender == 'Male' & 
           race == 'WHITE ALONE, NOT HISPANIC OR LATINO') %>%
  group_by(GEOID) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup()


# median age
median_age <- tidycensus::get_acs(geography = 'state',
                                  variables = "B01002_001",
                                  year = 2017,
                                  survey = 'acs5') 

# and employment
employment <- tidycensus::get_acs(geography = 'state',
                                  variables = c("B23025_002","B27011_008"),
                                  year = 2017,
                                  survey = 'acs5') 
employment <-   employment %>%
  dplyr::select(GEOID,variable,estimate) %>%
  spread(variable,estimate) %>%
  mutate(estimate = B27011_008 / B23025_002)


# put all together
census_estimates <- 
  total_pop %>% select(fips=GEOID,pop_total = estimate) %>%
  left_join(white_pop %>% select(fips=GEOID,white_pop = estimate)) %>%
  left_join(black_pop %>% select(fips=GEOID,black_pop = estimate)) %>%
  left_join(hispanic_otherpop %>% select(fips=GEOID,other_nonwhite = estimate)) %>%
  left_join(college_pop %>% select(fips=GEOID,college_pop = estimate)) %>%
  left_join(white_noncollege_pop %>% select(fips=GEOID,white_noncollege_pop = estimate)) %>%
  left_join(median_age %>% select(fips=GEOID,median_age = estimate)) %>%
  left_join(employment %>% select(fips=GEOID,unemployed = estimate)) 

# change to percentages
census_estimates[,3:7] <- apply(census_estimates[,3:7],
                                2,
                                function(x){x/census_estimates$pop_total})

# rename vars
names(census_estimates) <- c('state_fips','pop_total','white_pct','black_pct',
                             'hisp_other_pct','college_pct','wwc_pct',
                             'median_age','unemployed')

# add state abbreviation
fips_abb_cw <- urbnmapr::statedata %>%
  select(state_fips,state_name)
  
fips_abb_cw$state <- c(state.abb,'DC')[match(fips_abb_cw$state_name,c(state.name,'District of Columbia'))]

census_estimates <- census_estimates %>%
  left_join(fips_abb_cw %>% select(-state_name))

write_csv(census_estimates,'data/acs_2013_variables.csv')

