# State and national presidential election forecasting model

Code for a dynamic multi-level Bayesian model to predict US presidential elections, implemented in Stan.

Improving on Pierre Kremp's implementation of Drew Linzer's dynamic linear model for election forecasting [(Linzer 2013)](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf).


## TODO:

- [x] find a better prior (approval/favorability lm)
- [x] rewrite model with empirical correlation matrix
- [x] add 3rd party step 
- [ ] add correction for whether a survey weights by party/past vote or note
- [ ] optimize various error terms


## Model design

[See Kremp's original description...](http://www.slate.com/features/pkremp_forecast/report.html)

## Possible live-updating forecast on election night

[See Gelman and Silver 2020](http://www.stat.columbia.edu/~gelman/research/published/electionnight4.pdf)