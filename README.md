# State and national presidential election forecasting model

Code for a dynamic multi-level Bayesian model to predict US presidential elections, implemented in Stan.

Improving on Pierre Kremp's [implementation](http://www.slate.com/features/pkremp_forecast/report.html) of Drew Linzer's dynamic linear model for election forecasting [(Linzer 2013)](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf), we add a correction for partisan non-response in national polls and an informative state-level prior that updates throughought the election year.


## Model design and performance


### Endmatter

* Possible live-updating forecast on election night: [See Gelman and Silver 2010](http://www.stat.columbia.edu/~gelman/research/published/electionnight4.pdf)
