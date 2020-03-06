# State and national POTUS polling model

Code for a dynamic multi-level Bayesian model to predict US presidential elections, implemented in Stan.

Improving on Kremp/Gelman's implementation of Linzer's dynamic linear model.


## TODO:

- [x] find a better prior (approval/favorability lm)
- [x] rewrite model with correlation matrix
- [x] add 3rd party step 
- [ ] add correction for partisan survey composition
- [ ] optimize various error terms


## Model design

[See Kremp's original description...](http://www.slate.com/features/pkremp_forecast/report.html)