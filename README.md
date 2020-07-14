State and national presidential election forecasting model
================
Last update on Tuesday July 14, 2020 at 12:40 PM EDT

Code for a dynamic multilevel Bayesian model to predict US presidential
elections. Written in R and Stan.

Improving on Pierre Kremp’s
[implementation](http://www.slate.com/features/pkremp_forecast/report.html)
of Drew Linzer’s dynamic linear model for election forecasting
[(Linzer 2013)](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf),
we (1) add corrections for partisan non-response, survey mode and survey
population; (2) use informative state-level priors that update
throughout the election year; and (3) specify empirical state-level
correlations from political and demographic variables.

You can see the model’s predictions for 2020
[here](https://projects.economist.com/us-2020-forecast/president) and
read how it works
[here](https://projects.economist.com/us-2020-forecast/president/how-this-works).

## File dictionary

In terms of useful files, you should pay attention to the 3 scripts for
the 2008, 2012 and 2016 US presidential elections are located in the
`scripts/model` directory. There are three R scripts that import data,
run models and parse results:

  - `final_model_2008.R`
  - `final_model_2012.R`
  - `final_model_2016.R`

And there are 3 different Stan scripts that will run different versions
of our polling aggregate and election forecasting model:

  - `poll_model_2020.stan` - the final model we use for the 2020
    presidential election
  - `poll_model_2020_no_partisan_correction.stan` - a model that removes
    the correction for partisan non-response bias in the polls
  - `poll_model_2020_no_mode_adjustment.stan` - a model that further
    removes the adjustments for the mode in which a survey is conducted
    (live phone, online, other) and its population (adult, likely voter,
    registered voter)

The model diagnostics displayed below are all results of the
`poll_model_2020.stan` script.

## Model performance

Here is a graphical summary of the model’s performance in 2008, 2012 and
2016.

### 2008

#### Map

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### Final electoral-college histogram

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### National and state polling averages and the electoral college “now-cast” over time

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### States’ partisan leans over time

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### Performance

| outlet               | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :------------------- | -------------: | -----------: | --------------: |
| economist (backtest) |      0.0333707 |    0.0302863 |              49 |

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## [1] 0.02242826

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| NC    | 0.503 | 0.454 | 0.555 | 0.527 | 0.031 |
| MO    | 0.510 | 0.461 | 0.561 | 0.635 | 0.030 |
| IN    | 0.486 | 0.434 | 0.537 | 0.330 | 0.031 |
| MT    | 0.483 | 0.427 | 0.534 | 0.297 | 0.031 |
| FL    | 0.519 | 0.472 | 0.569 | 0.734 | 0.030 |
| GA    | 0.476 | 0.425 | 0.528 | 0.201 | 0.031 |
| VA    | 0.527 | 0.479 | 0.574 | 0.818 | 0.028 |
| OH    | 0.527 | 0.478 | 0.576 | 0.818 | 0.029 |
| AR    | 0.472 | 0.423 | 0.524 | 0.190 | 0.031 |
| AZ    | 0.470 | 0.418 | 0.523 | 0.172 | 0.032 |
| WV    | 0.469 | 0.420 | 0.520 | 0.154 | 0.031 |
| NV    | 0.534 | 0.483 | 0.587 | 0.859 | 0.031 |
| MS    | 0.465 | 0.412 | 0.522 | 0.134 | 0.034 |
| CO    | 0.535 | 0.485 | 0.583 | 0.878 | 0.028 |
| ND    | 0.463 | 0.403 | 0.520 | 0.150 | 0.034 |
| LA    | 0.461 | 0.412 | 0.514 | 0.102 | 0.031 |
| TX    | 0.458 | 0.407 | 0.508 | 0.092 | 0.030 |
| –     | 0.544 | 0.513 | 0.575 | 0.996 | 0.018 |
| SC    | 0.455 | 0.408 | 0.508 | 0.066 | 0.032 |
| SD    | 0.453 | 0.401 | 0.506 | 0.074 | 0.031 |
| NH    | 0.552 | 0.503 | 0.602 | 0.956 | 0.030 |
| PA    | 0.557 | 0.508 | 0.606 | 0.966 | 0.029 |
| WI    | 0.559 | 0.507 | 0.606 | 0.974 | 0.028 |
| KY    | 0.440 | 0.391 | 0.491 | 0.030 | 0.031 |
| NM    | 0.560 | 0.502 | 0.614 | 0.951 | 0.032 |
| MN    | 0.561 | 0.512 | 0.611 | 0.976 | 0.030 |
| TN    | 0.438 | 0.388 | 0.491 | 0.030 | 0.032 |
| IA    | 0.564 | 0.512 | 0.612 | 0.978 | 0.029 |
| MI    | 0.565 | 0.516 | 0.612 | 0.981 | 0.028 |
| AK    | 0.427 | 0.375 | 0.479 | 0.007 | 0.031 |
| OR    | 0.575 | 0.524 | 0.624 | 0.993 | 0.030 |
| KS    | 0.424 | 0.375 | 0.475 | 0.009 | 0.030 |
| ME    | 0.587 | 0.537 | 0.635 | 0.997 | 0.029 |
| WA    | 0.587 | 0.536 | 0.636 | 0.998 | 0.030 |
| NE    | 0.412 | 0.363 | 0.462 | 0.001 | 0.029 |
| AL    | 0.407 | 0.359 | 0.460 | 0.009 | 0.031 |
| NJ    | 0.594 | 0.543 | 0.644 | 0.999 | 0.030 |
| DE    | 0.619 | 0.569 | 0.666 | 0.999 | 0.028 |
| CA    | 0.621 | 0.569 | 0.669 | 1.000 | 0.028 |
| OK    | 0.379 | 0.330 | 0.432 | 0.001 | 0.031 |
| CT    | 0.623 | 0.576 | 0.670 | 1.000 | 0.028 |
| WY    | 0.375 | 0.325 | 0.426 | 0.000 | 0.031 |
| MD    | 0.629 | 0.568 | 0.687 | 0.999 | 0.034 |
| IL    | 0.633 | 0.586 | 0.679 | 1.000 | 0.027 |
| MA    | 0.636 | 0.586 | 0.686 | 1.000 | 0.030 |
| ID    | 0.352 | 0.301 | 0.404 | 0.000 | 0.031 |
| NY    | 0.649 | 0.599 | 0.699 | 1.000 | 0.029 |
| UT    | 0.344 | 0.297 | 0.393 | 0.000 | 0.029 |
| VT    | 0.660 | 0.611 | 0.707 | 1.000 | 0.028 |
| RI    | 0.669 | 0.622 | 0.716 | 1.000 | 0.028 |
| HI    | 0.677 | 0.618 | 0.727 | 1.000 | 0.030 |
| DC    | 0.908 | 0.880 | 0.934 | 1.000 | 0.015 |

### 2012

#### Map

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Final electoral-college histogram

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

#### National and state polling averages and the electoral college “now-cast” over time

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

#### States’ partisan leans over time

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### Performance

| outlet                | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :-------------------- | -------------: | -----------: | --------------: |
| Linzer                |             NA |     0.003800 |              NA |
| Wang/Ferguson         |             NA |     0.007610 |              NA |
| Silver/538            |             NA |     0.009110 |              NA |
| Jackman/Pollster      |             NA |     0.009710 |              NA |
| Desart/Holbrook       |             NA |     0.016050 |              NA |
| economist (backtest)  |      0.0327484 |     0.021624 |              50 |
| Intrade               |             NA |     0.028120 |              NA |
| Enten/Margin of Error |             NA |     0.050750 |              NA |

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## [1] 0.02187645

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| FL    | 0.497 | 0.450 | 0.542 | 0.468 | 0.027 |
| VA    | 0.506 | 0.461 | 0.550 | 0.591 | 0.026 |
| CO    | 0.510 | 0.465 | 0.557 | 0.645 | 0.028 |
| OH    | 0.511 | 0.465 | 0.558 | 0.656 | 0.028 |
| –     | 0.512 | 0.480 | 0.542 | 0.773 | 0.018 |
| NC    | 0.488 | 0.442 | 0.534 | 0.343 | 0.027 |
| NH    | 0.517 | 0.470 | 0.564 | 0.726 | 0.028 |
| IA    | 0.518 | 0.473 | 0.561 | 0.749 | 0.026 |
| NV    | 0.521 | 0.474 | 0.568 | 0.787 | 0.028 |
| WI    | 0.524 | 0.478 | 0.569 | 0.805 | 0.027 |
| PA    | 0.531 | 0.485 | 0.577 | 0.862 | 0.027 |
| MO    | 0.463 | 0.416 | 0.510 | 0.095 | 0.028 |
| MN    | 0.538 | 0.493 | 0.582 | 0.916 | 0.027 |
| OR    | 0.539 | 0.492 | 0.585 | 0.919 | 0.027 |
| AZ    | 0.459 | 0.412 | 0.504 | 0.072 | 0.027 |
| MI    | 0.541 | 0.495 | 0.586 | 0.928 | 0.027 |
| IN    | 0.458 | 0.413 | 0.504 | 0.062 | 0.027 |
| NM    | 0.543 | 0.496 | 0.589 | 0.936 | 0.027 |
| MT    | 0.455 | 0.409 | 0.502 | 0.056 | 0.028 |
| GA    | 0.453 | 0.405 | 0.499 | 0.049 | 0.028 |
| SC    | 0.441 | 0.386 | 0.496 | 0.043 | 0.033 |
| NJ    | 0.559 | 0.510 | 0.606 | 0.975 | 0.028 |
| SD    | 0.439 | 0.392 | 0.487 | 0.016 | 0.029 |
| ME    | 0.561 | 0.515 | 0.606 | 0.982 | 0.027 |
| WA    | 0.565 | 0.519 | 0.612 | 0.987 | 0.028 |
| CT    | 0.571 | 0.523 | 0.619 | 0.993 | 0.029 |
| ND    | 0.425 | 0.380 | 0.469 | 0.005 | 0.026 |
| TN    | 0.425 | 0.378 | 0.472 | 0.004 | 0.028 |
| NE    | 0.422 | 0.375 | 0.470 | 0.003 | 0.028 |
| WV    | 0.418 | 0.368 | 0.469 | 0.002 | 0.031 |
| MS    | 0.415 | 0.351 | 0.478 | 0.015 | 0.037 |
| TX    | 0.415 | 0.373 | 0.461 | 0.002 | 0.027 |
| CA    | 0.590 | 0.547 | 0.632 | 0.998 | 0.025 |
| MA    | 0.590 | 0.541 | 0.637 | 0.996 | 0.028 |
| LA    | 0.405 | 0.359 | 0.452 | 0.002 | 0.028 |
| KY    | 0.405 | 0.354 | 0.455 | 0.000 | 0.030 |
| KS    | 0.399 | 0.342 | 0.460 | 0.005 | 0.036 |
| DE    | 0.604 | 0.549 | 0.659 | 0.997 | 0.033 |
| IL    | 0.604 | 0.556 | 0.649 | 1.000 | 0.027 |
| MD    | 0.607 | 0.559 | 0.653 | 1.000 | 0.027 |
| AL    | 0.389 | 0.342 | 0.437 | 0.000 | 0.028 |
| AR    | 0.378 | 0.333 | 0.425 | 0.000 | 0.028 |
| RI    | 0.623 | 0.573 | 0.672 | 1.000 | 0.029 |
| NY    | 0.623 | 0.576 | 0.668 | 1.000 | 0.027 |
| AK    | 0.366 | 0.306 | 0.428 | 0.001 | 0.037 |
| OK    | 0.339 | 0.290 | 0.393 | 0.000 | 0.032 |
| HI    | 0.662 | 0.615 | 0.707 | 1.000 | 0.027 |
| VT    | 0.674 | 0.622 | 0.721 | 1.000 | 0.028 |
| ID    | 0.325 | 0.276 | 0.375 | 0.000 | 0.030 |
| WY    | 0.317 | 0.250 | 0.391 | 0.000 | 0.044 |
| UT    | 0.275 | 0.231 | 0.323 | 0.000 | 0.029 |
| DC    | 0.899 | 0.848 | 0.938 | 1.000 | 0.023 |

### 2016

#### Map

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

#### Final electoral-college histogram

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

#### National and state polling averages and the electoral college “now-cast” over time

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

#### States’ partisan leans over time

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### Performance

| outlet               | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :------------------- | -------------: | -----------: | --------------: |
| economist (backtest) |       0.084973 |    0.0590491 |              47 |
| 538 polls-plus       |       0.092800 |    0.0664000 |              46 |
| 538 polls-only       |       0.093600 |    0.0672000 |              46 |
| princeton            |       0.116900 |    0.0744000 |              47 |
| nyt upshot           |       0.120800 |    0.0801000 |              46 |
| kremp/slate          |       0.121000 |    0.0766000 |              46 |
| pollsavvy            |       0.121900 |    0.0794000 |              46 |
| predictwise markets  |       0.127200 |    0.0767000 |              46 |
| predictwise overall  |       0.127600 |    0.0783000 |              46 |
| desart and holbrook  |       0.127900 |    0.0825000 |              44 |
| daily kos            |       0.143900 |    0.0864000 |              46 |
| huffpost             |       0.150500 |    0.0892000 |              46 |

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

    ## [1] 0.03065481

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| FL    | 0.501 | 0.458 | 0.545 | 0.512 | 0.026 |
| NC    | 0.496 | 0.450 | 0.543 | 0.450 | 0.028 |
| OH    | 0.492 | 0.444 | 0.539 | 0.386 | 0.028 |
| IA    | 0.490 | 0.441 | 0.542 | 0.374 | 0.030 |
| NV    | 0.514 | 0.467 | 0.560 | 0.667 | 0.028 |
| –     | 0.518 | 0.489 | 0.550 | 0.819 | 0.019 |
| PA    | 0.518 | 0.470 | 0.565 | 0.740 | 0.028 |
| NH    | 0.520 | 0.471 | 0.567 | 0.745 | 0.028 |
| CO    | 0.520 | 0.475 | 0.565 | 0.764 | 0.027 |
| AZ    | 0.474 | 0.429 | 0.519 | 0.174 | 0.026 |
| WI    | 0.527 | 0.479 | 0.575 | 0.822 | 0.029 |
| MI    | 0.527 | 0.479 | 0.573 | 0.835 | 0.028 |
| VA    | 0.528 | 0.483 | 0.573 | 0.847 | 0.027 |
| GA    | 0.472 | 0.427 | 0.517 | 0.153 | 0.026 |
| MN    | 0.534 | 0.486 | 0.581 | 0.876 | 0.028 |
| NM    | 0.540 | 0.493 | 0.589 | 0.918 | 0.029 |
| SC    | 0.456 | 0.411 | 0.501 | 0.054 | 0.027 |
| ME    | 0.550 | 0.503 | 0.598 | 0.960 | 0.029 |
| MO    | 0.446 | 0.399 | 0.492 | 0.028 | 0.027 |
| OR    | 0.555 | 0.507 | 0.600 | 0.965 | 0.027 |
| TX    | 0.442 | 0.399 | 0.486 | 0.019 | 0.026 |
| MS    | 0.441 | 0.395 | 0.485 | 0.013 | 0.026 |
| WA    | 0.572 | 0.524 | 0.618 | 0.992 | 0.028 |
| CT    | 0.572 | 0.526 | 0.618 | 0.994 | 0.028 |
| AK    | 0.425 | 0.378 | 0.471 | 0.006 | 0.027 |
| IN    | 0.425 | 0.381 | 0.471 | 0.004 | 0.027 |
| DE    | 0.576 | 0.532 | 0.623 | 0.997 | 0.028 |
| NJ    | 0.579 | 0.534 | 0.626 | 0.998 | 0.027 |
| LA    | 0.413 | 0.368 | 0.456 | 0.002 | 0.026 |
| IL    | 0.588 | 0.543 | 0.632 | 1.000 | 0.026 |
| MT    | 0.412 | 0.363 | 0.460 | 0.002 | 0.029 |
| KS    | 0.412 | 0.366 | 0.458 | 0.001 | 0.028 |
| TN    | 0.408 | 0.362 | 0.452 | 0.000 | 0.027 |
| RI    | 0.593 | 0.544 | 0.639 | 0.999 | 0.027 |
| SD    | 0.404 | 0.356 | 0.451 | 0.000 | 0.028 |
| UT    | 0.398 | 0.331 | 0.454 | 0.003 | 0.033 |
| NE    | 0.392 | 0.348 | 0.438 | 0.000 | 0.027 |
| NY    | 0.609 | 0.564 | 0.654 | 1.000 | 0.027 |
| AR    | 0.390 | 0.344 | 0.437 | 0.000 | 0.028 |
| AL    | 0.390 | 0.347 | 0.433 | 0.000 | 0.026 |
| ND    | 0.387 | 0.341 | 0.432 | 0.000 | 0.027 |
| KY    | 0.379 | 0.334 | 0.425 | 0.000 | 0.027 |
| CA    | 0.625 | 0.582 | 0.666 | 1.000 | 0.024 |
| MA    | 0.631 | 0.586 | 0.675 | 1.000 | 0.027 |
| MD    | 0.639 | 0.592 | 0.683 | 1.000 | 0.026 |
| ID    | 0.359 | 0.312 | 0.404 | 0.000 | 0.027 |
| WV    | 0.358 | 0.315 | 0.404 | 0.000 | 0.027 |
| HI    | 0.654 | 0.604 | 0.706 | 1.000 | 0.031 |
| OK    | 0.346 | 0.303 | 0.390 | 0.000 | 0.026 |
| VT    | 0.674 | 0.629 | 0.719 | 1.000 | 0.027 |
| WY    | 0.290 | 0.249 | 0.332 | 0.000 | 0.025 |
| DC    | 0.906 | 0.866 | 0.938 | 1.000 | 0.019 |

## Cumulative charts

### Calibration plot

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

# Licence

This software is published by *[The
Economist](https://www.economist.com)* under the [MIT
licence](https://opensource.org/licenses/MIT). The data generated by
*The Economist* are available under the [Creative Commons
Attribution 4.0 International
License](https://creativecommons.org/licenses/by/4.0/).

The licences include only the data and the software authored by *The
Economist*, and do not cover any *Economist* content or third-party data
or content made available using the software. More information about
licensing, syndication and the copyright of *Economist* content can be
found [here](https://www.economist.com/rights/).
