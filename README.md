State and national presidential election forecasting model
================
Last update on Wednesday August 05, 2020 at 09:53 AM EDT

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
  - `poll_model_2020_no_mode_adjustment.stan` - a model that removes the
    correction for partisan non-response bias in the polls and the
    adjustments for the mode in which a survey is conducted (live phone,
    online, other) and its population (adult, likely voter, registered
    voter)

## Model performance

Here is a graphical summary of the model’s performance in 2008, 2012 and
2016.

### 2008

#### Map

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

#### Final electoral college histogram

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### State vs national deltas over time

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### Performance

| outlet               | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :------------------- | -------------: | -----------: | --------------: |
| economist (backtest) |      0.0327228 |    0.0290726 |              49 |

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## [1] 0.02310776

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| NC    | 0.501 | 0.471 | 0.533 | 0.525 | 0.019 |
| MO    | 0.509 | 0.479 | 0.539 | 0.693 | 0.018 |
| FL    | 0.513 | 0.483 | 0.545 | 0.774 | 0.019 |
| IN    | 0.483 | 0.454 | 0.513 | 0.178 | 0.018 |
| AR    | 0.477 | 0.443 | 0.508 | 0.112 | 0.019 |
| OH    | 0.524 | 0.495 | 0.554 | 0.908 | 0.018 |
| VA    | 0.526 | 0.495 | 0.557 | 0.922 | 0.018 |
| MT    | 0.474 | 0.445 | 0.505 | 0.082 | 0.018 |
| GA    | 0.473 | 0.441 | 0.504 | 0.078 | 0.019 |
| NV    | 0.531 | 0.500 | 0.562 | 0.949 | 0.018 |
| WV    | 0.469 | 0.439 | 0.501 | 0.053 | 0.019 |
| AZ    | 0.468 | 0.437 | 0.500 | 0.048 | 0.019 |
| CO    | 0.533 | 0.502 | 0.564 | 0.956 | 0.018 |
| LA    | 0.460 | 0.425 | 0.494 | 0.026 | 0.021 |
| –     | 0.540 | 0.520 | 0.557 | 1.000 | 0.011 |
| MS    | 0.456 | 0.423 | 0.490 | 0.019 | 0.020 |
| TX    | 0.453 | 0.418 | 0.488 | 0.015 | 0.020 |
| SD    | 0.452 | 0.421 | 0.483 | 0.005 | 0.019 |
| NH    | 0.550 | 0.519 | 0.580 | 0.996 | 0.018 |
| SC    | 0.449 | 0.416 | 0.482 | 0.005 | 0.020 |
| ND    | 0.448 | 0.417 | 0.480 | 0.004 | 0.019 |
| PA    | 0.553 | 0.524 | 0.583 | 0.998 | 0.018 |
| TN    | 0.444 | 0.414 | 0.476 | 0.002 | 0.019 |
| WI    | 0.557 | 0.526 | 0.586 | 0.999 | 0.017 |
| KY    | 0.441 | 0.412 | 0.472 | 0.001 | 0.018 |
| MN    | 0.559 | 0.528 | 0.588 | 0.999 | 0.017 |
| NM    | 0.560 | 0.527 | 0.592 | 0.997 | 0.019 |
| IA    | 0.561 | 0.531 | 0.591 | 1.000 | 0.018 |
| MI    | 0.564 | 0.534 | 0.593 | 1.000 | 0.017 |
| OR    | 0.571 | 0.542 | 0.600 | 1.000 | 0.017 |
| KS    | 0.423 | 0.393 | 0.454 | 0.000 | 0.018 |
| ME    | 0.583 | 0.552 | 0.614 | 1.000 | 0.018 |
| WA    | 0.584 | 0.553 | 0.613 | 1.000 | 0.018 |
| AK    | 0.416 | 0.383 | 0.449 | 0.000 | 0.020 |
| NJ    | 0.590 | 0.559 | 0.620 | 1.000 | 0.018 |
| AL    | 0.405 | 0.373 | 0.438 | 0.000 | 0.020 |
| NE    | 0.402 | 0.370 | 0.434 | 0.000 | 0.019 |
| DE    | 0.615 | 0.582 | 0.648 | 1.000 | 0.020 |
| CA    | 0.616 | 0.584 | 0.648 | 1.000 | 0.019 |
| CT    | 0.618 | 0.586 | 0.649 | 1.000 | 0.018 |
| MD    | 0.618 | 0.580 | 0.654 | 1.000 | 0.021 |
| OK    | 0.378 | 0.347 | 0.411 | 0.000 | 0.020 |
| IL    | 0.629 | 0.597 | 0.660 | 1.000 | 0.018 |
| WY    | 0.366 | 0.335 | 0.396 | 0.000 | 0.018 |
| MA    | 0.642 | 0.611 | 0.673 | 1.000 | 0.018 |
| NY    | 0.646 | 0.617 | 0.675 | 1.000 | 0.017 |
| ID    | 0.354 | 0.324 | 0.383 | 0.000 | 0.018 |
| VT    | 0.654 | 0.623 | 0.683 | 1.000 | 0.018 |
| UT    | 0.342 | 0.312 | 0.373 | 0.000 | 0.018 |
| HI    | 0.662 | 0.628 | 0.697 | 1.000 | 0.021 |
| RI    | 0.670 | 0.639 | 0.698 | 1.000 | 0.017 |
| DC    | 0.933 | 0.920 | 0.944 | 1.000 | 0.007 |

### 2012

#### Map

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Final electoral college histogram

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

#### State vs national deltas over time

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### Performance

| outlet                | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :-------------------- | -------------: | -----------: | --------------: |
| Linzer                |             NA |    0.0038000 |              NA |
| Wang/Ferguson         |             NA |    0.0076100 |              NA |
| Silver/538            |             NA |    0.0091100 |              NA |
| Jackman/Pollster      |             NA |    0.0097100 |              NA |
| Desart/Holbrook       |             NA |    0.0160500 |              NA |
| economist (backtest)  |      0.0320587 |    0.0188167 |              50 |
| Intrade               |             NA |    0.0281200 |              NA |
| Enten/Margin of Error |             NA |    0.0507500 |              NA |

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## [1] 0.02241299

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| VA    | 0.504 | 0.475 | 0.533 | 0.589 | 0.017 |
| FL    | 0.495 | 0.466 | 0.525 | 0.391 | 0.018 |
| CO    | 0.506 | 0.476 | 0.535 | 0.620 | 0.017 |
| –     | 0.509 | 0.490 | 0.529 | 0.785 | 0.012 |
| OH    | 0.510 | 0.482 | 0.539 | 0.721 | 0.017 |
| NH    | 0.513 | 0.483 | 0.543 | 0.768 | 0.018 |
| NC    | 0.486 | 0.457 | 0.515 | 0.212 | 0.017 |
| IA    | 0.515 | 0.486 | 0.544 | 0.793 | 0.017 |
| NV    | 0.516 | 0.486 | 0.547 | 0.807 | 0.018 |
| WI    | 0.521 | 0.491 | 0.551 | 0.884 | 0.018 |
| PA    | 0.528 | 0.498 | 0.558 | 0.936 | 0.018 |
| MN    | 0.534 | 0.504 | 0.564 | 0.971 | 0.018 |
| MI    | 0.538 | 0.509 | 0.568 | 0.983 | 0.018 |
| OR    | 0.539 | 0.508 | 0.571 | 0.979 | 0.019 |
| MO    | 0.460 | 0.430 | 0.491 | 0.014 | 0.018 |
| NM    | 0.540 | 0.509 | 0.572 | 0.983 | 0.019 |
| IN    | 0.456 | 0.426 | 0.487 | 0.009 | 0.018 |
| MT    | 0.453 | 0.423 | 0.484 | 0.004 | 0.018 |
| AZ    | 0.453 | 0.421 | 0.483 | 0.006 | 0.018 |
| GA    | 0.452 | 0.420 | 0.484 | 0.006 | 0.019 |
| NJ    | 0.556 | 0.525 | 0.587 | 0.999 | 0.018 |
| ME    | 0.558 | 0.527 | 0.589 | 0.999 | 0.018 |
| SC    | 0.439 | 0.401 | 0.475 | 0.004 | 0.022 |
| WA    | 0.562 | 0.531 | 0.591 | 1.000 | 0.017 |
| CT    | 0.567 | 0.537 | 0.596 | 1.000 | 0.017 |
| SD    | 0.431 | 0.398 | 0.465 | 0.000 | 0.020 |
| ND    | 0.423 | 0.393 | 0.454 | 0.000 | 0.019 |
| MS    | 0.420 | 0.382 | 0.458 | 0.000 | 0.023 |
| TN    | 0.419 | 0.388 | 0.450 | 0.000 | 0.019 |
| WV    | 0.416 | 0.384 | 0.452 | 0.000 | 0.021 |
| CA    | 0.585 | 0.555 | 0.615 | 1.000 | 0.018 |
| MA    | 0.588 | 0.559 | 0.617 | 1.000 | 0.017 |
| TX    | 0.409 | 0.378 | 0.442 | 0.000 | 0.019 |
| NE    | 0.408 | 0.377 | 0.438 | 0.000 | 0.018 |
| LA    | 0.401 | 0.368 | 0.434 | 0.000 | 0.020 |
| IL    | 0.600 | 0.569 | 0.629 | 1.000 | 0.018 |
| KY    | 0.399 | 0.366 | 0.434 | 0.000 | 0.020 |
| DE    | 0.602 | 0.565 | 0.638 | 1.000 | 0.021 |
| KS    | 0.396 | 0.360 | 0.431 | 0.000 | 0.021 |
| MD    | 0.607 | 0.573 | 0.639 | 1.000 | 0.019 |
| RI    | 0.616 | 0.584 | 0.648 | 1.000 | 0.019 |
| AR    | 0.384 | 0.352 | 0.418 | 0.000 | 0.020 |
| AL    | 0.383 | 0.350 | 0.415 | 0.000 | 0.020 |
| NY    | 0.620 | 0.590 | 0.649 | 1.000 | 0.017 |
| AK    | 0.364 | 0.326 | 0.401 | 0.000 | 0.022 |
| VT    | 0.660 | 0.628 | 0.693 | 1.000 | 0.019 |
| HI    | 0.661 | 0.630 | 0.692 | 1.000 | 0.019 |
| ID    | 0.332 | 0.301 | 0.364 | 0.000 | 0.019 |
| OK    | 0.331 | 0.298 | 0.363 | 0.000 | 0.019 |
| WY    | 0.313 | 0.281 | 0.347 | 0.000 | 0.020 |
| UT    | 0.291 | 0.261 | 0.320 | 0.000 | 0.018 |
| DC    | 0.903 | 0.884 | 0.920 | 1.000 | 0.010 |

### 2016

#### Map

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

#### Final electoral college histogram

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

#### State vs national deltas over time

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

#### Model results vs polls vs the prior

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

#### Performance

| outlet               | ev\_wtd\_brier | unwtd\_brier | states\_correct |
| :------------------- | -------------: | -----------: | --------------: |
| economist (backtest) |      0.0746651 |    0.0520891 |              48 |
| 538 polls-plus       |      0.0928000 |    0.0664000 |              46 |
| 538 polls-only       |      0.0936000 |    0.0672000 |              46 |
| princeton            |      0.1169000 |    0.0744000 |              47 |
| nyt upshot           |      0.1208000 |    0.0801000 |              46 |
| kremp/slate          |      0.1210000 |    0.0766000 |              46 |
| pollsavvy            |      0.1219000 |    0.0794000 |              46 |
| predictwise markets  |      0.1272000 |    0.0767000 |              46 |
| predictwise overall  |      0.1276000 |    0.0783000 |              46 |
| desart and holbrook  |      0.1279000 |    0.0825000 |              44 |
| daily kos            |      0.1439000 |    0.0864000 |              46 |
| huffpost             |      0.1505000 |    0.0892000 |              46 |

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

    ## [1] 0.0274812

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| FL    | 0.497 | 0.458 | 0.536 | 0.436 | 0.023 |
| NC    | 0.493 | 0.453 | 0.534 | 0.367 | 0.024 |
| NV    | 0.509 | 0.468 | 0.549 | 0.667 | 0.023 |
| –     | 0.514 | 0.483 | 0.543 | 0.831 | 0.017 |
| PA    | 0.514 | 0.475 | 0.554 | 0.757 | 0.024 |
| NH    | 0.514 | 0.474 | 0.555 | 0.746 | 0.024 |
| OH    | 0.485 | 0.446 | 0.524 | 0.233 | 0.023 |
| CO    | 0.516 | 0.476 | 0.556 | 0.773 | 0.024 |
| MI    | 0.519 | 0.479 | 0.558 | 0.821 | 0.023 |
| WI    | 0.521 | 0.481 | 0.559 | 0.860 | 0.022 |
| IA    | 0.479 | 0.438 | 0.518 | 0.147 | 0.023 |
| VA    | 0.523 | 0.482 | 0.564 | 0.862 | 0.025 |
| MN    | 0.528 | 0.488 | 0.568 | 0.919 | 0.024 |
| AZ    | 0.471 | 0.431 | 0.511 | 0.081 | 0.024 |
| GA    | 0.470 | 0.431 | 0.511 | 0.076 | 0.024 |
| NM    | 0.533 | 0.492 | 0.576 | 0.941 | 0.025 |
| ME    | 0.542 | 0.501 | 0.583 | 0.977 | 0.024 |
| SC    | 0.452 | 0.411 | 0.493 | 0.011 | 0.024 |
| OR    | 0.549 | 0.509 | 0.589 | 0.990 | 0.024 |
| TX    | 0.443 | 0.403 | 0.485 | 0.002 | 0.025 |
| MO    | 0.440 | 0.402 | 0.480 | 0.001 | 0.024 |
| MS    | 0.437 | 0.396 | 0.478 | 0.001 | 0.025 |
| CT    | 0.565 | 0.524 | 0.606 | 0.999 | 0.025 |
| WA    | 0.568 | 0.528 | 0.608 | 1.000 | 0.024 |
| DE    | 0.569 | 0.527 | 0.612 | 1.000 | 0.026 |
| AK    | 0.425 | 0.384 | 0.468 | 0.000 | 0.026 |
| NJ    | 0.578 | 0.538 | 0.619 | 1.000 | 0.025 |
| IN    | 0.419 | 0.379 | 0.458 | 0.000 | 0.023 |
| IL    | 0.584 | 0.543 | 0.625 | 1.000 | 0.025 |
| LA    | 0.411 | 0.373 | 0.451 | 0.000 | 0.024 |
| MT    | 0.406 | 0.367 | 0.445 | 0.000 | 0.023 |
| RI    | 0.596 | 0.553 | 0.637 | 1.000 | 0.025 |
| TN    | 0.404 | 0.365 | 0.443 | 0.000 | 0.023 |
| KS    | 0.404 | 0.365 | 0.442 | 0.000 | 0.023 |
| SD    | 0.399 | 0.360 | 0.437 | 0.000 | 0.023 |
| ND    | 0.390 | 0.351 | 0.429 | 0.000 | 0.023 |
| NY    | 0.611 | 0.570 | 0.651 | 1.000 | 0.024 |
| NE    | 0.388 | 0.351 | 0.427 | 0.000 | 0.023 |
| AL    | 0.385 | 0.346 | 0.425 | 0.000 | 0.024 |
| AR    | 0.382 | 0.344 | 0.423 | 0.000 | 0.024 |
| CA    | 0.621 | 0.582 | 0.658 | 1.000 | 0.022 |
| UT    | 0.375 | 0.338 | 0.414 | 0.000 | 0.023 |
| KY    | 0.374 | 0.337 | 0.412 | 0.000 | 0.023 |
| MA    | 0.629 | 0.590 | 0.668 | 1.000 | 0.023 |
| MD    | 0.639 | 0.598 | 0.681 | 1.000 | 0.025 |
| WV    | 0.353 | 0.316 | 0.391 | 0.000 | 0.023 |
| ID    | 0.349 | 0.312 | 0.386 | 0.000 | 0.023 |
| OK    | 0.342 | 0.305 | 0.381 | 0.000 | 0.023 |
| VT    | 0.659 | 0.618 | 0.696 | 1.000 | 0.022 |
| HI    | 0.662 | 0.622 | 0.700 | 1.000 | 0.023 |
| WY    | 0.289 | 0.255 | 0.324 | 0.000 | 0.021 |
| DC    | 0.908 | 0.886 | 0.928 | 1.000 | 0.012 |

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
