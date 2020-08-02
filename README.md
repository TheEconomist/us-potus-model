State and national presidential election forecasting model
================
Last update on Sunday August 02, 2020 at 06:21 PM EDT

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
| economist (backtest) |      0.0291422 |    0.0251644 |              49 |

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## [1] 0.02046181

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| NC    | 0.504 | 0.469 | 0.539 | 0.568 | 0.021 |
| MO    | 0.507 | 0.472 | 0.543 | 0.627 | 0.021 |
| IN    | 0.488 | 0.454 | 0.523 | 0.294 | 0.021 |
| FL    | 0.513 | 0.478 | 0.549 | 0.725 | 0.021 |
| MT    | 0.482 | 0.446 | 0.517 | 0.198 | 0.021 |
| OH    | 0.522 | 0.489 | 0.556 | 0.844 | 0.020 |
| GA    | 0.476 | 0.440 | 0.514 | 0.142 | 0.022 |
| VA    | 0.529 | 0.493 | 0.564 | 0.906 | 0.021 |
| AZ    | 0.470 | 0.431 | 0.507 | 0.098 | 0.022 |
| AR    | 0.468 | 0.430 | 0.506 | 0.086 | 0.023 |
| NV    | 0.532 | 0.496 | 0.568 | 0.929 | 0.021 |
| WV    | 0.464 | 0.427 | 0.500 | 0.048 | 0.022 |
| CO    | 0.536 | 0.500 | 0.571 | 0.953 | 0.021 |
| ND    | 0.462 | 0.422 | 0.502 | 0.061 | 0.024 |
| –     | 0.539 | 0.517 | 0.562 | 0.992 | 0.013 |
| MS    | 0.459 | 0.418 | 0.502 | 0.055 | 0.025 |
| LA    | 0.455 | 0.415 | 0.495 | 0.032 | 0.024 |
| SD    | 0.454 | 0.419 | 0.490 | 0.025 | 0.021 |
| SC    | 0.451 | 0.414 | 0.489 | 0.016 | 0.023 |
| NH    | 0.551 | 0.516 | 0.586 | 0.991 | 0.021 |
| PA    | 0.552 | 0.516 | 0.586 | 0.993 | 0.020 |
| TX    | 0.446 | 0.407 | 0.486 | 0.015 | 0.023 |
| MN    | 0.558 | 0.523 | 0.592 | 0.998 | 0.021 |
| WI    | 0.559 | 0.522 | 0.593 | 0.997 | 0.021 |
| NM    | 0.561 | 0.525 | 0.596 | 0.997 | 0.021 |
| KY    | 0.438 | 0.403 | 0.474 | 0.004 | 0.021 |
| MI    | 0.563 | 0.528 | 0.598 | 0.999 | 0.020 |
| IA    | 0.564 | 0.528 | 0.599 | 0.998 | 0.021 |
| TN    | 0.432 | 0.394 | 0.470 | 0.002 | 0.023 |
| AK    | 0.426 | 0.386 | 0.466 | 0.002 | 0.024 |
| OR    | 0.575 | 0.537 | 0.610 | 0.999 | 0.021 |
| KS    | 0.420 | 0.383 | 0.456 | 0.000 | 0.021 |
| ME    | 0.583 | 0.546 | 0.619 | 1.000 | 0.022 |
| WA    | 0.585 | 0.550 | 0.619 | 1.000 | 0.020 |
| NJ    | 0.591 | 0.554 | 0.628 | 1.000 | 0.022 |
| NE    | 0.408 | 0.369 | 0.446 | 0.000 | 0.023 |
| AL    | 0.400 | 0.359 | 0.440 | 0.000 | 0.024 |
| CT    | 0.618 | 0.582 | 0.654 | 1.000 | 0.022 |
| DE    | 0.619 | 0.582 | 0.657 | 1.000 | 0.022 |
| CA    | 0.620 | 0.586 | 0.654 | 1.000 | 0.020 |
| MD    | 0.620 | 0.574 | 0.664 | 1.000 | 0.026 |
| IL    | 0.628 | 0.593 | 0.663 | 1.000 | 0.021 |
| OK    | 0.371 | 0.335 | 0.409 | 0.000 | 0.023 |
| WY    | 0.368 | 0.332 | 0.404 | 0.000 | 0.021 |
| MA    | 0.635 | 0.598 | 0.672 | 1.000 | 0.022 |
| NY    | 0.648 | 0.612 | 0.682 | 1.000 | 0.020 |
| ID    | 0.352 | 0.316 | 0.389 | 0.000 | 0.022 |
| UT    | 0.352 | 0.315 | 0.391 | 0.000 | 0.023 |
| VT    | 0.658 | 0.621 | 0.696 | 1.000 | 0.023 |
| RI    | 0.665 | 0.627 | 0.701 | 1.000 | 0.021 |
| HI    | 0.679 | 0.638 | 0.718 | 1.000 | 0.023 |
| DC    | 0.919 | 0.898 | 0.937 | 1.000 | 0.011 |

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
| economist (backtest)  |      0.0323689 |    0.0168787 |              50 |
| Intrade               |             NA |    0.0281200 |              NA |
| Enten/Margin of Error |             NA |    0.0507500 |              NA |

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    ## [1] 0.02332798

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| VA    | 0.504 | 0.476 | 0.532 | 0.595 | 0.017 |
| FL    | 0.494 | 0.466 | 0.523 | 0.360 | 0.017 |
| OH    | 0.508 | 0.480 | 0.535 | 0.681 | 0.016 |
| CO    | 0.509 | 0.480 | 0.537 | 0.720 | 0.016 |
| –     | 0.510 | 0.490 | 0.532 | 0.833 | 0.013 |
| NH    | 0.516 | 0.487 | 0.543 | 0.820 | 0.016 |
| IA    | 0.517 | 0.488 | 0.544 | 0.843 | 0.016 |
| NC    | 0.483 | 0.455 | 0.511 | 0.157 | 0.016 |
| NV    | 0.521 | 0.491 | 0.550 | 0.882 | 0.017 |
| WI    | 0.523 | 0.496 | 0.551 | 0.920 | 0.016 |
| PA    | 0.528 | 0.500 | 0.556 | 0.950 | 0.016 |
| MN    | 0.533 | 0.505 | 0.561 | 0.967 | 0.017 |
| MO    | 0.465 | 0.436 | 0.493 | 0.016 | 0.016 |
| IN    | 0.461 | 0.432 | 0.488 | 0.011 | 0.016 |
| NM    | 0.540 | 0.508 | 0.570 | 0.981 | 0.018 |
| MI    | 0.542 | 0.513 | 0.571 | 0.990 | 0.017 |
| OR    | 0.544 | 0.513 | 0.573 | 0.992 | 0.017 |
| AZ    | 0.455 | 0.425 | 0.485 | 0.008 | 0.018 |
| MT    | 0.454 | 0.424 | 0.484 | 0.002 | 0.017 |
| GA    | 0.449 | 0.419 | 0.479 | 0.002 | 0.018 |
| NJ    | 0.557 | 0.527 | 0.587 | 0.997 | 0.018 |
| ME    | 0.559 | 0.530 | 0.587 | 1.000 | 0.017 |
| WA    | 0.561 | 0.532 | 0.589 | 1.000 | 0.017 |
| SC    | 0.437 | 0.405 | 0.468 | 0.000 | 0.018 |
| SD    | 0.431 | 0.404 | 0.460 | 0.000 | 0.017 |
| CT    | 0.572 | 0.543 | 0.601 | 1.000 | 0.017 |
| ND    | 0.423 | 0.394 | 0.452 | 0.000 | 0.017 |
| WV    | 0.420 | 0.389 | 0.450 | 0.000 | 0.018 |
| TN    | 0.418 | 0.390 | 0.446 | 0.000 | 0.017 |
| MA    | 0.583 | 0.554 | 0.612 | 1.000 | 0.018 |
| TX    | 0.415 | 0.384 | 0.444 | 0.000 | 0.017 |
| CA    | 0.586 | 0.557 | 0.614 | 1.000 | 0.017 |
| MS    | 0.411 | 0.379 | 0.442 | 0.000 | 0.019 |
| NE    | 0.403 | 0.375 | 0.433 | 0.000 | 0.018 |
| KY    | 0.402 | 0.373 | 0.431 | 0.000 | 0.017 |
| LA    | 0.400 | 0.370 | 0.429 | 0.000 | 0.018 |
| DE    | 0.602 | 0.568 | 0.634 | 1.000 | 0.019 |
| KS    | 0.398 | 0.368 | 0.428 | 0.000 | 0.018 |
| MD    | 0.603 | 0.574 | 0.634 | 1.000 | 0.018 |
| IL    | 0.604 | 0.574 | 0.631 | 1.000 | 0.016 |
| AR    | 0.389 | 0.360 | 0.418 | 0.000 | 0.017 |
| RI    | 0.615 | 0.584 | 0.646 | 1.000 | 0.018 |
| NY    | 0.618 | 0.591 | 0.646 | 1.000 | 0.017 |
| AL    | 0.380 | 0.351 | 0.408 | 0.000 | 0.017 |
| AK    | 0.366 | 0.332 | 0.401 | 0.000 | 0.021 |
| VT    | 0.654 | 0.624 | 0.683 | 1.000 | 0.017 |
| ID    | 0.339 | 0.311 | 0.366 | 0.000 | 0.016 |
| HI    | 0.664 | 0.633 | 0.692 | 1.000 | 0.016 |
| OK    | 0.332 | 0.305 | 0.364 | 0.000 | 0.018 |
| WY    | 0.315 | 0.289 | 0.342 | 0.000 | 0.016 |
| UT    | 0.304 | 0.276 | 0.331 | 0.000 | 0.016 |
| DC    | 0.900 | 0.885 | 0.914 | 1.000 | 0.008 |

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
| economist (backtest) |      0.0735268 |    0.0508397 |              47 |
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

    ## [1] 0.02687551

#### Predictions for each state

| state |  mean |   low |  high |  prob |    se |
| :---- | ----: | ----: | ----: | ----: | ----: |
| FL    | 0.500 | 0.455 | 0.544 | 0.515 | 0.026 |
| NC    | 0.498 | 0.451 | 0.546 | 0.462 | 0.029 |
| NV    | 0.506 | 0.459 | 0.552 | 0.618 | 0.027 |
| PA    | 0.513 | 0.467 | 0.561 | 0.702 | 0.029 |
| MI    | 0.515 | 0.467 | 0.561 | 0.724 | 0.028 |
| NH    | 0.515 | 0.467 | 0.562 | 0.728 | 0.028 |
| –     | 0.516 | 0.478 | 0.544 | 0.870 | 0.016 |
| CO    | 0.517 | 0.470 | 0.561 | 0.775 | 0.026 |
| OH    | 0.482 | 0.432 | 0.529 | 0.218 | 0.028 |
| WI    | 0.521 | 0.473 | 0.568 | 0.809 | 0.028 |
| AZ    | 0.477 | 0.432 | 0.522 | 0.172 | 0.027 |
| VA    | 0.524 | 0.479 | 0.569 | 0.849 | 0.027 |
| GA    | 0.476 | 0.430 | 0.525 | 0.169 | 0.029 |
| IA    | 0.473 | 0.426 | 0.520 | 0.132 | 0.028 |
| MN    | 0.529 | 0.481 | 0.575 | 0.890 | 0.027 |
| NM    | 0.533 | 0.487 | 0.581 | 0.911 | 0.029 |
| ME    | 0.541 | 0.492 | 0.588 | 0.951 | 0.028 |
| SC    | 0.458 | 0.411 | 0.506 | 0.046 | 0.029 |
| OR    | 0.551 | 0.501 | 0.598 | 0.979 | 0.027 |
| TX    | 0.447 | 0.404 | 0.492 | 0.012 | 0.027 |
| MS    | 0.442 | 0.393 | 0.492 | 0.011 | 0.029 |
| MO    | 0.438 | 0.393 | 0.483 | 0.004 | 0.027 |
| DE    | 0.568 | 0.516 | 0.617 | 0.996 | 0.029 |
| CT    | 0.569 | 0.523 | 0.616 | 0.997 | 0.028 |
| WA    | 0.571 | 0.523 | 0.616 | 0.997 | 0.027 |
| AK    | 0.425 | 0.376 | 0.475 | 0.002 | 0.029 |
| NJ    | 0.577 | 0.530 | 0.621 | 0.999 | 0.026 |
| IN    | 0.417 | 0.372 | 0.464 | 0.000 | 0.028 |
| IL    | 0.584 | 0.537 | 0.631 | 1.000 | 0.028 |
| LA    | 0.412 | 0.367 | 0.459 | 0.000 | 0.028 |
| RI    | 0.590 | 0.542 | 0.636 | 0.999 | 0.028 |
| TN    | 0.406 | 0.359 | 0.454 | 0.000 | 0.029 |
| KS    | 0.406 | 0.359 | 0.452 | 0.000 | 0.028 |
| MT    | 0.404 | 0.355 | 0.450 | 0.000 | 0.027 |
| SD    | 0.396 | 0.349 | 0.443 | 0.000 | 0.028 |
| UT    | 0.392 | 0.345 | 0.438 | 0.000 | 0.028 |
| NY    | 0.611 | 0.564 | 0.655 | 1.000 | 0.026 |
| NE    | 0.389 | 0.342 | 0.437 | 0.000 | 0.029 |
| AL    | 0.387 | 0.340 | 0.434 | 0.000 | 0.028 |
| ND    | 0.385 | 0.339 | 0.431 | 0.000 | 0.027 |
| AR    | 0.384 | 0.337 | 0.433 | 0.000 | 0.030 |
| CA    | 0.625 | 0.580 | 0.668 | 1.000 | 0.026 |
| KY    | 0.372 | 0.328 | 0.419 | 0.000 | 0.027 |
| MA    | 0.633 | 0.587 | 0.680 | 1.000 | 0.028 |
| MD    | 0.643 | 0.596 | 0.685 | 1.000 | 0.025 |
| ID    | 0.352 | 0.309 | 0.398 | 0.000 | 0.027 |
| WV    | 0.351 | 0.306 | 0.396 | 0.000 | 0.027 |
| HI    | 0.654 | 0.609 | 0.697 | 1.000 | 0.026 |
| OK    | 0.340 | 0.297 | 0.383 | 0.000 | 0.026 |
| VT    | 0.661 | 0.614 | 0.704 | 1.000 | 0.026 |
| WY    | 0.287 | 0.247 | 0.328 | 0.000 | 0.024 |
| DC    | 0.908 | 0.877 | 0.933 | 1.000 | 0.015 |

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
