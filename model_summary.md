---
title: "_The Economist_'s Presidential Election Forecasting Model"
subtitle: "Model designed in collaboration with Merlin Heidemanns and Andrew Gelman (Columbia University)"
date: "Last update on Wednesday April 08, 2020 at 12:45 PM EDT"
output: 
  html_document:
    theme: readable
    keep_md: true
    
---

---



## Methodology

Our forecasting model produces predictions for Democratic and Republican two-party vote share at the state and national level for every day in the election campaign. This section describes the method with which it arrives at those predictions. 

Following the model description is a summary of how the method performed in the 2008, 2012 and 2016 presidential elections. 

### Structure

It is useful to think of the model as a combination of pre-election polls with other data about election outcomes, sometimes referred to as the "fundamentals" of the campaign. These data translate the president's popularity and the economic environment into long-range forecasts of the final election outcome, made as early as March and updated throughout the election year. In the Bayesian sense, the model is using state and national polls to update these prior expectations of the political environment. 

We are interested not only in the precise vote-share predictions from the model, but also the level of certainty we do(n't) have in them. The model is thus made up of three different conceptual components:

1. Prior predictions
2. Polling averages
3. Exploration of uncertainty

The first two are easier to explain than the latter. But before we get there, let's address what data the model is ingesting.

### Data

TK
Prior: Head-to-heads + economic index throughout the election year
Polls: Well, polls :)

### Prior

TK

### Poll averaging

TK

### Uncertianty (or, how wrong could we be?)

TK

---

## Model performance {.tabset .tabset-fade}

Overall:


Select an election for more:

### 2008 



#### Map

![](model_summary_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Final electoral college histogram

![](model_summary_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](model_summary_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### State vs national deltas over time

![](model_summary_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Model results vs polls vs the prior

![](model_summary_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

#### Performance


outlet                  ev_wtd_brier   unwtd_brier   states_correct
---------------------  -------------  ------------  ---------------
economist (backtest)       0.0286901     0.0286249               49

#### Predictions for each state

<!--html_preserve--><div id="htmlwidget-249a41bf6920fa137619" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-249a41bf6920fa137619">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52"],["IN","MO","NC","MT","ND","AR","AZ","GA","FL","OH","MS","NV","CO","VA","WV","SC","LA","SD","--","KY","TN","NM","PA","TX","MN","NH","WI","IA","MI","AK","OR","KS","ME","WA","NE","AL","NJ","CT","CA","OK","WY","IL","MD","DE","MA","UT","NY","VT","ID","RI","HI","DC"],[0.493,0.51,0.511,0.486,0.48,0.479,0.479,0.479,0.522,0.527,0.469,0.532,0.533,0.533,0.462,0.459,0.457,0.457,0.543,0.449,0.445,0.556,0.556,0.443,0.558,0.56,0.564,0.568,0.573,0.427,0.577,0.422,0.58,0.583,0.414,0.405,0.597,0.62,0.621,0.379,0.379,0.624,0.624,0.63,0.635,0.355,0.65,0.65,0.348,0.656,0.693,0.873],[0.446,0.461,0.463,0.432,0.428,0.426,0.427,0.43,0.474,0.482,0.417,0.486,0.486,0.487,0.413,0.408,0.406,0.405,0.494,0.398,0.394,0.507,0.509,0.391,0.51,0.512,0.516,0.521,0.526,0.378,0.53,0.372,0.531,0.538,0.369,0.351,0.547,0.572,0.573,0.331,0.326,0.575,0.573,0.583,0.584,0.307,0.602,0.596,0.299,0.605,0.642,0.844],[0.543,0.558,0.56,0.536,0.533,0.533,0.53,0.531,0.573,0.573,0.524,0.581,0.581,0.581,0.51,0.515,0.51,0.507,0.591,0.498,0.497,0.603,0.6,0.495,0.603,0.606,0.613,0.616,0.62,0.479,0.624,0.47,0.629,0.628,0.464,0.453,0.646,0.669,0.67,0.429,0.431,0.668,0.677,0.675,0.684,0.403,0.695,0.701,0.397,0.705,0.738,0.899],[0.403,0.634,0.642,0.331,0.275,0.252,0.231,0.273,0.776,0.852,0.167,0.868,0.869,0.873,0.098,0.107,0.091,0.092,null,0.048,0.042,0.973,0.976,0.042,0.985,0.98,0.985,0.991,0.991,0.005,0.992,0.005,0.996,0.999,0.001,0.001,1,1,1,0,0,1,1,1,1,0,1,1,0,1,1,1],[0.025,0.024,0.025,0.025,0.027,0.027,0.026,0.026,0.025,0.023,0.027,0.024,0.024,0.024,0.024,0.028,0.027,0.025,0.024,0.025,0.026,0.024,0.022,0.026,0.022,0.023,0.024,0.024,0.024,0.026,0.024,0.024,0.024,0.022,0.025,0.024,0.025,0.025,0.024,0.025,0.026,0.022,0.026,0.023,0.024,0.024,0.022,0.025,0.025,0.025,0.022,0.013]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>state<\/th>\n      <th>mean<\/th>\n      <th>low<\/th>\n      <th>high<\/th>\n      <th>prob<\/th>\n      <th>se<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### 2012 




#### Map

![](model_summary_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#### Final electoral college histogram

![](model_summary_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](model_summary_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

#### State vs national deltas over time

![](model_summary_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### Model results vs polls vs the prior

![](model_summary_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Performance


outlet                   ev_wtd_brier   unwtd_brier   states_correct
----------------------  -------------  ------------  ---------------
Linzer                             NA     0.0038000               NA
Wang/Ferguson                      NA     0.0076100               NA
Silver/538                         NA     0.0091100               NA
Jackman/Pollster                   NA     0.0097100               NA
Desart/Holbrook                    NA     0.0160500               NA
economist (backtest)        0.0267766     0.0193962               51
Intrade                            NA     0.0281200               NA
Enten/Margin of Error              NA     0.0507500               NA


#### Predictions for each state

<!--html_preserve--><div id="htmlwidget-04f60b1ca175d60a6c24" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-04f60b1ca175d60a6c24">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52"],["FL","NC","CO","VA","IA","--","OH","NH","NV","WI","MO","OR","PA","AZ","MT","IN","MI","MN","GA","NM","SD","SC","TN","NJ","ME","WA","CT","ND","NE","TX","WV","MS","KY","LA","CA","MA","KS","AL","IL","MD","AK","DE","AR","NY","RI","OK","HI","WY","ID","VT","UT","DC"],[0.505,0.495,0.512,0.515,0.518,0.518,0.52,0.522,0.527,0.528,0.464,0.536,0.538,0.461,0.459,0.458,0.544,0.544,0.454,0.551,0.447,0.437,0.436,0.566,0.567,0.57,0.572,0.427,0.427,0.421,0.419,0.413,0.407,0.406,0.603,0.605,0.394,0.391,0.609,0.613,0.385,0.628,0.371,0.63,0.633,0.338,0.662,0.335,0.329,0.695,0.278,0.822],[0.457,0.447,0.463,0.465,0.469,0.468,0.472,0.474,0.478,0.477,0.416,0.486,0.489,0.417,0.41,0.413,0.496,0.495,0.404,0.503,0.395,0.351,0.387,0.515,0.515,0.521,0.523,0.377,0.378,0.375,0.367,0.324,0.358,0.359,0.557,0.557,0.304,0.342,0.561,0.56,0.3,0.548,0.326,0.585,0.584,0.294,0.611,0.258,0.28,0.648,0.238,0.752],[0.553,0.542,0.562,0.561,0.568,0.568,0.573,0.573,0.576,0.577,0.513,0.586,0.586,0.513,0.509,0.505,0.595,0.592,0.503,0.6,0.503,0.529,0.488,0.614,0.618,0.617,0.621,0.478,0.477,0.469,0.474,0.506,0.458,0.455,0.651,0.653,0.486,0.442,0.654,0.66,0.475,0.714,0.42,0.674,0.681,0.389,0.71,0.421,0.383,0.742,0.322,0.879],[0.563,0.437,0.646,0.686,0.73,null,0.727,0.749,0.804,0.823,0.115,0.874,0.9,0.097,0.093,0.077,0.931,0.927,0.06,0.962,0.063,0.12,0.024,0.979,0.985,0.991,0.994,0.008,0.007,0.005,0.006,0.063,0.003,0,1,1,0.024,0,1,1,0.02,0.99,0,1,1,0,1,0.002,0,1,0,1],[0.024,0.024,0.025,0.023,0.025,0.025,0.027,0.026,0.025,0.025,0.024,0.025,0.024,0.026,0.025,0.023,0.026,0.024,0.025,0.024,0.028,0.046,0.026,0.024,0.025,0.024,0.024,0.026,0.025,0.024,0.027,0.046,0.026,0.024,0.024,0.024,0.046,0.026,0.023,0.024,0.045,0.043,0.024,0.022,0.024,0.025,0.024,0.043,0.027,0.024,0.022,0.028]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>state<\/th>\n      <th>mean<\/th>\n      <th>low<\/th>\n      <th>high<\/th>\n      <th>prob<\/th>\n      <th>se<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### 2016




#### Map

![](model_summary_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

#### Final electoral college histogram

![](model_summary_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

#### National and state polls and the electoral college over time

![](model_summary_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

#### State vs national deltas over time

![](model_summary_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

#### Model results vs polls vs the prior

![](model_summary_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

#### Performance


outlet                  ev_wtd_brier   unwtd_brier   states_correct
---------------------  -------------  ------------  ---------------
economist (backtest)       0.0817432     0.0575215               47
538 polls-plus             0.0928000     0.0664000               46
538 polls-only             0.0936000     0.0672000               46
princeton                  0.1169000     0.0744000               47
nyt upshot                 0.1208000     0.0801000               46
kremp/slate                0.1210000     0.0766000               46
pollsavvy                  0.1219000     0.0794000               46
predictwise markets        0.1272000     0.0767000               46
predictwise overall        0.1276000     0.0783000               46
desart and holbrook        0.1279000     0.0825000               44
daily kos                  0.1439000     0.0864000               46
huffpost                   0.1505000     0.0892000               46

#### Predictions for each state

<!--html_preserve--><div id="htmlwidget-22492bd0593adcce1923" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-22492bd0593adcce1923">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52"],["NC","FL","NV","OH","CO","--","PA","MI","IA","NH","VA","AZ","WI","GA","MN","NM","SC","ME","MO","TX","OR","MS","AK","CT","WA","IN","DE","NJ","TN","IL","KS","MT","RI","LA","SD","UT","NE","AL","ND","AR","NY","KY","CA","MA","ID","WV","MD","OK","HI","VT","WY","DC"],[0.499,0.502,0.506,0.486,0.515,0.518,0.519,0.52,0.477,0.523,0.526,0.473,0.53,0.469,0.537,0.539,0.46,0.554,0.443,0.441,0.563,0.434,0.428,0.575,0.578,0.42,0.581,0.582,0.416,0.587,0.411,0.409,0.592,0.405,0.4,0.395,0.391,0.387,0.384,0.382,0.62,0.375,0.629,0.632,0.358,0.357,0.647,0.343,0.658,0.68,0.293,0.825],[0.45,0.453,0.453,0.438,0.467,0.47,0.47,0.469,0.425,0.475,0.482,0.425,0.479,0.416,0.489,0.488,0.407,0.505,0.395,0.396,0.515,0.386,0.378,0.527,0.529,0.371,0.526,0.532,0.37,0.539,0.365,0.363,0.542,0.357,0.349,0.346,0.344,0.338,0.335,0.335,0.568,0.331,0.58,0.586,0.31,0.313,0.598,0.298,0.609,0.634,0.251,0.757],[0.549,0.554,0.556,0.533,0.563,0.567,0.568,0.567,0.529,0.572,0.571,0.519,0.58,0.517,0.587,0.59,0.512,0.603,0.496,0.488,0.609,0.48,0.476,0.62,0.627,0.467,0.632,0.63,0.465,0.632,0.459,0.457,0.641,0.454,0.453,0.443,0.438,0.438,0.431,0.436,0.67,0.421,0.678,0.677,0.405,0.404,0.695,0.389,0.704,0.726,0.338,0.88],[0.488,0.505,0.565,0.315,0.679,null,0.737,0.76,0.217,0.791,0.821,0.182,0.845,0.159,0.896,0.897,0.089,0.966,0.036,0.022,0.985,0.011,0.008,0.993,0.993,0.004,0.995,0.992,0.003,0.999,0.002,0,0.996,0.001,0,0,0.001,0,0,0,0.999,0,1,1,0,0,1,0,1,1,0,1],[0.025,0.026,0.025,0.023,0.024,0.024,0.025,0.024,0.026,0.024,0.023,0.023,0.025,0.024,0.025,0.025,0.026,0.025,0.026,0.024,0.023,0.023,0.024,0.022,0.025,0.023,0.026,0.024,0.024,0.022,0.024,0.024,0.025,0.024,0.026,0.024,0.023,0.026,0.024,0.027,0.025,0.023,0.024,0.022,0.023,0.023,0.024,0.023,0.023,0.023,0.022,0.028]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>state<\/th>\n      <th>mean<\/th>\n      <th>low<\/th>\n      <th>high<\/th>\n      <th>prob<\/th>\n      <th>se<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

