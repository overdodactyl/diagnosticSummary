
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diagnosticSummary <img src="man/figures/logo.png" align="right" width="100" height="100" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/overdodactyl/diagnosticSummary/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/overdodactyl/diagnosticSummary/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`diagnosticSummary` is designed to quickly create diagnostic summaries
and reports for binary classification data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("overdodactyl/diagnosticSummary")
```

## Example

``` r
library(diagnosticSummary)
# Load sample data
data("dx_heart_failure")
head(dx_heart_failure)
#>   AgeGroup    Sex truth   predicted           AgeSex predicted_rf
#> 1  (20,50]   Male     0 0.016164112   (20,50] - Male   0.19774011
#> 2  (20,50]   Male     0 0.074193671   (20,50] - Male   0.04624277
#> 3  (20,50] Female     0 0.004677979 (20,50] - Female   0.22448980
#> 4  (20,50] Female     0 0.017567313 (20,50] - Female   0.09326425
#> 5  (20,50] Female     0 0.017517025 (20,50] - Female   0.04878049
#> 6  (20,50]   Male     0 0.051570734   (20,50] - Male   0.10982659
```

``` r

# Create dx object
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  threshold_range = c(.1,.2,.3),
  setthreshold = .3,
  doboot = TRUE,
  bootreps = 1000,
  grouping_variables = c("AgeGroup", "Sex", "AgeSex")
)
```

``` r
summary(dx_obj, variable = "Overall", show_var = F, show_label = F)
```

| measure                          | summary              |
|:---------------------------------|:---------------------|
| AUC ROC                          | 0.904 (0.864, 0.943) |
| Accuracy                         | 79.3% (73.9%, 84.1%) |
| Sensitivity                      | 84.7% (76.0%, 91.2%) |
| Specificity                      | 76.1% (68.8%, 82.4%) |
| Positive Predictive Value        | 68.0% (59.0%, 76.2%) |
| Negative Predictive Value        | 89.2% (82.8%, 93.8%) |
| LRT+                             | 3.54 (2.66, 4.71)    |
| LRT-                             | 0.20 (0.13, 0.32)    |
| Odds Ratio                       | 17.59 (9.12, 33.94)  |
| F1 Score                         | 75.5% (68.6%, 81.2%) |
| F2 Score                         | 80.7% (73.9%, 86.3%) |
| Prevalence                       | 37.5% (31.7%, 43.7%) |
| False Negative Rate              | 15.3% (8.8%, 24.0%)  |
| False Positive Rate              | 23.9% (17.6%, 31.2%) |
| False Discovery Rate             | 32.0% (23.8%, 41.0%) |
| AUC PR                           | 0.87                 |
| Cohen’s Kappa                    | 0.58 (0.48, 0.68)    |
| Matthews Correlation Coefficient | 59.0% (49.6%, 68.1%) |
| Balanced Accuracy                | 80.4% (75.5%, 85.5%) |
| Informedness                     | 60.8% (51.2%, 69.9%) |
| Markedness                       | 57.2% (47.2%, 66.7%) |
| G-mean                           | 80.3% (75.5%, 85.0%) |
| Fowlkes-Mallows Index            | 75.9% (69.7%, 81.7%) |
| Brier Score                      | 0.11                 |
| Pearson’s Chi-squared            | p\<0.01              |
| Fisher’s Exact                   | p\<0.01              |
| G-Test                           | p\<0.01              |

Threshold= 0.3
