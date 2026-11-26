# diagnosticSummary

`diagnosticSummary` summarizes how well predicted probabilities agree
with a binary reference standard. One call to
[`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
gives:

- sensitivity, specificity, predictive values, likelihood ratios, the
  AUC, and other measures, with confidence intervals
- the same measures at other thresholds and by subgroup
- plots such as ROC, precision-recall, calibration, and decision curves,
  and a forest plot of subgroups (`dx_plot_*()`)

[`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
compares models, and
[`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
handles missing predictions.

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
  threshold_range = c(.1,.2,.3),
  setthreshold = .3,
  doboot = TRUE,
  bootreps = 1000,
  grouping_variables = c("AgeGroup", "Sex", "AgeSex")
)
```

Printing the object gives a short overview:

``` r

dx_obj
#> Diagnostic summary (dx)
#>   Predictions: predicted
#>   Reference standard: truth (event: 1)
#>   Cases: 261, 98 with the event (37.5%)
#>   Threshold: 0.3
#>   Subgroups: AgeGroup, Sex, AgeSex
#>   Other thresholds: 0.1, 0.2
#> 
#>  measure                   summary             
#>  AUC ROC                   0.904 (0.864, 0.943)
#>  Sensitivity               84.7% (76.0%, 91.2%)
#>  Specificity               76.1% (68.8%, 82.4%)
#>  Positive Predictive Value 68.0% (59.0%, 76.2%)
#>  Negative Predictive Value 89.2% (82.8%, 93.8%)
#> 
#> Use summary() or as.data.frame() for all measures.
```

[`summary()`](https://rdrr.io/r/base/summary.html) shows every measure,
and [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
returns them with unformatted estimates and confidence limits:

``` r

summary(dx_obj, variable = "Overall", show_var = FALSE, show_label = FALSE)
```

| measure                                   | summary              |
|:------------------------------------------|:---------------------|
| AUC ROC                                   | 0.904 (0.864, 0.943) |
| Accuracy                                  | 79.3% (73.9%, 84.1%) |
| Sensitivity                               | 84.7% (76.0%, 91.2%) |
| Specificity                               | 76.1% (68.8%, 82.4%) |
| Positive Predictive Value                 | 68.0% (59.0%, 76.2%) |
| Negative Predictive Value                 | 89.2% (82.8%, 93.8%) |
| LRT+                                      | 3.54 (2.66, 4.71)    |
| LRT-                                      | 0.20 (0.13, 0.32)    |
| Odds Ratio                                | 17.59 (9.12, 33.94)  |
| F1 Score                                  | 0.75 (0.69, 0.82)    |
| F2 Score                                  | 0.81 (0.74, 0.87)    |
| Prevalence                                | 37.5% (31.7%, 43.7%) |
| False Negative Rate                       | 15.3% (8.8%, 24.0%)  |
| False Positive Rate                       | 23.9% (17.6%, 31.2%) |
| False Discovery Rate                      | 32.0% (23.8%, 41.0%) |
| AUC PR                                    | 0.873                |
| Cohen’s Kappa                             | 0.58 (0.48, 0.68)    |
| Matthews Correlation Coefficient          | 0.59 (0.49, 0.69)    |
| Balanced Accuracy                         | 0.80 (0.75, 0.85)    |
| Informedness                              | 0.61 (0.50, 0.70)    |
| Markedness                                | 0.57 (0.47, 0.66)    |
| G-mean                                    | 0.80 (0.76, 0.85)    |
| Fowlkes-Mallows Index                     | 0.76 (0.69, 0.82)    |
| Brier Score                               | 0.114                |
| Pearson’s Chi-squared (Yates’ correction) | p\<0.01              |
| Fisher’s Exact                            | p\<0.01              |
| G-Test                                    | p\<0.01              |

Threshold= 0.3

## Plots

``` r

dx_plot_roc(dx_obj)
```

![](reference/figures/README-roc-1.svg)

## Comparing models

``` r

dx_rf <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted_rf",
  setthreshold = .3
)

dx_compare(list(Logistic = dx_obj, `Random forest` = dx_rf), paired = TRUE)
#> Comparison of 2 models (dx_compare): Logistic, Random forest
#> 
#>  models                     test                         summary          
#>  Logistic vs. Random forest DeLong's test for ROC curves 0.04 (0.02, 0.06)
#>  Logistic vs. Random forest McNemar's Chi-squared Test   p=0.15           
#>  p_value
#>  p<0.001
#>  p=0.151
#> 
#> All results are in the `tests` element.
```

## Missing predictions

If some predictions are missing, for example because a case failed
quality control,
[`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
keeps those cases as ungradable results in a 3x2 intention-to-diagnose
analysis. It reports the diagnostic yield, post-test disease risks and
likelihood ratios for each result, and worst-case performance, which
counts every ungradable result as incorrect.

``` r

data_missing <- dx_heart_failure
data_missing$predicted[seq(5, 261, by = 20)] <- NA

itd_obj <- dx_itd(
  data = data_missing,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)

dx_itd_table(itd_obj)
#>                      row     disease_positive     disease_negative
#> 1               Positive                   79                   36
#> 2               Negative                   14                  119
#> 3             Ungradable                    5                    8
#> 4                  Total                   98                  163
#> 5       Diagnostic yield 94.9% (88.5%, 98.3%) 95.1% (90.6%, 97.9%)
#> 6 Worst-case performance 80.6% (71.4%, 87.9%) 73.0% (65.5%, 79.7%)
#>                            total                             predictive_value
#> 1                            115                         68.7% (59.4%, 77.0%)
#> 2                            133                         89.5% (83.0%, 94.1%)
#> 3                             13                                             
#> 4                            261 Pre-test disease risk = 37.5% (31.7%, 43.7%)
#> 5           95.0% (91.6%, 97.3%)                                             
#> 6 Youden's J = 0.54 (0.43, 0.64)                                             
#>         post_test_risk  likelihood_ratio
#> 1 68.7% (59.4%, 77.0%) 3.65 (2.69, 4.95)
#> 2  10.5% (5.9%, 17.0%) 0.20 (0.12, 0.32)
#> 3 38.5% (13.9%, 68.4%) 1.04 (0.35, 3.09)
#> 4                                       
#> 5                                       
#> 6
```
