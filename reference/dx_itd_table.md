# 3x2 table for an intention-to-diagnose analysis

Lay out the results of
[`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
as a 3x2 table: counts of positive, negative, and ungradable results by
outcome class, with predictive values, post-test disease risks, and
likelihood ratios for each row, and the diagnostic yield and worst-case
performance below.

## Usage

``` r
dx_itd_table(x, variable = "Overall", label = "Overall", show_ci = TRUE)
```

## Arguments

- x:

  An object of class `dx_itd`.

- variable, label:

  The subgroup to show. Defaults to all cases.

- show_ci:

  Show confidence intervals? If `FALSE`, only estimates are shown.

## Value

A data frame of formatted strings, one row per table row. The pre-test
disease risk is in the `predictive_value` column of the "Total" row, and
Youden's J in the `total` column of the "Worst-case performance" row.

## Examples

``` r
data <- dx_heart_failure
data$predicted[seq(5, 261, by = 20)] <- NA
fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
dx_itd_table(fit)
#>                      row     disease_positive     disease_negative
#> 1               Positive                   66                   12
#> 2               Negative                   27                  143
#> 3             Ungradable                    5                    8
#> 4                  Total                   98                  163
#> 5       Diagnostic yield 94.9% (88.5%, 98.3%) 95.1% (90.6%, 97.9%)
#> 6 Worst-case performance 67.3% (57.1%, 76.5%) 87.7% (81.7%, 92.3%)
#>                            total                             predictive_value
#> 1                             78                         84.6% (74.7%, 91.8%)
#> 2                            170                         84.1% (77.7%, 89.3%)
#> 3                             13                                             
#> 4                            261 Pre-test disease risk = 37.5% (31.7%, 43.7%)
#> 5           95.0% (91.6%, 97.3%)                                             
#> 6 Youden's J = 0.55 (0.45, 0.66)                                             
#>         post_test_risk   likelihood_ratio
#> 1 84.6% (74.7%, 91.8%) 9.15 (5.22, 16.04)
#> 2 15.9% (10.7%, 22.3%)  0.31 (0.23, 0.44)
#> 3 38.5% (13.9%, 68.4%)  1.04 (0.35, 3.09)
#> 4                                        
#> 5                                        
#> 6                                        
dx_itd_table(fit, show_ci = FALSE)
#>                      row disease_positive disease_negative             total
#> 1               Positive               66               12                78
#> 2               Negative               27              143               170
#> 3             Ungradable                5                8                13
#> 4                  Total               98              163               261
#> 5       Diagnostic yield            94.9%            95.1%             95.0%
#> 6 Worst-case performance            67.3%            87.7% Youden's J = 0.55
#>                predictive_value post_test_risk likelihood_ratio
#> 1                         84.6%          84.6%             9.15
#> 2                         84.1%          15.9%             0.31
#> 3                                        38.5%             1.04
#> 4 Pre-test disease risk = 37.5%                                
#> 5                                                              
#> 6                                                              
```
