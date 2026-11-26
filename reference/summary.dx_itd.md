# Summary

Summary

## Usage

``` r
# S3 method for class 'dx_itd'
summary(
  object,
  variable = NA,
  label = NA,
  show_var = TRUE,
  show_label = TRUE,
  measure = NA,
  ...
)
```

## Arguments

- object:

  An object of class "dx_itd"

- variable:

  Variable to include in returned values

- label:

  Labels to include in returned values

- show_var:

  Include variable column in returned data?

- show_label:

  Include label in returned data?

- measure:

  Measures to include

- ...:

  additional arguments to be passed to or from methods

## Value

A table of the measures and their summaries, made with
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). If knitr
is not installed, a data frame.

## Examples

``` r
data <- dx_heart_failure
data$predicted[seq(5, 261, by = 20)] <- NA
fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
summary(fit, show_var = FALSE)
#> 
#> 
#> Table: Threshold= 0.5
#> 
#> |label   |measure                             |summary              |
#> |:-------|:-----------------------------------|:--------------------|
#> |Overall |Diagnostic Yield (Disease Positive) |94.9% (88.5%, 98.3%) |
#> |Overall |Diagnostic Yield (Disease Negative) |95.1% (90.6%, 97.9%) |
#> |Overall |Diagnostic Yield (Overall)          |95.0% (91.6%, 97.3%) |
#> |Overall |Pre-Test Disease Risk               |37.5% (31.7%, 43.7%) |
#> |Overall |Positive Predictive Value           |84.6% (74.7%, 91.8%) |
#> |Overall |Negative Predictive Value           |84.1% (77.7%, 89.3%) |
#> |Overall |Post-Test Disease Risk (Positive)   |84.6% (74.7%, 91.8%) |
#> |Overall |Post-Test Disease Risk (Negative)   |15.9% (10.7%, 22.3%) |
#> |Overall |Post-Test Disease Risk (Ungradable) |38.5% (13.9%, 68.4%) |
#> |Overall |Likelihood Ratio (Positive)         |9.15 (5.22, 16.04)   |
#> |Overall |Likelihood Ratio (Negative)         |0.31 (0.23, 0.44)    |
#> |Overall |Likelihood Ratio (Ungradable)       |1.04 (0.35, 3.09)    |
#> |Overall |Worst-Case Sensitivity              |67.3% (57.1%, 76.5%) |
#> |Overall |Worst-Case Specificity              |87.7% (81.7%, 92.3%) |
#> |Overall |Worst-Case Youden's J               |0.55 (0.45, 0.66)    |
```
