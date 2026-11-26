# Print a diagnostic summary

Prints a short overview of a `dx`, `dx_itd`, or `dx_compare` object: the
inputs and the main results. Use
[`summary()`](https://rdrr.io/r/base/summary.html) or
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for all
of the measures.

## Usage

``` r
# S3 method for class 'dx_itd'
print(x, ...)

# S3 method for class 'dx'
print(x, ...)

# S3 method for class 'dx_compare'
print(x, ...)
```

## Arguments

- x:

  An object of class "dx", "dx_itd", or "dx_compare"

- ...:

  Not used.

## Value

`x`, invisibly.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = "Sex"
)
dx_obj
#> Diagnostic summary (dx)
#>   Predictions: predicted
#>   Reference standard: truth (event: 1)
#>   Cases: 261, 98 with the event (37.5%)
#>   Threshold: 0.3
#>   Subgroups: Sex
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
