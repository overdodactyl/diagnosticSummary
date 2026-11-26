# Calculate Fowlkes-Mallows Index

Calculates the Fowlkes-Mallows Index (FM Index) for the provided
confusion matrix. FM Index is the geometric mean of precision and
recall, providing a balance measure between these two metrics.

## Usage

``` r
dx_fowlkes_mallows(cm, detail = "full", boot = FALSE, bootreps = 2000)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

- boot:

  Logical specifying if confidence intervals should be generated via a
  percentile bootstrap of the confusion matrix cell counts. With the
  default, `FALSE`, no confidence interval is reported. Call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first for
  reproducible intervals.

- bootreps:

  The number of bootstrap replications for calculating confidence
  intervals.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

Fowlkes-Mallows Index is defined as the geometric mean of the precision
(Positive Predictive Value) and recall (True Positive Rate or
Sensitivity). It's a useful measure when you want a balance between
precision and recall without the harshness of the harmonic mean used in
F1 score. A higher Fowlkes-Mallows Index indicates better precision and
recall balance.

The formula for Fowlkes-Mallows Index is: \$\$FM = \sqrt{Precision
\times Recall}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

[`dx_ppv()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md),
[`dx_sensitivity()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md)
for components of FM Index.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
simple_fm_index <- dx_fowlkes_mallows(cm, detail = "simple")
detailed_fm_index <- dx_fowlkes_mallows(cm)
print(simple_fm_index)
#> [1] 0.7632264
print(detailed_fm_index)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Fowlkes-Mallows … 0.76       0.763       NA        NA ""       NA        Spec…
```
