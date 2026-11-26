# Calculate G-mean

Calculates the Geometric Mean (G-mean) for the provided confusion
matrix. G-mean is a measure of a model's performance that considers both
the sensitivity (True Positive Rate) and specificity (True Negative
Rate), especially useful in imbalanced datasets.

## Usage

``` r
dx_g_mean(cm, detail = "full", boot = FALSE, bootreps = 2000)
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

G-mean is the geometric mean of sensitivity and specificity. It tries to
maximize the accuracy on each of the two classes while keeping these
accuracies balanced. For a classifier to achieve a high G-mean score, it
must perform well on both positive and negative classes.

The formula for G-mean is: \$\$G-mean = \sqrt{Sensitivity \times
Specificity}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

[`dx_sensitivity()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md),
[`dx_specificity()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md)
for components of G-mean.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
simple_g_mean <- dx_g_mean(cm, detail = "simple")
detailed_g_mean <- dx_g_mean(cm)
print(simple_g_mean)
#> [1] 0.7990855
print(detailed_g_mean)
#> # A tibble: 1 × 8
#>   measure summary estimate conf_low conf_high fraction conf_type notes          
#>   <chr>   <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>          
#> 1 G-mean  0.80       0.799       NA        NA ""       NA        Specify `boot …
```
