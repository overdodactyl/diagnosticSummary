# Calculate False Discovery Rate (FDR)

Calculates the False Discovery Rate (FDR), which is the proportion of
false positives among all positive predictions. FDR is a critical
measure in many classification contexts, particularly where the cost of
a false positive is high.

## Usage

``` r
dx_fdr(cm, detail = "full", ...)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

- ...:

  Additional arguments to pass to metric_binomial function, such as
  `citype` for type of confidence interval method.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

FDR is an important measure when the consequences of false discoveries
(false positives) are significant. It helps in understanding the error
rate among the positive predictions made by the classifier. A lower FDR
indicates a better precision of the classifier in identifying only the
true positives.

The formula for FDR is: \$\$FDR = \frac{False Positives}{False
Positives + True Positives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_fdr <- dx_fdr(cm, detail = "simple")
detailed_fdr <- dx_fdr(cm)
print(simple_fdr)
#> [1] 0.1604938
print(detailed_fdr)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 False Discovery … 16.0% …    0.160   0.0883     0.259 13/81    Binomial… ""   
```
