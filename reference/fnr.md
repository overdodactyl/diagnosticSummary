# Calculate False Negative Rate (FNR)

Calculates the False Negative Rate (FNR), which is the proportion of
actual positives that were incorrectly identified as negatives by the
classifier. FNR is also known as the miss rate and is a critical measure
in evaluating the performance of a classifier, especially in scenarios
where failing to detect positives is costly.

## Usage

``` r
dx_fnr(cm, detail = "full", ...)

dx_miss_rate(cm, detail = "full", ...)
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

  Additional arguments, such as `citype`, the confidence interval method
  for proportions (see
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)).

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

FNR is an important measure in situations where the cost of missing a
positive classification is high. It complements the True Positive Rate
(sensitivity) and helps in understanding the trade-offs between
identifying positives and avoiding false alarms. A lower FNR is
generally desirable and indicates a more sensitive classifier.

The formula for FNR is: \$\$FNR = \frac{False Negatives}{False
Negatives + True Positives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_fnr <- dx_fnr(cm, detail = "simple")
detailed_fnr <- dx_fnr(cm)
print(simple_fnr)
#> [1] 0.3061224
print(detailed_fnr)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 False Negative R… 30.6% …    0.306    0.217     0.407 30/98    Binomial… ""   
```
