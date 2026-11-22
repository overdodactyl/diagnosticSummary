# Calculate False Positive Rate (FPR)

Calculates the False Positive Rate (FPR), which is the proportion of
actual negatives that were incorrectly identified as positives by the
classifier. FPR is also known as the fall-out rate and is crucial in
evaluating the specificity of a classifier.

## Usage

``` r
dx_fpr(cm, detail = "full", ...)

dx_fall_out(cm, detail = "full", ...)
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

FPR is particularly important in contexts where false alarms are costly.
It is used alongside True Negative Rate (specificity) to understand the
classifier's ability to correctly identify negative instances. A lower
FPR indicates a classifier that is better at correctly identifying
negatives and not alarming false positives.

The formula for FPR is: \$\$FPR = \frac{False Positives}{False
Positives + True Negatives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_fpr <- dx_fpr(cm, detail = "simple")
detailed_fpr <- dx_fpr(cm)
print(simple_fpr)
#> [1] 0.0797546
print(detailed_fpr)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 False Positive R… 8.0% (…   0.0798   0.0431     0.133 13/163   Binomial… ""   
```
