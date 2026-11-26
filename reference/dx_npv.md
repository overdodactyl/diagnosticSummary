# Calculate Negative Predictive Value (NPV)

Calculates the proportion of true negatives out of the total predicted
negatives (true negatives + false negatives), known as the Negative
Predictive Value (NPV). This metric is a measure of the classifier's
ability to identify negatives correctly. Note that NPV, like other
metrics, may not fully represent classifier performance in unbalanced
datasets and should be used alongside other metrics.

## Usage

``` r
dx_npv(cm, detail = "full", ...)
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

NPV is the ratio of true negatives to the sum of true and false
negatives. It is an indicator of how well the classifier can identify
negative instances. High NPV means that the classifier is reliable in
its negative classifications. However, it may be influenced by the
prevalence of the condition and is best used in conjunction with other
metrics like PPV, sensitivity, and specificity for a comprehensive
evaluation.

The formula for NPV is: \$\$NPV = \frac{True Negatives}{True Negatives +
False Negatives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_npv <- dx_npv(cm, detail = "simple")
detailed_npv <- dx_npv(cm)
print(simple_npv)
#> [1] 0.8333333
print(detailed_npv)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Negative Predict… 83.3% …    0.833    0.771     0.885 150/180  Binomial… ""   
```
