# Calculate Specificity (True Negative Rate)

Calculates Specificity, also known as the True Negative Rate (TNR),
which is the proportion of actual negatives that are correctly
identified as such by the classifier. Specificity is a key measure in
evaluating the effectiveness of a classifier in identifying negative
instances.

## Usage

``` r
dx_specificity(cm, detail = "full", ...)

dx_tnr(cm, detail = "full", ...)
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

Specificity or TNR measures how well the classifier can identify
negative instances, which is critical in situations where false
positives carry a high cost. A higher specificity indicates a better
performance in recognizing negative instances and avoiding false alarms.

The formula for Specificity is: \$\$Specificity = \frac{True
Negatives}{True Negatives + False Positives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_specificity <- dx_specificity(cm, detail = "simple")
detailed_specificity <- dx_specificity(cm)
print(simple_specificity)
#> [1] 0.9202454
print(detailed_specificity)
#> # A tibble: 1 × 8
#>   measure     summary       estimate conf_low conf_high fraction conf_type notes
#>   <chr>       <chr>            <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Specificity 92.0% (86.7%…    0.920    0.867     0.957 150/163  Binomial… ""   
```
