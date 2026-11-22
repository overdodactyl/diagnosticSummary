# Calculate Accuracy

Calculates the proportion of correct predictions (True Positives + True
Negatives) over all cases from a confusion matrix object, providing a
measure of the classifier's overall correctness.

## Usage

``` r
dx_accuracy(cm, detail = "full", ...)
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

\\Accuracy = \frac{True Positives + True Negatives}{Total Cases}\\

Accuracy is one of the most intuitive performance measures and it is
simply a ratio of correctly predicted observation to the total
observations. It's a common starting point for evaluating the
performance of a classifier. However, it's not suitable for unbalanced
classes due to its tendency to be misleadingly high when the class of
interest is underrepresented. For detailed diagnostics, including
confidence intervals, specify detail = "full".

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(
  dx_heart_failure$predicted,
  dx_heart_failure$predicted,
  threshold = 0.3, poslabel = 1
)
simple_accuracy <- dx_accuracy(cm, detail = "simple")
detailed_accuracy <- dx_accuracy(cm)
print(simple_accuracy)
#> [1] 0.532567
print(detailed_accuracy)
#> # A tibble: 1 × 8
#>   measure  summary          estimate conf_low conf_high fraction conf_type notes
#>   <chr>    <chr>               <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Accuracy 53.3% (47.0%, 5…    0.533    0.470     0.594 139/261  Binomial… ""   
```
