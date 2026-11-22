# Calculate Sensitivity (True Positive Rate, Recall)

Calculates Sensitivity, also known as the True Positive Rate (TPR) or
recall, which is the proportion of actual positives that are correctly
identified as such by the classifier. Sensitivity is a key measure in
evaluating the effectiveness of a classifier in identifying positive
instances.

## Usage

``` r
dx_sensitivity(cm, detail = "full", ...)

dx_recall(cm, detail = "full", ...)

dx_tpr(cm, detail = "full", ...)
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

Sensitivity or TPR is an important measure in scenarios where missing a
positive identification has serious consequences. It essentially
measures the proportion of actual positives that are correctly
identified, giving insight into the ability of the classifier to detect
positive instances. A higher sensitivity indicates a better performance
in recognizing positive instances.

The formula for Sensitivity is: \$\$Sensitivity = \frac{True
Positives}{True Positives + False Negatives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_sensitivity <- dx_sensitivity(cm, detail = "simple")
detailed_sensitivity <- dx_sensitivity(cm)
print(simple_sensitivity)
#> [1] 0.6938776
print(detailed_sensitivity)
#> # A tibble: 1 × 8
#>   measure     summary       estimate conf_low conf_high fraction conf_type notes
#>   <chr>       <chr>            <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Sensitivity 69.4% (59.3%…    0.694    0.593     0.783 68/98    Binomial… ""   
```
