# Calculate Balanced Accuracy

Calculates Balanced Accuracy, which is the average of sensitivity
(recall) and specificity. This metric is particularly useful for
imbalanced datasets as it accounts for both the positive and negative
classes equally and doesn't inherently favor the majority class.

## Usage

``` r
dx_balanced_accuracy(cm, detail = "full", boot = FALSE, bootreps = 1000)
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

  Logical specifying if confidence intervals should be generated via
  bootstrapping. Note, this can be slow.

- bootreps:

  The number of bootstrap replications for calculating confidence
  intervals.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

Balanced Accuracy mitigates the issue of the regular accuracy metric
favoring models that predict the majority class in an imbalanced
dataset. By taking the average of sensitivity and specificity, it gives
a better measure of the overall performance especially when classes are
imbalanced or when costs of different errors vary greatly.

The formula for Balanced Accuracy is: \$\$Balanced Accuracy =
\frac{Sensitivity + Specificity}{2}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

[`dx_sensitivity()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md),
[`dx_specificity()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md)
for the components of balanced accuracy.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
simple_balanced_accuracy <- dx_balanced_accuracy(cm, detail = "simple")
detailed_balanced_accuracy <- dx_balanced_accuracy(cm)
print(simple_balanced_accuracy)
#> [1] 0.8070615
print(detailed_balanced_accuracy)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Balanced Accuracy 0.81       0.807       NA        NA ""       NA        Spec…
```
