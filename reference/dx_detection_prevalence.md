# Calculate Detection Prevalence

Calculates Detection Prevalence, which is the proportion of cases that
are predicted positive by the classifier over the total number of cases.
Detection Prevalence provides a measure of how often the condition is
identified by the model, regardless of its actual prevalence.

## Usage

``` r
dx_detection_prevalence(cm, detail = "full", ...)
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

Detection Prevalence is a measure of the frequency with which a
classifier predicts a condition as positive. It is different from the
actual condition prevalence in the population and is influenced by the
classifier's threshold and performance characteristics. High detection
prevalence could indicate a tendency of the model to predict more
positive cases, which might be useful or detrimental depending on the
specific application and the cost of false positives. It is important to
compare Detection Prevalence with the actual condition prevalence to
assess the model's performance.

The formula for Detection Prevalence is: \$\$Detection Prevalence =
\frac{Number of Predicted Positives}{Total Number of Cases}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_detection_prevalence <- dx_detection_prevalence(cm, detail = "simple")
detailed_detection_prevalence <- dx_detection_prevalence(cm)
print(simple_detection_prevalence)
#> [1] 0.3103448
print(detailed_detection_prevalence)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Detection Preval… 31.0% …    0.310    0.255     0.370 81/261   Binomial… ""   
```
