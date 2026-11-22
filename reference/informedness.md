# Calculate Informedness

Calculates Informedness for the provided confusion matrix. Informedness
is a combined measure of Sensitivity (True Positive Rate) and
Specificity (True Negative Rate). It reflects the probability that a
classifier is informed about the true class, ranging from -1 to 1.

## Usage

``` r
dx_informedness(cm, detail = "full", boot = FALSE, bootreps = 1000)

dx_youden_j(cm, detail = "full", boot = FALSE, bootreps = 1000)
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

Informedness is defined as
`Informedness = Sensitivity + Specificity - 1`. It is the sum of the
true positive rate and the true negative rate minus one. It's a useful
measure when you want to consider both the sensitivity and specificity
of a test. A higher informedness indicates better overall performance of
the classifier in distinguishing between the classes.

The formula for Informedness is: \$\$Informedness = Sensitivity +
Specificity - 1\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

[`dx_sensitivity()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md),
[`dx_specificity()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md)
for components of informedness.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
simple_informedness <- dx_informedness(cm, detail = "simple")
detailed_informedness <- dx_informedness(cm)
print(simple_informedness)
#> [1] 0.6141229
print(detailed_informedness)
#> # A tibble: 1 × 8
#>   measure      summary estimate conf_low conf_high fraction conf_type notes     
#>   <chr>        <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>     
#> 1 Informedness 0.61       0.614       NA        NA ""       NA        Specify `…
```
