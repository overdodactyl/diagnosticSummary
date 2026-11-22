# DeLong's Test for Comparing Two ROC Curves

This function applies DeLong's test to compare the areas under two
correlated ROC curves, providing a statistical approach to assess if
there is a significant difference between them. It's particularly useful
for comparing the performance of two diagnostic models.

## Usage

``` r
dx_delong(dx1, dx2, detail = "full", paired = TRUE)
```

## Arguments

- dx1:

  A `dx` object containing the first ROC curve.

- dx2:

  A `dx` object containing the second ROC curve.

- detail:

  Character specifying the level of detail in the output: "simple" for
  p-value only, "full" for detailed test results.

- paired:

  Logical indicating if data between `dx1` and `dx2` is paired.

## Value

Depending on the `detail` parameter, returns a p-value or a
comprehensive summary of the test including the test statistic, p-value,
and confidence interval for the difference in AUC.

## Details

The function utilizes the `roc.test` function from the `pROC` package to
perform DeLong's test, which is suitable for comparing two correlated
ROC curves. This correlation typically arises from using the same set of
samples to generate both curves. A significant p-value indicates a
statistically significant difference between the AUCs of the two models.
When `detail` is "full", additional information about the comparison,
including the estimated difference in AUC and its confidence interval,
is provided. The difference is calculated as the AUC of `dx1` minus the
AUC of `dx2`, so a negative estimate indicates that `dx2` has the higher
AUC. The confidence interval gives a range of plausible values for the
difference in AUC between the two models. A confidence interval is only
available for paired comparisons.

## See also

[`pROC::roc.test()`](https://rdrr.io/pkg/pROC/man/roc.test.html) for the
underlying test implementation.

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create a `dx` object.

## Examples

``` r
dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
simple <- dx_delong(dx_glm, dx_rf, detail = "simple")
detailed <- dx_delong(dx_glm, dx_rf)
print(simple)
#> [1] 0.0005886491
print(detailed)
#> # A tibble: 1 × 9
#>   models test        summary p_value estimate conf_low conf_high statistic notes
#>   <chr>  <chr>       <chr>     <dbl>    <dbl>    <dbl>     <dbl>     <dbl> <chr>
#> 1 ""     DeLong's t… 0.04 (… 5.89e-4   0.0413   0.0178    0.0649      3.44 ""   
```
