# McNemar's Chi-squared Test for Paired Proportions

Performs McNemar's test to evaluate the difference between two paired
proportions. This is typically used in the context of binary
classification to test whether the proportion of correct classifications
significantly differs between two classifiers on the same set of
instances.

## Usage

``` r
dx_mcnemars(dx1, dx2, detail = "full")
```

## Arguments

- dx1:

  The first `dx` object containing predictions and truth values.

- dx2:

  The second `dx` object containing predictions and truth values.

- detail:

  A string indicating the level of detail for the output: "simple" for
  just the p-value, "full" for a comprehensive result including test
  statistics.

## Value

Depending on the `detail` parameter, returns either the p-value (simple)
or a data frame with the test statistic and p-value (full).

## Details

McNemar's test is appropriate when comparing the classification results
of two algorithms on the same data set (paired design). It specifically
tests the null hypothesis that the marginal probabilities of row and
column variable are the same.

This test is suitable for binary classification tasks where you are
comparing the performance of two classifiers over the same instances.
It's not appropriate for independent samples or more than two
classifiers.

The function expects the input as two `dx` objects, each containing the
predictions and truth values from the classifiers being compared. It
calculates the contingency table based on the agreements and
disagreements between the classifiers and applies McNemar's test to this
table.

## See also

[`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
to create a `dx` object,
[`stats::mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html) for
the underlying test.

## Examples

``` r
dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
dx_mcnemars(dx_glm, dx_rf)
#> # A tibble: 1 × 9
#>   models test        summary p_value estimate conf_low conf_high statistic notes
#>   <chr>  <chr>       <chr>     <dbl> <lgl>    <lgl>    <lgl>         <dbl> <chr>
#> 1 ""     McNemar's … p=0.02   0.0180 NA       NA       NA              5.6 ""   
```
