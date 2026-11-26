# Z-test for Comparing Two Proportions

Conducts a two-sided Z-test (using prop.test for two proportions) to
assess whether the success rates (proportions) of two groups are
different from each other based on a specified metric. It can compare
accuracy, PPV, NPV, FNR, FPR, FDR, sensitivity, or specificity between
two dx objects. The reported statistic is the chi-squared statistic from
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) with
continuity correction, which is the square of the Z statistic.

## Usage

``` r
dx_z_test(
  dx1,
  dx2,
  metric = c("accuracy", "ppv", "npv", "fnr", "fpr", "fdr", "sensitivity", "specificity"),
  detail = "full"
)
```

## Arguments

- dx1:

  A `dx` object for the first group.

- dx2:

  A `dx` object for the second group.

- metric:

  A character string specifying the metric to compare between the two
  groups. Options include "accuracy", "ppv", "npv", "fnr", "fpr", "fdr",
  "sensitivity", "specificity".

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate (p-value only), "full" for detailed estimate including
  confidence intervals and test statistic.

## Value

Depending on the `detail` parameter, returns the p-value of the test or
a data frame with the difference in proportions, its confidence
interval, the test statistic, and the p-value.

## Details

The function uses the prop.test function to perform the hypothesis test,
assuming the null hypothesis that the two proportions based on the
specified metric are the same. A low p-value indicates a significant
difference in the proportions, suggesting that the success rates of the
two groups are statistically significantly different. The function
automatically adjusts for continuity and provides confidence intervals
for the difference in proportions. The difference is calculated as the
metric of `dx1` minus the metric of `dx2`, so a negative estimate
indicates that `dx2` has the higher value. When both proportions are 0
or both are 1, the test is undefined: the estimate is 0 and the p-value
is `NA`, with a note.

The Z-test for two proportions is appropriate when comparing the success
rates (proportions) of two independent samples. Here are some
considerations for using this test:

- Independence: The samples should be independent. This test is not
  appropriate for paired or matched data.

- Sample Size: Both groups should have a sufficiently large number of
  trials. As a rule of thumb, the test assumes that the number of
  successes and failures in each group should be at least 5. However,
  for more accurate results, especially in cases with extreme
  proportions (close to 0 or 1), larger sample sizes may be necessary.

- Binary Outcome: This test is specific to binary outcomes
  (success/failure, presence/absence, etc.) represented as proportions.
  It's not suitable for continuous data or counts that haven't been
  converted to proportions.

- Normal Approximation: The Z-test is based on the normal approximation
  of the distribution of the sample proportion. This approximation is
  more accurate when the sample size is large and the proportion is not
  extremely close to 0 or 1.

It's also important to note that while `prop.test` adjusts for
continuity, this adjustment may not be sufficient for very small sample
sizes or very unbalanced designs. Always consider the context and
assumptions of your data when interpreting the results of the test.

## Examples

``` r
dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
dx_z_test(dx_glm, dx_rf, metric = "accuracy")
#> # A tibble: 1 × 9
#>   models test        summary p_value estimate conf_low conf_high statistic notes
#>   <chr>  <chr>       <chr>     <dbl>    <dbl>    <dbl>     <dbl>     <dbl> <chr>
#> 1 ""     Z-test: ac… 0.06 (…   0.121   0.0575  -0.0140     0.129      2.41 ""   
```
