# Calculate Positive Likelihood Ratio

Calculates the Positive Likelihood Ratio (LR+) from a confusion matrix
object. LR+ compares the probability of a positive test result among
patients with the disease to the probability of a positive test result
among patients without the disease.

## Usage

``` r
dx_lrt_pos(cm, detail = "full", ...)
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

The positive likelihood ratio is calculated as (TP / (TP + FN)) / (FP /
(FP + TN)). It is used to assess the diagnostic usefulness of a test. A
LR+ much greater than 1 indicates a good diagnostic test that can
confidently confirm the disease when the test is positive.

The result is `NA` unless both outcome classes are present and there are
both positive and negative predictions. If any cell of the confusion
matrix is zero, 0.5 is added to every cell before the estimate and
confidence interval are calculated.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_lrp <- dx_lrt_pos(cm, detail = "simple")
detailed_lrp <- dx_lrt_pos(cm)
print(simple_lrp)
#> [1] 8.700157
print(detailed_lrp)
#> # A tibble: 1 × 8
#>   measure summary           estimate conf_low conf_high fraction conf_type notes
#>   <chr>   <chr>                <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 LRT+    8.70 (5.08, 14.9…     8.70     5.08      14.9 ""       Large sa… ""   
```
