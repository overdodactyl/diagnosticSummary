# Calculate Negative Likelihood Ratio

Calculates the Negative Likelihood Ratio (LR-) from a confusion matrix
object. LR- compares the probability of a negative test result among
patients with the disease to the probability of a negative test result
among patients without the disease.

## Usage

``` r
dx_lrt_neg(cm, detail = "full", ...)
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

The negative likelihood ratio is calculated as (FN / (TP + FN)) / (TN /
(FP + TN)). It is used to assess the diagnostic usefulness of a test. A
LR- closer to 0 indicates a good diagnostic test that can confidently
rule out the disease when the test is negative.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_lrn <- dx_lrt_neg(cm, detail = "simple")
detailed_lrn <- dx_lrt_neg(cm)
print(simple_lrn)
#> [1] 0.3326531
print(detailed_lrn)
#> # A tibble: 1 × 8
#>   measure summary           estimate conf_low conf_high fraction conf_type notes
#>   <chr>   <chr>                <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 LRT-    0.33 (0.25, 0.45)    0.333    0.246     0.450 ""       Large sa… ""   
```
