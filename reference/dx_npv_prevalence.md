# Calculate Negative Predictive Value (NPV) at Target Prevalence

Computes the Negative Predictive Value (NPV) adjusted to a specified
prevalence level. This function is useful for understanding classifier
performance in settings where the actual prevalence of the condition may
differ from that in the initial data.

## Usage

``` r
dx_npv_prevalence(cm, prevalence, detail = "full", ...)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- prevalence:

  Numeric value between 0 and 1, representing the target prevalence for
  adjusting the NPV calculation.

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

- ...:

  Additional arguments, such as `citype`, the confidence interval method
  for proportions (see
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)).

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

This function calculates the NPV using the formula: \$\$NPV =
\frac{Specificity \times (1 - Prevalence)}{(1 - Sensitivity) \times
Prevalence + Specificity \times (1 - Prevalence)}\$\$ where
`Specificity` is the true negative rate and `Sensitivity` is the true
positive rate. Adjusting NPV for prevalence allows better estimation of
the classifier’s performance in different population settings.

Confidence intervals are calculated on the logit scale (Mercaldo et al.,
2007).

## References

Mercaldo ND, Lau KF, Zhou XH. Confidence intervals for predictive values
with an emphasis to case-control studies. Statistics in Medicine.
2007;26(10):2170-2183.

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md),
[`dx_npv()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_npv.md)
for the non-prevalence adjusted NPV.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold = 0.5, poslabel = 1
)
# Calculate NPV at a prevalence of 0.1
dx_npv_prevalence(cm, prevalence = 0.1)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Negative Predict… 96.4% …    0.964    0.952     0.973 ""       Simple L… Prev…
```
