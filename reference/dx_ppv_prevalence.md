# Calculate Positive Predictive Value (PPV) at Target Prevalence

Computes the Positive Predictive Value (PPV) adjusted to a specified
prevalence level. This function is useful for understanding classifier
performance in settings where the actual prevalence of the condition may
differ from that in the initial data.

## Usage

``` r
dx_ppv_prevalence(cm, prevalence, detail = "full", ...)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- prevalence:

  Numeric value between 0 and 1, representing the target prevalence for
  adjusting the PPV calculation.

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

This function calculates the PPV using the formula: \$\$PPV =
\frac{Sensitivity \times Prevalence}{(Sensitivity \times Prevalence) +
(1 - Specificity) \times (1 - Prevalence)}\$\$ where `Sensitivity` is
the true positive rate and `Specificity` is the true negative rate.
Adjusting PPV for prevalence allows a more accurate assessment of the
classifier’s performance in different population settings.

Confidence intervales are calucated using a simple logit (Mercaldo et
al, 2007)

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md),
[`dx_ppv()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md)
for the non-prevalence adjusted PPV.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold = 0.5, poslabel = 1
)
# Calculate PPV at a prevalence of 0.1
dx_ppv_prevalence(cm, prevalence = 0.1)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Positive Predict… 49.2% …    0.492    0.361     0.623 ""       Simple L… Prev…
```
