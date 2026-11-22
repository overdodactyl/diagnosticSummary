# Calculate Prevalence

Calculates Prevalence, which is the proportion of cases that are
positive for the condition of interest over the total number of cases.
Prevalence provides a measure of how widespread a condition is within
the population at a given time.

## Usage

``` r
dx_prevalence(cm, detail = "full", ...)
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

Prevalence is a measure of the burden of a condition or disease in a
population. It is an important measure in epidemiology and health
service planning as it helps to understand the level of disease in a
population at a given time. Unlike other metrics that are based on the
classifier's performance, prevalence is a measure of the actual
condition being tested.

The formula for Prevalence is: \$\$Prevalence = \frac{Number of Current
Cases (Positives)}{Total Number of Cases}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_prevalence <- dx_prevalence(cm, detail = "simple")
detailed_prevalence <- dx_prevalence(cm)
print(simple_prevalence)
#> [1] 0.3754789
print(detailed_prevalence)
#> # A tibble: 1 × 8
#>   measure    summary        estimate conf_low conf_high fraction conf_type notes
#>   <chr>      <chr>             <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Prevalence 37.5% (31.7%,…    0.375    0.317     0.437 98/261   Binomial… ""   
```
