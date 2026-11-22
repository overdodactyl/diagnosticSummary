# Calculate Positive Predictive Value (PPV, Precision)

Calculates the proportion of true positives out of the total predicted
positives (true positives + false positives). PPV is also known as
precision.Note that PPV can be influenced by the prevalence of the
condition and should be used alongside other metrics.

## Usage

``` r
dx_ppv(cm, detail = "full", ...)

dx_precision(cm, detail = "full", ...)
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

PPV, also known as precision, is the ratio of true positives to the sum
of true and false positives. It reflects the classifier's ability to
identify only relevant instances. However, like accuracy, it may not be
suitable for unbalanced datasets. For detailed diagnostics, including
confidence intervals, specify detail = "full".

The formula for PPV is: \$\$PPV = \frac{True Positives}{True Positives +
False Positives}\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_ppv <- dx_ppv(cm, detail = "simple")
detailed_ppv <- dx_ppv(cm)
print(simple_ppv)
#> [1] 0.8395062
print(detailed_ppv)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Positive Predict… 84.0% …    0.840    0.741     0.912 68/81    Binomial… ""   
```
