# Calculate Markedness

Calculates Markedness for the provided confusion matrix. Markedness is a
combined measure of PPV (Positive Predictive Value) and NPV (Negative
Predictive Value). It reflects the effectiveness of a classifier in
marking class labels correctly, ranging from -1 to 1.

## Usage

``` r
dx_markedness(cm, detail = "full", boot = FALSE, bootreps = 2000)
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

  Logical specifying if confidence intervals should be generated via a
  percentile bootstrap of the confusion matrix cell counts. Call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first for
  reproducible intervals.

- bootreps:

  The number of bootstrap replications for calculating confidence
  intervals.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

Markedness is defined as `Markedness = PPV + NPV - 1`. It is the sum of
the proportions of predicted positives that are true positives (PPV) and
the proportion of predicted negatives that are true negatives (NPV)
minus one. It's a useful measure when you want to consider both the
positive and negative predictive values of a test. A higher markedness
indicates better performance.

The formula for Markedness is: \$\$Markedness = PPV + NPV - 1\$\$

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
to understand how to create and interact with a 'dx_cm' object.

[`dx_ppv()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md),
[`dx_npv()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_npv.md)
for components of markedness.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
simple_markedness <- dx_markedness(cm, detail = "simple")
detailed_markedness <- dx_markedness(cm)
print(simple_markedness)
#> [1] 0.6728395
print(detailed_markedness)
#> # A tibble: 1 × 8
#>   measure    summary estimate conf_low conf_high fraction conf_type notes       
#>   <chr>      <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>       
#> 1 Markedness 0.67       0.673       NA        NA ""       NA        Specify `bo…
```
