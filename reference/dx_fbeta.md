# Calculate F-beta Score with Confidence Intervals

Calculates the F-beta score from a confusion matrix object with an
option to include bootstrapped confidence intervals. The F-beta score is
a generalization of the F1 score, allowing different importance to
precision and recall via the beta parameter.

## Usage

``` r
dx_fbeta(cm, beta = 1, detail = "full", boot = FALSE, bootreps = 2000)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- beta:

  The beta value determining the weight of precision in the F-score.

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

Depending on the `detail` parameter, returns a single numeric value of
F-beta or a data frame with the F-beta and its confidence intervals.

## See also

[`dx_f1()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_f1.md),
[`dx_f2()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_f2.md)
for specific F-beta scores.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_f <- dx_fbeta(cm, beta = .5, detail = "simple")
detailed_f <- dx_fbeta(cm, beta = .5)
print(simple_f)
#> [1] 0.8056872
print(detailed_f)
#> # A tibble: 1 × 8
#>   measure    summary estimate conf_low conf_high fraction conf_type notes       
#>   <chr>      <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>       
#> 1 F0.5 Score 0.81       0.806       NA        NA ""       NA        Specify `bo…
```
