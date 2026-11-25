# Calculate F2 Score with Confidence Intervals

Calculates the F2 score from a confusion matrix object with an option to
include bootstrapped confidence intervals. The F2 score weights recall
higher than precision.

## Usage

``` r
dx_f2(cm, detail = "full", boot = FALSE, bootreps = 2000)
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

Depending on the `detail` parameter, returns a single numeric value of
F2 or a data frame with the F2 and its confidence intervals.

## See also

[`dx_fbeta()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fbeta.md),
[`dx_f1()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_f1.md)
for other F-beta scores.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_f <- dx_f2(cm, detail = "simple")
detailed_f <- dx_f2(cm)
print(simple_f)
#> [1] 0.7188161
print(detailed_f)
#> # A tibble: 1 × 8
#>   measure  summary estimate conf_low conf_high fraction conf_type notes         
#>   <chr>    <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>         
#> 1 F2 Score 0.72       0.719       NA        NA ""       NA        Specify `boot…
```
