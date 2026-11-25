# Calculate No Information Rate (NIR)

The No Information Rate is the proportion of the largest class in the
actual outcomes. It represents the accuracy that a naive model would
achieve by always predicting the most frequent class. It's a baseline
measure for classification performance.

## Usage

``` r
dx_nir(cm, detail = "full")
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
nir <- dx_nir(cm)
print(nir)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>
#> 1 No Information R… 62.5%      0.625 NA       NA        163/261  ""        ""   
```
