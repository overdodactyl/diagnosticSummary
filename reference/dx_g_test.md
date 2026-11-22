# G-Test (Log-Likelihood Ratio Test) for Independence in 2x2 Table

Conducts a G-test of independence to assess the goodness of fit or the
association between two categorical variables in a 2x2 contingency
table. It is an alternative to the chi-squared test and is particularly
useful when dealing with small expected frequencies.

## Usage

``` r
dx_g_test(cm, detail = "full")
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

## Details

The test compares the observed frequencies to the expected frequencies
based on the marginal totals and calculates a G statistic, which follows
a chi-squared distribution. The test is especially useful when the data
contains small expected frequencies, which might make the chi-squared
test less accurate. A low p-value indicates a significant association
between the variables or a significant difference from the expected
distribution. Caution is needed with zero counts or very small samples.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold = 0.3, poslabel = 1
)
simple <- dx_g_test(cm, detail = "simple")
detailed <- dx_g_test(cm)
print(simple)
#> [1] 5.50871e-23
print(detailed)
#> # A tibble: 1 × 8
#>   measure summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>   <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>
#> 1 G-Test  p<0.01  5.51e-23 NA       NA        ""       ""        ""   
```
