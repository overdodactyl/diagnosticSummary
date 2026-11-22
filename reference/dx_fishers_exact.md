# Fisher's Exact Test for Independence in a 2x2 Table

Conducts Fisher's Exact test of independence on a 2x2 confusion matrix
derived from binary classification results, assessing the significance
of the association between the observed and expected frequencies.

## Usage

``` r
dx_fishers_exact(cm, detail = "full")
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

Depending on the `detail` parameter: - if "simple": a single numeric
value representing the p-value of Fisher's Exact test. - if "full": a
data frame with the Fisher's Exact test result, including the p-value
and method note.

## Details

Fisher's Exact Test is used to examine the significance of the
association between the variables in a 2x2 contingency table,
particularly useful when sample sizes are small. Unlike the chi-square
test, it does not rely on large sample distribution approximations and
is hence exact. It's especially preferred when the data has small
expected frequencies in one or more cells of the table. A low p-value
indicates a significant association between the predicted and actual
binary classifications.

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
for creating a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold = 0.3, poslabel = 1
)
simple <- dx_fishers_exact(cm, detail = "simple")
detailed <- dx_fishers_exact(cm)
print(simple)
#> [1] 1.660279e-22
print(detailed)
#> # A tibble: 1 × 8
#>   measure        summary estimate conf_low conf_high fraction conf_type notes   
#>   <chr>          <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>   
#> 1 Fisher's Exact p<0.01  1.66e-22 NA       NA        ""       ""        Exact t…
```
