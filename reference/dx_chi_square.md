# Chi-Square Test for Independence in a 2x2 Table

Conducts a Chi-square test of independence on a 2x2 confusion matrix
derived from binary classification results, assessing whether the
observed frequency distribution differs from the expected distribution.

## Usage

``` r
dx_chi_square(cm, detail = "full")
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
value representing the p-value of the Chi-square test. - if "full": a
data frame with the Chi-square test result, including the p-value and
method note.

## Details

The Chi-square test is used to determine whether there is a significant
association between the predicted and actual binary classifications. It
compares the observed frequencies in each cell of the table to the
frequencies expected if the rows and columns are independent. A low
p-value indicates that the distributions of actual and predicted
classifications are not independent, suggesting a significant
association between them. The function uses Pearson's Chi-squared test
with Yates' continuity correction by default, which is more accurate for
small sample sizes. The test is most appropriate when each cell in the
2x2 table has an expected frequency of 5 or more.

## See also

[`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
for creating a 'dx_cm' object.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold = 0.3, poslabel = 1
)
simple <- dx_chi_square(cm, detail = "simple")
detailed <- dx_chi_square(cm)
print(simple)
#> [1] 5.450633e-21
print(detailed)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>
#> 1 Pearson's Chi-sq… p<0.01  5.45e-21 NA       NA        ""       ""        Pear…
```
