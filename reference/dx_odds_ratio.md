# Calculate Odds Ratio

Calculates the Odds Ratio (OR) from a confusion matrix object. OR is a
measure of association between exposure and an outcome. It represents
the odds that an outcome will occur given a particular exposure,
compared to the odds of the outcome occurring in the absence of that
exposure.

## Usage

``` r
dx_odds_ratio(cm, detail = "full", ...)
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

  Additional arguments, such as `citype`, the confidence interval method
  for proportions (see
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)).

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

The odds ratio is calculated as (TP \* TN) / (FP \* FN). It is used in
case-control studies to estimate the strength of the association between
exposure and outcome. Note that a value of 1 indicates no association,
greater than 1 indicates increased odds of the outcome with exposure,
and less than 1 indicates decreased odds.

The result is `NA` unless both outcome classes are present and there are
both positive and negative predictions. If any cell of the confusion
matrix is zero, 0.5 is added to every cell before the estimate and
confidence interval are calculated.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_or <- dx_odds_ratio(cm, detail = "simple")
detailed_or <- dx_odds_ratio(cm)
print(simple_or)
#> [1] 26.15385
print(detailed_or)
#> # A tibble: 1 × 8
#>   measure    summary        estimate conf_low conf_high fraction conf_type notes
#>   <chr>      <chr>             <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Odds Ratio 26.15 (12.84,…     26.2     12.8      53.3 ""       Large sa… ""   
```
