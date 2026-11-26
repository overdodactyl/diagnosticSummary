# Convert an intention-to-diagnose analysis to a data frame

Returns the measures in a `dx_itd` object, with unformatted estimates
and confidence limits.

## Usage

``` r
# S3 method for class 'dx_itd'
as.data.frame(
  x,
  row.names = NULL,
  optional = TRUE,
  variable = NA,
  label = NA,
  measure = NA,
  ...
)
```

## Arguments

- x:

  An object of class "dx_itd"

- row.names, optional:

  Not used. Included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- variable:

  Variables to include: "Overall" for all cases, or names in
  `grouping_variables`. Defaults to all.

- label:

  Subgroups to include: levels of the grouping variables, or "Overall".
  Defaults to all.

- measure:

  Names of the measures to include. Defaults to all.

- ...:

  Not used.

## Value

A data frame with one row per measure, variable, and label, with the
estimate, confidence interval, and formatted summary.

## Examples

``` r
data <- dx_heart_failure
data$predicted[seq(5, 261, by = 20)] <- NA
fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
head(as.data.frame(fit))
#> # A tibble: 6 × 12
#>   variable label  measure summary estimate conf_low conf_high fraction conf_type
#>   <chr>    <chr>  <chr>   <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>    
#> 1 Overall  Overa… Diagno… 94.9% …    0.949    0.885     0.983 93/98    Binomial…
#> 2 Overall  Overa… Diagno… 95.1% …    0.951    0.906     0.979 155/163  Binomial…
#> 3 Overall  Overa… Diagno… 95.0% …    0.950    0.916     0.973 248/261  Binomial…
#> 4 Overall  Overa… Pre-Te… 37.5% …    0.375    0.317     0.437 98/261   Binomial…
#> 5 Overall  Overa… Positi… 84.6% …    0.846    0.747     0.918 66/78    Binomial…
#> 6 Overall  Overa… Negati… 84.1% …    0.841    0.777     0.893 143/170  Binomial…
#> # ℹ 3 more variables: notes <chr>, threshold <dbl>, n <int>
```
