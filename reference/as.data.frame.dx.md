# Convert a diagnostic summary to a data frame

Returns the measures in a `dx` object, with unformatted estimates and
confidence limits.

## Usage

``` r
# S3 method for class 'dx'
as.data.frame(
  x,
  row.names = NULL,
  optional = TRUE,
  thresh = NA,
  variable = NA,
  label = NA,
  measure = NA,
  ...
)
```

## Arguments

- x:

  An object of class "dx"

- row.names, optional:

  Not used. Included for compatibility with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- thresh:

  Thresholds to include: `setthreshold` or values in `threshold_range`.
  Defaults to all.

- variable:

  Variables to include: "Overall" for all cases, or names in
  `grouping_variables`. Defaults to all.

- label:

  Subgroups to include: levels of the grouping variables, or "Overall".
  Defaults to all.

- measure:

  Names of the measures to include, such as "Sensitivity". Defaults to
  all.

- ...:

  Not used.

## Value

A data frame with one row per measure, variable, label, and threshold,
with the estimate, confidence interval, and formatted summary.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)
head(as.data.frame(dx_obj))
#> # A tibble: 6 × 12
#>   variable label  measure summary estimate conf_low conf_high fraction conf_type
#>   <chr>    <chr>  <chr>   <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>    
#> 1 Overall  Overa… AUC ROC 0.904 …    0.904    0.864     0.943 ""       DeLong   
#> 2 Overall  Overa… Accura… 79.3% …    0.793    0.739     0.841 "207/26… Binomial…
#> 3 Overall  Overa… Sensit… 84.7% …    0.847    0.760     0.912 "83/98"  Binomial…
#> 4 Overall  Overa… Specif… 76.1% …    0.761    0.688     0.824 "124/16… Binomial…
#> 5 Overall  Overa… Positi… 68.0% …    0.680    0.590     0.762 "83/122" Binomial…
#> 6 Overall  Overa… Negati… 89.2% …    0.892    0.828     0.938 "124/13… Binomial…
#> # ℹ 3 more variables: notes <chr>, threshold <dbl>, n <int>
as.data.frame(dx_obj, measure = c("Sensitivity", "Specificity"))
#> # A tibble: 2 × 12
#>   variable label  measure summary estimate conf_low conf_high fraction conf_type
#>   <chr>    <chr>  <chr>   <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>    
#> 1 Overall  Overa… Sensit… 84.7% …    0.847    0.760     0.912 83/98    Binomial…
#> 2 Overall  Overa… Specif… 76.1% …    0.761    0.688     0.824 124/163  Binomial…
#> # ℹ 3 more variables: notes <chr>, threshold <dbl>, n <int>
```
