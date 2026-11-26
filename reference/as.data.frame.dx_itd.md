# Convert to a data frame

Convert to a data frame

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

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see make.names) is optional.

- variable:

  Variable to include in returned values

- label:

  Labels to include in returned values

- measure:

  Measures to include

- ...:

  additional arguments to be passed to or from methods

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
