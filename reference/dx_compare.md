# Compare Multiple Classification Models

Compares multiple classification models pairwise using various
statistical tests to assess differences in performance metrics. It
supports both paired and unpaired comparisons.

## Usage

``` r
dx_compare(dx_list, paired = TRUE)
```

## Arguments

- dx_list:

  A list of `dx` objects representing the models to be compared. Each
  `dx` object should be the result of a call to
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md).

- paired:

  Logical, indicating whether the comparisons should be treated as
  paired. Paired comparisons are appropriate when models are evaluated
  on the same set of instances (e.g., cross-validation or repeated
  measures).

## Value

A `dx_compare` object containing a list of `dx` objects and a data frame
of pairwise comparison results for each test conducted.

## Details

This function is a utility to perform a comprehensive comparison between
multiple classification models. Based on the value of `paired`, it will
perform appropriate tests. The resulting object can be used it further
functions like `dx_plot_rocs.`

## See also

[`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md),
[`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md),
[`dx_mcnemars()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_mcnemars.md)
for more details on the tests used for comparisons.

## Examples

``` r
dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
dx_list <- list(dx_glm, dx_rf)
dx_comp <- dx_compare(dx_list, paired = TRUE)
print(dx_comp$tests)
#> # A tibble: 2 × 9
#>   models       test  summary p_value estimate conf_low conf_high statistic notes
#>   <chr>        <chr> <chr>     <dbl>    <dbl>    <dbl>     <dbl>     <dbl> <chr>
#> 1 Model 1 vs.… DeLo… 0.04 (… 5.89e-4   0.0413   0.0178    0.0649      3.44 "Z s…
#> 2 Model 1 vs.… McNe… p=0.02  1.80e-2  NA       NA        NA           5.6  ""   
```
