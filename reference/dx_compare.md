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

  A list of two or more `dx` objects, created with
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md),
  representing the models to be compared. Name the list to label the
  models; names must be unique. Unnamed models are called "Model 1",
  "Model 2", and so on.

- paired:

  Logical, indicating whether the comparisons should be treated as
  paired. Paired comparisons are appropriate when models are evaluated
  on the same set of instances (e.g., cross-validation or repeated
  measures).

## Value

A `dx_compare` object containing a list of `dx` objects and a data frame
of pairwise comparison results for each test conducted.

## Details

Every pair of models is compared. With `paired = TRUE`, the tests are
DeLong's test for paired ROC curves
([`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md))
and McNemar's test
([`dx_mcnemars()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_mcnemars.md)).
With `paired = FALSE`, they are DeLong's test for unpaired ROC curves
and a test of two proportions
([`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md))
for accuracy, PPV, NPV, FNR, FPR, FDR, sensitivity, and specificity.
P-values are not adjusted for multiple comparisons. The resulting object
can be passed to
[`dx_plot_rocs()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_rocs.md).

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
