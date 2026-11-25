# Calculate Area Under the Precision-Recall Curve (AUC-PR)

Calculates the area under the precision-recall (PR) curve, using the
interpolation of Davis and Goadrich (2006) between thresholds.

## Usage

``` r
dx_auc_pr(truth, predprob, detail = "full", poslabel = 1)
```

## Arguments

- truth:

  Numeric vector of true binary outcomes (0 or 1).

- predprob:

  Numeric vector of predicted probabilities that the outcome is 1, the
  same length as `truth`.

- detail:

  Character string specifying the level of detail in the output:
  "simple" for just the AUC value, "full" for a data frame of results.

- poslabel:

  The outcome class that is the event: 1 (default) or 0. With
  `poslabel = 0`, a prediction below the threshold is a positive test.

## Value

Depending on the `detail` parameter, returns a single numeric value of
AUC or a data frame with the AUC. Confidence intervals are not
calculated.

## Details

Precision and recall are calculated at every distinct prediction.
Between two neighboring thresholds, true and false positives increase
together, so precision follows a curve rather than a straight line.
Joining the points with straight lines (the trapezoidal rule) overstates
the area, especially when many predictions are tied (Davis and Goadrich,
2006).

Instead, between neighboring thresholds false positives are assumed to
increase in proportion to true positives, which is the expected path
when tied predictions are ordered at random. Precision is integrated
exactly along that path. The curve starts with no predictions positive
and ends with all predictions positive (recall of 1 and precision equal
to the prevalence).

The AUC-PR is `NA` unless both outcome classes are present. A classifier
that guesses at random has an expected AUC-PR equal to the prevalence,
so compare values with the prevalence rather than with 0.5.

The same curve is drawn by
[`dx_plot_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_pr.md).

## References

Davis J, Goadrich M. The relationship between precision-recall and ROC
curves. Proceedings of the 23rd International Conference on Machine
Learning. 2006:233-240.

## Examples

``` r
dx_auc_pr(dx_heart_failure$truth, dx_heart_failure$predicted)
#> # A tibble: 1 × 8
#>   measure summary estimate conf_low conf_high fraction conf_type notes          
#>   <chr>   <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>          
#> 1 AUC PR  0.873      0.873 NA       NA        ""       ""        Davis & Goadri…
dx_auc_pr(dx_heart_failure$truth, dx_heart_failure$predicted, detail = "simple")
#> [1] 0.8727206
```
