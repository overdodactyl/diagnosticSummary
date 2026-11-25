# Calculate Area Under the ROC Curve (AUC)

Calculates the Area Under the Receiver Operating Characteristic (ROC)
Curve from prediction probabilities and true binary outcomes. AUC is a
measure of the ability of a classifier to distinguish between classes
and is used as a summary of the ROC curve.

## Usage

``` r
dx_auc(truth, predprob, detail = "full", direction = "<")
```

## Arguments

- truth:

  Vector of true binary class outcomes (0 and 1).

- predprob:

  Vector of prediction probabilities corresponding to the true outcomes.

- detail:

  Character string specifying the level of detail in the output:
  "simple" for just the AUC value, "full" for the AUC value along with
  confidence intervals.

- direction:

  Direction of the comparison, passed to
  [`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html). The default,
  "\<", treats higher predicted values as more likely to be positive.
  Use "\>" if lower values indicate the positive class. "auto" lets pROC
  choose the direction that gives an AUC of at least 0.5, which hides
  models that perform worse than chance and biases the AUC upward in
  small samples.

## Value

Depending on the `detail` parameter, returns a single numeric value of
AUC or a data frame with the AUC and its confidence intervals.

## Examples

``` r
simple_auc <- dx_auc(dx_heart_failure$truth, dx_heart_failure$predicted, detail = "simple")
detailed_auc <- dx_auc(dx_heart_failure$truth, dx_heart_failure$predicted)
print(simple_auc)
#> [1] 0.9036559
print(detailed_auc)
#> # A tibble: 1 × 8
#>   measure summary           estimate conf_low conf_high fraction conf_type notes
#>   <chr>   <chr>                <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 AUC ROC 0.904 (0.864, 0.…    0.904    0.864     0.943 ""       DeLong    ""   
```
