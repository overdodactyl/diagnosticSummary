# Calculate Area Under the Precision-Recall Curve (AUC-PR)

This function calculates the Area Under the Curve (AUC) for the
Precision-Recall curve using the trapezoidal rule. It ensures proper
alignment of precision and recall values by adding a starting point at
recall=0 with the first observed precision and an ending point at
recall=1 with the last observed precision.

## Usage

``` r
dx_auc_pr(precision, recall, detail = "full")
```

## Arguments

- precision:

  Numeric vector of precision values corresponding to different
  thresholds.

- recall:

  Numeric vector of recall values corresponding to different thresholds.

- detail:

  Character string specifying the level of detail in the output:
  "simple" for just the AUC value, "full" for the AUC value along with
  confidence intervals.

## Value

Depending on the `detail` parameter, returns a single numeric value of
AUC or a data frame with the AUC and its confidence intervals.

## Details

The function prepares the precision and recall vectors by ensuring they
are ordered by increasing recall values. It then calculates the AUC
using the trapezoidal rule, which is the sum of areas of trapezoids
formed between each consecutive pair of points. The first and last
points are added to cover the entire recall range from 0 to 1.

## Examples

``` r
# Assuming pr_data is your dataframe with precision and recall columns
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  setthreshold = .3
)
auc_pr <- dx_auc_pr(dx_obj$thresholds$precision, dx_obj$thresholds$sensitivity)
print(auc_pr)
#> # A tibble: 1 × 8
#>   measure summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>   <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>
#> 1 AUC PR  0.87       0.873 NA       NA        ""       ""        ""   
```
