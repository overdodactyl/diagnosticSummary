# Plot Precision-Recall Curve

Plots a Precision-Recall (PR) curve from a `dx` object. The PR curve is
a useful tool for evaluating the performance of a binary classification
model, especially in situations where classes are imbalanced. It shows
the trade-off between precision and recall for different thresholds.

## Usage

``` r
dx_plot_pr(dx_obj)
```

## Arguments

- dx_obj:

  A `dx` object containing diagnostic measurements, including a data
  frame with sensitivity (recall) and precision values at various
  thresholds.

## Value

A `ggplot` object representing the PR curve, which can be further
customized as needed.

## Details

The PR curve is a plot that demonstrates the relationship between
precision (the proportion of true positive predictions among all
positive predictions) and recall (the proportion of true positive
predictions among all actual positives), for each possible cut-off
threshold. A model with perfect performance would show a line that
reaches the top-right corner of the plot, indicating both high precision
and recall.

Between neighboring thresholds, the curve uses the interpolation of
Davis and Goadrich (2006) rather than a straight line: false positives
are assumed to increase in proportion to true positives, so precision
follows a curve. This matters mostly when predictions are tied. The
curve runs from no predictions positive to all predictions positive, and
the area under it is the AUC-PR from
[`dx_auc_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc_pr.md).

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)
dx_plot_pr(dx_obj)
```
