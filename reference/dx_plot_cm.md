# Plot Confusion Matrix with Metrics

Creates a graphical representation of a confusion matrix from a `dx`
object. The plot displays the counts of true positives, false negatives,
true negatives, and false positives. Additionally, it annotates the plot
with relevant performance metrics such as Sensitivity, Specificity, PPV,
and NPV, along with their confidence intervals where applicable.

## Usage

``` r
dx_plot_cm(dx_obj, palette = c("#e5eef7", "#0057B8"), levels = c("-", "+"))
```

## Arguments

- dx_obj:

  A `dx` object, created with
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md).

- palette:

  A character vector of length 2 specifying the colors for the low and
  high ends of the fill gradient used in the plot. Defaults to
  c("#e5eef7", "#0057B8").

- levels:

  A character vector of length 2 specifying the labels for negative and
  positive classes, respectively. Defaults to c("-", "+").

## Value

A `ggplot` object that represents the confusion matrix with additional
performance metrics.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)
dx_plot_cm(dx_obj)
```
