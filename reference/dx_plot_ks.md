# Plot Kolmogorov-Smirnov Curve

Generates a Kolmogorov-Smirnov (KS) plot from a `dx` object. The KS plot
is a graphical tool used to assess the discriminatory power of a binary
classification model by visualizing the difference between the
cumulative distribution functions of the true positive rate (TPR) and
false positive rate (FPR) across different thresholds.

## Usage

``` r
dx_plot_ks(dx_obj)
```

## Arguments

- dx_obj:

  A `dx` object, created with
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md).

## Value

A `ggplot` object representing the KS plot, which can be further
customized as needed.

## Details

The KS plot displays two lines representing the cumulative TPR and FPR,
with the x-axis showing the prediction scores in descending order and
the y-axis showing the cumulative proportion of positive and negative
instances. The point where the difference between the TPR and FPR is
maximal indicates the threshold with the highest potential for
separating positive from negative instances. This point is marked with a
dashed vertical line on the plot, and the KS statistic, representing the
maximum distance between the two lines, is annotated on the plot. A
higher KS statistic indicates a model with better discriminatory
ability. The ideal model would have a KS plot with the TPR line close to
the top-left corner and the FPR line close to the bottom-right corner,
maximizing the distance between the two lines.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)
dx_plot_ks(dx_obj)
```
