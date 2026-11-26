# Plot Decision Curve

Generates a decision curve to visualize the net benefit of a binary
classification model across a specified range of threshold
probabilities. The decision curve shows the net benefit of using the
model at different threshold probabilities compared to the strategy of
treating all or none.

## Usage

``` r
dx_plot_decision_curve(dx_obj, plot_range = c(0.05, 0.95))
```

## Arguments

- dx_obj:

  A `dx` object.

- plot_range:

  A numeric vector of length 2 specifying the lower and upper bounds,
  between 0 and 1, for the range of threshold probabilities to be
  plotted. Defaults to c(0.05, .95).

## Value

A `ggplot` object representing the decision curve, which can be further
customized as needed.

## Details

Decision Curve Analysis (DCA) is a method for evaluating and comparing
the clinical usefulness of prediction models by considering the clinical
consequences of decision making. The decision curve plots the net
benefit of using the model across a range of threshold probabilities for
making a decision. The net benefit is calculated as the proportion of
true positives minus the weighted proportion of false positives, where
the weight is the ratio of the cost of false positives relative to the
benefit of true positives. This ratio is derived from the threshold
probability (Vickers & Elkin, 2006): \$\$Net Benefit = \frac{TP}{N} -
Weight \times \frac{FP}{N}\$\$ where Weight = Threshold / (1 -
Threshold) and N is the total number of observations. The threshold is
the probability of the event, so with `poslabel = 0` it is 1 minus the
threshold on the predictions. Net benefit is calculated on an even grid
of thresholds across `plot_range` and at each distinct prediction, so
the curve is also drawn for models with few distinct predictions, such
as 0/1 classes.

The decision curve will typically be compared against two default
strategies: treating all patients or treating none. The net benefit is
higher when the model's predictions provide a clear advantage over these
default strategies. A well-performing model will have a decision curve
that stays above the treat-none and treat-all lines for a range of
clinically relevant threshold probabilities.

The 'plot_range' parameter allows limiting the analysis to a relevant
range of thresholds, avoiding the extremes where the net benefit
calculation can become unstable or not clinically relevant.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3
)
dx_plot_decision_curve(dx_obj)
```
