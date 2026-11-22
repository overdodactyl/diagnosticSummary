# Calculate Brier Score

The Brier score is a proper score function that measures the accuracy of
probabilistic predictions. It is applicable to tasks in which
predictions must assign probabilities to a set of mutually exclusive
discrete outcomes. For binary classification, the Brier score is a
measure of how far the predicted probabilities are from the actual
outcomes.

## Usage

``` r
dx_brier(predprob, truth, detail = "full")
```

## Arguments

- predprob:

  Numeric vector of predicted probabilities associated with the positive
  class.

- truth:

  Numeric vector of true binary outcomes, typically 0 or 1, with the
  same length as `predprob`.

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.

## Details

The formula for the Brier score in a binary classification is:

\$\$BS = \frac{1}{N} \sum\_{i=1}^{N} (f_i - o_i)^2\$\$

where:

- \\N\\ is the number of predictions,

- \\f_i\\ is the predicted probability of the occurrence of the positive
  class for the ith prediction,

- \\o_i\\ is the actual outcome for the ith prediction, 0 or 1.

The Brier score ranges from 0 to 1, where 0 represents a perfect model
and 1 represents the worst model. It is equivalent to the mean squared
error used in regression and can be decomposed into calibration loss,
refinement loss, and uncertainty. This makes it a very informative
metric for probabilistic forecasts, providing a nuanced view of the
model's predictive performance.

## Examples

``` r
predprob <- dx_heart_failure$predicted
truth <- dx_heart_failure$truth
simple <- dx_brier(predprob, truth, detail = "simple")
detailed <- dx_brier(predprob, truth)
print(simple)
#> [1] 0.1137428
print(detailed)
#> # A tibble: 1 × 8
#>   measure     summary estimate conf_low conf_high fraction conf_type notes      
#>   <chr>       <chr>      <dbl> <lgl>    <lgl>     <chr>    <chr>     <chr>      
#> 1 Brier Score 0.11       0.114 NA       NA        ""       ""        CIs not ye…
```
