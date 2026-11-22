# Shared Parameters for Diagnostic Metrics

Shared Parameters for Diagnostic Metrics

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

- ...:

  Additional arguments to pass to metric_binomial function, such as
  `citype` for type of confidence interval method.

- boot:

  Logical specifying if confidence intervals should be generated via
  bootstrapping. Note, this can be slow.

- bootreps:

  The number of bootstrap replications for calculating confidence
  intervals.

- predprob:

  Numeric vector of predicted probabilities associated with the positive
  class.

- truth:

  Numeric vector of true binary outcomes, typically 0 or 1, with the
  same length as `predprob`.

- dx1:

  A `dx` object

- dx2:

  A `dx` object

## Value

Depending on the `detail` parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with detailed
diagnostics including confidence intervals and possibly other metrics
relevant to understanding the metric.
