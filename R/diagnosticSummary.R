#' diagnosticSummary: Diagnostic Accuracy Summaries for Binary Classifiers
#'
#' Summarizes how well predicted probabilities agree with a binary reference
#' standard. [dx()] calculates the measures, with confidence intervals,
#' overall, across thresholds, and by subgroup. [dx_itd()] does the same when
#' some predictions are missing, [dx_compare()] compares models, and the
#' `dx_plot_*()` functions draw the results. Each measure is also available
#' as its own function, such as [dx_sensitivity()], from a confusion matrix
#' made with [dx_cm()].
#'
#' @keywords internal
"_PACKAGE"

## quiets concerns of R CMD check for variables used in ggplot2
utils::globalVariables(c(".data"))
