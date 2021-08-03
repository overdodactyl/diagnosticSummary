#' \code{diagnosticSummary} package
#'
#' Diagnostic Summaries
#'
#' @docType package
#' @name diagnosticSummary
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(
  ".",
  "ci_type",
  "estimate",
  "group",
  "label",
  "measure",
  "AUC",
  "Odds Ratio",
  "Sensitivity",
  "Specificity",
  "sensitivity",
  "specificity",
  "variable",
  "variable_label",
  "Level",
  "bootseed",
  "lower",
  "n",
  "obj",
  "rawestime",
  "rawlci",
  "rawuci",
  "testresult",
  "text",
  "threshold",
  "trueresult",
  "upper",
  "counts",
  " ",
  "x",
  "numden",
  "y",
  "fraction",
  "combined_summary"
))
