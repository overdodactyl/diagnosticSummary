% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_metrics.R
\name{dx_auc_pr}
\alias{dx_auc_pr}
\title{Calculate Area Under the Precision-Recall Curve (AUC-PR)}
\usage{
dx_auc_pr(precision, recall, detail = "full")
}
\arguments{
\item{precision}{Numeric vector of precision values corresponding to different thresholds.}

\item{recall}{Numeric vector of recall values corresponding to different thresholds.}

\item{detail}{Character string specifying the level of detail in the output: "simple"
for just the AUC value, "full" for the AUC value along with confidence intervals.}
}
\value{
Depending on the \code{detail} parameter, returns a single numeric value of AUC or
a data frame with the AUC and its confidence intervals.
}
\description{
This function calculates the Area Under the Curve (AUC) for the Precision-Recall curve
using the trapezoidal rule. It ensures proper alignment of precision and recall values
by adding a starting point at recall=0 with the first observed precision and an ending
point at recall=1 with the last observed precision.
}
\details{
The function prepares the precision and recall vectors by ensuring they are ordered
by increasing recall values. It then calculates the AUC using the trapezoidal rule,
which is the sum of areas of trapezoids formed between each consecutive pair of points.
The first and last points are added to cover the entire recall range from 0 to 1.
}
\examples{
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
}
\concept{metrics}
