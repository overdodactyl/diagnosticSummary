% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_metrics.R
\name{sensitivity}
\alias{sensitivity}
\alias{dx_sensitivity}
\alias{dx_recall,}
\alias{dx_tpr}
\alias{dx_recall}
\title{Calculate Sensitivity (True Positive Rate, Recall)}
\usage{
dx_sensitivity(cm, detail = "full", ...)

dx_recall(cm, detail = "full", ...)

dx_tpr(cm, detail = "full", ...)
}
\arguments{
\item{cm}{A dx_cm object created by \code{\link[=dx_cm]{dx_cm()}}.}

\item{detail}{Character specifying the level of detail in the output:
"simple" for raw estimate, "full" for detailed estimate
including 95\% confidence intervals.}

\item{...}{Additional arguments to pass to metric_binomial function, such as
\code{citype} for type of confidence interval method.}
}
\value{
Depending on the \code{detail} parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with
detailed diagnostics including confidence intervals and possibly other
metrics relevant to understanding the metric.
}
\description{
Calculates Sensitivity, also known as the True Positive Rate (TPR) or recall, which
is the proportion of actual positives that are correctly identified as such by the
classifier. Sensitivity is a key measure in evaluating the effectiveness of a classifier
in identifying positive instances.
}
\details{
Sensitivity or TPR is an important measure in scenarios where missing a positive
identification has serious consequences. It essentially measures the proportion of
actual positives that are correctly identified, giving insight into the ability of
the classifier to detect positive instances. A higher sensitivity indicates a better
performance in recognizing positive instances.

The formula for Sensitivity is:
\deqn{Sensitivity = \frac{True Positives}{True Positives + False Negatives}}{Sensitivity = TP / (TP + FN)}
}
\examples{
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
simple_sensitivity <- dx_sensitivity(cm, detail = "simple")
detailed_sensitivity <- dx_sensitivity(cm)
print(simple_sensitivity)
print(detailed_sensitivity)
}
\seealso{
\code{\link[=dx_cm]{dx_cm()}} to understand how to create and interact with a
'dx_cm' object.
}
\concept{metrics}
