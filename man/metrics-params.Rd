% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_metrics.R
\name{metrics-params}
\alias{metrics-params}
\alias{metricparams}
\title{Shared Parameters for Diagnostic Metrics}
\usage{
metricparams()
}
\arguments{
\item{cm}{A dx_cm object created by \code{\link[=dx_cm]{dx_cm()}}.}

\item{detail}{Character specifying the level of detail in the output:
"simple" for raw estimate, "full" for detailed estimate
including 95\% confidence intervals.}

\item{...}{Additional arguments to pass to metric_binomial function, such as
\code{citype} for type of confidence interval method.}

\item{boot}{Logical specifying if confidence intervals should be generated
via bootstrapping.  Note, this can be slow.}

\item{bootreps}{The number of bootstrap replications for calculating confidence intervals.}

\item{predprob}{Numeric vector of predicted probabilities associated with the positive class.}

\item{truth}{Numeric vector of true binary outcomes, typically 0 or 1, with the same length as \code{predprob}.}

\item{dx1}{A \code{dx} object}

\item{dx2}{A \code{dx} object}
}
\value{
Depending on the \code{detail} parameter, returns a numeric value
representing the calculated metric or a data frame/tibble with
detailed diagnostics including confidence intervals and possibly other
metrics relevant to understanding the metric.
}
\description{
Shared Parameters for Diagnostic Metrics
}
\keyword{internal}
