% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_utils.R
\name{summary.dx}
\alias{summary.dx}
\title{Summary}
\usage{
\method{summary}{dx}(
  object,
  thresh = object$options$setthreshold,
  variable = NA,
  label = NA,
  show_var = T,
  show_label = T,
  measure = NA,
  ...
)
}
\arguments{
\item{object}{An object of class "dx"}

\item{thresh}{The threshold to return values from}

\item{variable}{Variable to include in returned values}

\item{label}{Labels to include in returned values}

\item{show_var}{Include variable column in returned data?}

\item{show_label}{Include label in returned data?}

\item{measure}{Measures to include}

\item{...}{additional arguments to be passed to or from methods}
}
\description{
Summary
}
