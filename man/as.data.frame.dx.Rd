% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_utils.R
\name{as.data.frame.dx}
\alias{as.data.frame.dx}
\title{Convert to a data frame}
\usage{
\method{as.data.frame}{dx}(
  x,
  row.names = NULL,
  optional = TRUE,
  thresh = NA,
  variable = NA,
  label = NA,
  measure = NA,
  ...
)
}
\arguments{
\item{x}{An object of class "dx"}

\item{row.names}{NULL or a character vector giving the row names for the
data frame. Missing values are not allowed.}

\item{optional}{logical. If TRUE, setting row names and converting column
names (to syntactic names: see make.names) is optional.}

\item{thresh}{The threshold to return values from}

\item{variable}{Variable to include in returned values}

\item{label}{Labels to include in returned values}

\item{measure}{Measures to include}

\item{...}{additional arguments to be passed to or from methods}
}
\description{
Convert to a data frame
}
