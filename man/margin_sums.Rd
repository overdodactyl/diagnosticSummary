% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_tests.R
\name{margin_sums}
\alias{margin_sums}
\title{Calculate Margin Sums for a Contingency Table}
\usage{
margin_sums(stratum_data)
}
\arguments{
\item{stratum_data}{A 2x2 matrix representing a single stratum of the contingency table.}
}
\value{
A list with 'row' and 'col' elements containing the sums of rows and columns, respectively.
}
\description{
Calculates the row and column sums for each 2x2 stratum of the contingency table.
}
\keyword{internal}
