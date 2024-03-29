% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_tests.R
\name{dx_mcnemars}
\alias{dx_mcnemars}
\title{McNemar's Chi-squared Test for Paired Proportions}
\usage{
dx_mcnemars(dx1, dx2, detail = "full")
}
\arguments{
\item{dx1}{The first \code{dx} object containing predictions and truth values.}

\item{dx2}{The second \code{dx} object containing predictions and truth values.}

\item{detail}{A string indicating the level of detail for the output: "simple" for
just the p-value, "full" for a comprehensive result including test statistics.}
}
\value{
Depending on the \code{detail} parameter, returns either the p-value (simple)
or a more comprehensive list including the test statistic and p-value (full).
}
\description{
Performs McNemar's test to evaluate the difference between two paired proportions.
This is typically used in the context of binary classification to test whether the
proportion of correct classifications significantly differs between two classifiers
on the same set of instances.
}
\details{
McNemar's test is appropriate when comparing the classification results of two
algorithms on the same data set (paired design). It specifically tests the null
hypothesis that the marginal probabilities of row and column variable are the same.

This test is suitable for binary classification tasks where you are comparing the
performance of two classifiers over the same instances. It's not appropriate for
independent samples or more than two classifiers.

The function expects the input as two \code{dx} objects, each containing the predictions
and truth values from the classifiers being compared. It calculates the contingency
table based on the agreements and disagreements between the classifiers and applies
McNemar's test to this table.
}
\examples{
dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
dx_mcnemars(dx_glm, dx_rf)
}
\seealso{
\code{\link[=dx_cm]{dx_cm()}}, \code{\link[=mcnemar.test]{mcnemar.test()}}
}
\concept{tests}
