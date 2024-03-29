% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx_constructor.R
\name{dx}
\alias{dx}
\title{Set options for diagnostic analysis}
\usage{
dx(
  data,
  classlabels = c("Negative", "Positive"),
  threshold_range = NA,
  outcome_label = NA,
  pred_varname,
  true_varname,
  setthreshold = 0.5,
  poslabel = 1,
  grouping_variables = NA,
  citype = "exact",
  bootreps = 2000,
  bootseed = 20191015,
  doboot = FALSE,
  direction = "auto",
  ...
)
}
\arguments{
\item{data}{A tbl.}

\item{classlabels}{Labels for predicted variable.  Needs to be 0, 1 order.}

\item{threshold_range}{Optional. A numeric vector of thresholds to loop over.}

\item{outcome_label}{Label for outcome (string)}

\item{pred_varname}{Column name containing AI prediction (string)}

\item{true_varname}{Column name containing AI reference standard (string)}

\item{setthreshold}{A numeric value representing the threshold used
to identify AI prediction}

\item{poslabel}{Positive class.  Variable should be coded as 0/1
with 1 being the event}

\item{grouping_variables}{Character vector of variable names to
be summarized by.  These variables should all be factors.}

\item{citype}{Confidence interval type.}

\item{bootreps}{Number of bootstrap samples used to generate F1 score CI}

\item{bootseed}{Seed value to be used when calculating bootsraped CI's}

\item{doboot}{Logical. Generate bootstrap estimate of F1 confidence interval?}

\item{direction}{Direction for roc comparison.  See ?pROC::roc}

\item{...}{currently unused}
}
\description{
Use this function to return a list of tuning parameters to analyze your
diagnostic test
}
