#' Heart disease outcomes and predictions
#'
#' A data set containing heart disease outcomes for 261 patients along with
#' predicted probabilities of having heart disease from two models.
#'
#' @format A data frame with 261 rows and 6 variables:
#' \describe{
#'   \item{AgeGroup}{Age group}
#'   \item{Sex}{Sex}
#'   \item{truth}{Heart disease (outcome)}
#'   \item{predicted}{Predicted probability from a logistic regression model}
#'   \item{predicted_rf}{Predicted probability from a random forest model}
#'   \item{AgeSex}{Age and sex group}
#' }
#' @details The outcomes come from the Hungarian Institute of Cardiology
#'     records in the UCI Heart Disease data set (complete cases only). The
#'     predictions were fit to the same data, so they are for illustration
#'     only. See `data-raw/dx_heart_failure.R` in the package source.
#' @source Janosi A, Steinbrunn W, Pfisterer M, Detrano R (1989). Heart
#'     Disease. UCI Machine Learning Repository. \doi{10.24432/C52P4X}.
#'     Licensed under CC BY 4.0.
"dx_heart_failure"
