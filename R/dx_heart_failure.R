#' Heart attack outcomes and predictions
#'
#' A data set containing heart attack outcomes for 261 patients along with
#' predicted probabilities of having a heart attack.
#'
#' @format A data frame with 261 rows and 5 variables:
#' \describe{
#'   \item{AgeGroup}{Age group}
#'   \item{Sex}{sex}
#'   \item{truth}{Heart failure (outcome)}
#'   \item{predicted}{Predicted outcome from a GLM model}
#'   \item{predicted_rf}{Predicted outcome from a Random Forest model}
#'   \item{AgeSex}{Age and sex group}
#' }
#' @source \url{https://www.kaggle.com/imnikhilanand/heart-attack-prediction}
"dx_heart_failure"
