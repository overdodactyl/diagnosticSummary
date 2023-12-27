#' Generate a dataframe of diagnostic performance over the range of thresholds specified in the main dx function call.
#'
#' @param dx_obj An object of class dx
#' @param includeFractions Logical for inclusion of the fractions appended to the estimates and CIs
#' @param includeAUC Logical to include AUC in data frame
#' @param includeAccuracy Logical to include Accuracy in data frame
#' @param includeSensitivity Logical to include sensitivity in data frame
#' @param includeSpecificity Logical to include Specificity in data frame
#' @param includePPV Logical to include Positive predictive value in data frame
#' @param includeNPV Logical to include Negative predictive value in data frame
#' @param includeOR Logical to include Odds Ratio in data frame
#' @param includeF1 Logical to include F1 in data frame
#' @export
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_diagnostic_table(dx_obj, includeOR = FALSE, includeF1 = FALSE)
dx_diagnostic_table <- function(dx_obj,
                                includeFractions = TRUE,
                                includeAUC = TRUE,
                                includeAccuracy = TRUE,
                                includeSensitivity = TRUE,
                                includeSpecificity = TRUE,
                                includePPV = TRUE,
                                includeNPV = TRUE,
                                includeOR = TRUE,
                                includeF1 = TRUE) {

  # Define all possible measures and their respective inclusion flags in a named vector
  includeMeasures <- c(includeAUC, includeAccuracy, includeSensitivity,
                       includeSpecificity, includePPV, includeNPV,
                       includeOR, includeF1)

  measureNames <- c("AUC", "Accuracy", "Sensitivity", "Specificity",
                    "Positive Predictive Value", "Negative Predictive Value",
                    "Odds Ratio", "F1 Score")

  # Select the measures that are included
  selectedMeasures <- measureNames[includeMeasures]

  # Filter and transform the operating data
  operatingdata <- dx_obj$measures[dx_obj$measures$variable == "Overall", ]

  if (includeFractions) {
    operatingdata$fraction <- ifelse(nchar(as.character(operatingdata$fraction)) == 0,
                                     operatingdata$fraction,
                                     paste(" ", operatingdata$fraction))
  }

  # Combine estimate and fraction if needed
  operatingdata$combined_summary <- paste0(operatingdata$estimate, operatingdata$fraction)

  # Select and reshape the data to wide format
  operatingdata_wide <- reshape(operatingdata,
                                timevar = "measure",
                                idvar = "threshold",
                                direction = "wide")

  # Select the desired columns based on the selected measures
  cols_to_select <- c("threshold", paste("combined_summary", selectedMeasures, sep = "."))
  operatingdata_wide <- operatingdata_wide[, cols_to_select, drop = FALSE]
  names(operatingdata_wide) <- gsub("combined_summary\\.", "", names(operatingdata_wide))

  return(operatingdata_wide)
}

