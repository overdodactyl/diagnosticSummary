#' Generate a dataframe of diagnostic performance over the range of thresholds specified in the main dx function call.
#'
#' @param dx_obj An object of class dx
#' @param measures Vector of diagnostic measures to include
#' @param fraction Logical. Should fractions be included for measures (where applicable)
#' @noRd
#' @keywords internal
# dx_diagnostic_table <- function(dx_obj, measures = c("AUC", "Accuracy", "Sensitivity",
#                                                       "Specificity","Positive Predictive Value",
#                                                       "Negative Predictive Value","Odds Ratio", "F1 Score"),
#                                 fraction = TRUE) {
#
#   # Filter and transform the operating data
#   operatingdata <- dx_obj$measures[dx_obj$measures$variable == "Overall", ]
#
#   if (fraction) {
#     operatingdata$fraction <- ifelse(nchar(as.character(operatingdata$fraction)) == 0,
#                                      operatingdata$fraction,
#                                      paste0(" ", operatingdata$fraction))
#
#     # Combine estimate and fraction if needed
#     operatingdata$combined_summary <- paste0(operatingdata$estimate, operatingdata$fraction)
#   } else {
#     operatingdata$combined_summary <- operatingdata$estimate
#   }
#
#
#
#   # Select and reshape the data to wide format
#   operatingdata_wide <- stats::reshape(operatingdata,
#                                 timevar = "measure",
#                                 idvar = "threshold",
#                                 direction = "wide")
#
#   # Select the desired columns based on the selected measures
#   cols_to_select <- c("threshold", paste("combined_summary", measures, sep = "."))
#   operatingdata_wide <- operatingdata_wide[, cols_to_select, drop = FALSE]
#   names(operatingdata_wide) <- gsub("combined_summary\\.", "", names(operatingdata_wide))
#
#   operatingdata_wide <- operatingdata_wide[order(operatingdata_wide$threshold), ]
#
#   return(operatingdata_wide)
# }

