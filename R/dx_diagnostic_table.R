#' Generate a dataframe of diagnostic performance over the range of thresholds specified in the main dx function call.
#'
#' @param dx_obj An object of class dx
#' @param includeFractions Logical for inclusion of the fractions appended to the estimates and CIs
#' @param includeAUC Logical to include AUC in data frame
#' @param includeAccuracy Logical to include Accuracy in data frame
#' @param includeSensitivity Logical to include sensitivity in data frame
#' @param includeSpecificity Logical to include specificity in data frame
#' @param includePPV Logical to include positive predictive value in data frame
#' @param includeNPV Logical to include negative predictive value in data frame
#' @param includeOR Logical to include Odds Ratio in data frame
#' @param includeF1 Logical to include F1 in data frame
#' @export
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   study_name = "Heart Attack Prediction",
#'   data_description = "Validation Data",
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_diagnostic_table(dx_obj, includeOR=FALSE, includeF1=FALSE)


dx_diagnostic_table <- function(dx_obj,
                             includeFractions = TRUE,
                             includeAUC=TRUE,
                             includeAccuracy = TRUE,
                             includeSensitivity = TRUE,
                             includeSpecificity = TRUE,
                             includePPV = TRUE,
                             includeNPV = TRUE,
                             includeOR = TRUE,
                             includeF1 = TRUE){

 select_string <- c("threshold")
 if (includeAUC) {select_string <- c(select_string, "AUC")}
 if (includeAccuracy) {select_string <- c(select_string, "Accuracy")}
 if (includeSensitivity) {select_string <- c(select_string, "Sensitivity")}
 if (includeSpecificity) {select_string <- c(select_string, "Specificity")}
 if (includePPV) {select_string <- c(select_string, "Positive Predictive Value")}
 if (includeNPV) {select_string <- c(select_string, "Negative Predictive Value")}
 if (includeOR) {select_string <- c(select_string, "Odds Ratio")}
 if (includeF1) {select_string <- c(select_string, "F1 Score")}


 operatingdata <- dx_obj$measures   %>%
   filter(variable=="Overall")

 if (includeFractions) {

operatingdata <- operatingdata %>%
   mutate(
     fraction = case_when(
       nchar(fraction) == 0 ~ fraction,
       TRUE ~ paste("", fraction, sep=" ")
     )
   )
 }

operatingdata <- operatingdata  %>%
   unite("combined_summary",estimate, fraction, na.rm=TRUE, remove=TRUE, sep="") %>%
   select(threshold, measure, combined_summary) %>%
   spread(key = measure, value = combined_summary) %>%
   select(all_of(select_string))

 return(operatingdata)
}
