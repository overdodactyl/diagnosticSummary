#' Set options for diagnostic analysis
#'
#' Use this function to return a list of tuning parameters to analyze your
#' diagnostic test
#'
#' @param data A tbl.
#' @param study_name Name of study to be used in the report (string)
#' @param data_description Description of data to be used in the report (string)
#' @param classlabels Labels for predicted variable.  Needs to be 0, 1 order.
#' @param pred_varname Colomn name containing AI prediction (string)
#' @param setthreshold A numeric value represnting the threshold used to identify AI prediction
#' @param threshold_range Optional. A numeric vector of thresholds to loop over.
#' @param true_varname Column name containt AI reference standard (string)
#' @param poslabel Positive class.  Variable should be coded as 0/1 with 1 being the event
#' @param outcome_label Label for outcome (string)
#' @param grouping_variables Character vector of variable names to be summarized by.
#' These variables should all be factors.
#' @param citpye Confidence interval type.
#' @param bootreps Number of bootstrap samples used to generate F1 score CI
#' @param bootseed Seed value to be used when calculating bootsraped CI's
#' @param doboot Logical.  Generate bootstrap estimate of F1 confidence interval?
#' @param roc_curve_color Color of ROC curve
#' @param roc_text_color Color of text on ROC curve
#' @param roc_add_text (Logical) Include stats on ROC curve?
#' @param roc_add_ref_lines (Logical) Include reference lines on ROC curve?
#' @param roc_add_fractions (Logical) Include fractions on ROC curve?
#' @param roc_summary_stats Statistics to include on ROC curve
#' @param roc_filename Name of file output for ROC pdf file
#'
#' @export

dx_set_options <- function(data, study_name, data_description, classlabels = c("Negative", "Positive"),
                           threshold_range = NA, outcome_label,
                           pred_varname, true_varname, setthreshold = .5, poslabel = 1, grouping_variables = NA,
                           citype = "exact", bootreps = 2000, bootseed = 20191015, doboot = FALSE,
                           roc_curve_color = "red", roc_text_color = "black", roc_add_text = TRUE,
                           roc_add_ref_lines = TRUE, roc_add_fractions = TRUE,
                           roc_summary_stats = c(1,2,3,4,5,6,7,8),
                           roc_filename = paste0(study_name, "_ROC", data_description,".pdf")) {


  # Check if pred_varname in data
  if (! pred_varname %in% names(data)) {
    stop(paste(pred_varname, "was not found in `data`"))
  }

  # Check if true_varname in data
  if (! true_varname %in% names(data)) {
    stop(paste(true_varname, "was not found in `data`"))
  }

  # Check if boot available when needed
  if (doboot & !requireNamespace("boot", quietly = TRUE)) {
    stop("boot package not installed. Install package or use doboot = FALSE")
  }

  # Check if grouping variables are factors
  if (!identical(grouping_variables,NA)) {
    for(f in grouping_variables) {
      if (!is.factor(working_df[[f]])) {
        stop("All variables in `grouping_variables` should be a factor.")
      }
    }
  }

  # Check if pred_varname is numeric
  if (!is.numeric(data[[pred_varname]])) {
    stop(paste(pred_varname, "should be numeric"))
  }

  # Check if true_varname is consists of only c(0,1)
  if (!all(data[[true_varname]] %in% c(0,1))) {
    stop(paste(true_varname, "should be numeric vector consisting of only 0's and 1's"))
  }

  data$test_binary <- ifelse(eval(parse(text=paste0("data$", pred_varname))) < setthreshold, 0, 1)
  data$test_binaryf <- factor(data$test_binary, levels =  c(0,1), labels=classlabels)
  data$true_binaryf <- factor(eval(parse(text=paste0("data$", true_varname))),
                              levels=c(0,1), labels=classlabels)




  c(as.list(environment()))

}
