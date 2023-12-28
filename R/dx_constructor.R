#' Set options for diagnostic analysis
#'
#' Use this function to return a list of tuning parameters to analyze your
#' diagnostic test
#'
#' @param data A tbl.
#' @param classlabels Labels for predicted variable.  Needs to be 0, 1 order.
#' @param pred_varname Column name containing AI prediction (string)
#' @param setthreshold A numeric value representing the threshold used
#'     to identify AI prediction
#' @param threshold_range Optional. A numeric vector of thresholds to loop over.
#' @param true_varname Column name containing AI reference standard (string)
#' @param poslabel Positive class.  Variable should be coded as 0/1
#'     with 1 being the event
#' @param outcome_label Label for outcome (string)
#' @param grouping_variables Character vector of variable names to
#'     be summarized by.  These variables should all be factors.
#' @param citype Confidence interval type.
#' @param bootreps Number of bootstrap samples used to generate F1 score CI
#' @param bootseed Seed value to be used when calculating bootsraped CI's
#' @param doboot Logical. Generate bootstrap estimate of F1 confidence interval?
#' @param direction Direction for roc comparison.  See ?pROC::roc
#' @param ... currently unused
#'
#' @export

dx <- function(data,
               classlabels = c("Negative", "Positive"),
               threshold_range = NA, outcome_label, pred_varname, true_varname,
               setthreshold = .5, poslabel = 1, grouping_variables = NA,
               citype = "exact", bootreps = 2000, bootseed = 20191015,
               doboot = FALSE, direction = "auto", ...) {


  # Check if pred_varname in data
  if (!pred_varname %in% names(data)) {
    stop(paste(pred_varname, "was not found in `data`"))
  }

  # Check if true_varname in data
  if (!true_varname %in% names(data)) {
    stop(paste(true_varname, "was not found in `data`"))
  }

  # Check if boot available when needed
  if (doboot & !requireNamespace("boot", quietly = TRUE)) {
    stop("boot package not installed. Install package or use doboot = FALSE")
  }

  # Check if grouping variables are factors
  if (!identical(grouping_variables, NA)) {
    for (f in grouping_variables) {
      if (!is.factor(data[[f]])) {
        stop("All variables in `grouping_variables` should be a factor.")
      }
    }
  }

  # Check if pred_varname is numeric
  if (!is.numeric(data[[pred_varname]])) {
    stop(paste(pred_varname, "should be numeric"))
  }

  # Check if true_varname consists of only c(0,1)
  if (!all(data[[true_varname]] %in% c(0, 1))) {
    stop(paste(
      true_varname,
      "should be numeric vector consisting of only 0's and 1's"
    ))
  }

  data$test_binary <- ifelse(
    eval(parse(text = paste0("data$", pred_varname))) < setthreshold, 0, 1
  )
  data$test_binaryf <- factor(data$test_binary,
    levels = c(0, 1),
    labels = classlabels
  )
  data$true_binaryf <- factor(eval(parse(text = paste0("data$", true_varname))),
    levels = c(0, 1), labels = classlabels
  )

  options <- list(
    classlabels = classlabels,
    threshold_range = threshold_range,
    outcome_label = outcome_label,
    pred_varname = pred_varname,
    true_varname = true_varname,
    setthreshold = setthreshold,
    poslabel = poslabel,
    grouping_variables = grouping_variables,
    citype = citype,
    bootreps = bootreps,
    bootseed = bootseed,
    doboot = doboot
  )

  # Get all unique thresholds
  # Set thresholds may not be in threshold_range
  all_thresholds <- unique(c(options$setthreshold, options$threshold_range))
  all_thresholds <- all_thresholds[!is.na(all_thresholds)]

  # Loop through all thresholds and get measures
  threshold_measures <- list()

  for (i in seq_along(all_thresholds)) {
    threshold_measures[[i]] <- dx_measure(data,
      threshold = all_thresholds[i],
      options = options
    )
  }

  threshold_measures <- do.call(rbind, threshold_measures)

  ####### Subgroup Analysis

  if (!identical(grouping_variables, NA)) {
    subgroups <- list()
    for (i in seq_along(options$grouping_variables)) {
      subgroups[[i]] <- dx_group_measure(
        data = data, options = options,
        group_varname = options$grouping_variables[i]
      )
    }
    subgroups <- do.call(rbind, subgroups)

    threshold_measures <- rbind(subgroups, threshold_measures)
  }

  ####### Threshold Analysis
  threshold_analysis <- dx_thresholds(data, options)

  ####### Prevalence Analysis
  prevalence_analysis <- dx_prevalence_analysis(data, options)


  # Number of unique levels
  n_levels <- length(unique(threshold_measures$Label))

  roc <- get_roc(true_varname, pred_varname, data, direction)


  structure(list(
    data = data,
    options = options,
    measures = threshold_measures,
    thresholds = threshold_analysis,
    prevalence = prevalence_analysis,
    n_levels = n_levels, roc = roc
  ), class = "dx")
}
