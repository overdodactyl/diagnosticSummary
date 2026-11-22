#' Set options for diagnostic analysis
#'
#' Use this function to return a list of tuning parameters to analyze your
#' diagnostic test
#'
#' @param data A tbl.
#' @param classlabels Labels for predicted variable.  Needs to be 0, 1 order.
#' @param pred_varname Column name containing AI prediction (string). Values
#'     should be predicted probabilities between 0 and 1, with no missing values.
#' @param setthreshold A numeric value representing the threshold used
#'     to identify AI prediction
#' @param threshold_range Optional. A numeric vector of thresholds to loop over.
#' @param true_varname Column name containing AI reference standard (string).
#'     Values should be 0 or 1 (numeric, logical, factor, or character), with
#'     no missing values, and both classes must be present.
#' @param poslabel Positive class.  Variable should be coded as 0/1
#'     with 1 being the event
#' @param outcome_label Label for outcome (string)
#' @param grouping_variables Character vector of variable names to
#'     be summarized by.  Variables are converted to factors if not already one.
#' @param prevalence Numeric value between 0 and 1, representing a target
#'     prevalence for additional NPV and PPV calculations.
#' @param citype Confidence interval type.
#' @param bootreps Number of bootstrap samples used to generate F1 score CI
#' @param bootseed Seed value to be used when calculating bootsraped CI's
#' @param doboot Logical. Generate bootstrap estimate of F1 confidence interval?
#' @param direction Direction for roc comparison.  See ?pROC::roc. The default,
#'     "<", treats higher predicted probabilities as more likely to be positive.
#'     "auto" lets pROC choose the direction that gives an AUC of at least 0.5,
#'     which hides models that perform worse than chance. Only the AUC and ROC
#'     curve use `direction`; sensitivity, specificity, and other threshold based
#'     results always treat higher predictions as positive, so other values give
#'     a warning.
#' @param ... currently unused
#'
#' @export

dx <- function(data,
               classlabels = c("Negative", "Positive"),
               threshold_range = NA, outcome_label = NA, pred_varname, true_varname,
               setthreshold = .5, poslabel = 1, grouping_variables = NA, prevalence = NA,
               citype = "exact", bootreps = 2000, bootseed = 20191015,
               doboot = FALSE, direction = "<", ...) {


  # Check if pred_varname in data
  if (!pred_varname %in% names(data)) {
    stop(paste(pred_varname, "was not found in `data`"))
  }

  # Check if true_varname in data
  if (!true_varname %in% names(data)) {
    stop(paste(true_varname, "was not found in `data`"))
  }

  # Check if grouping variables are factors, and drop unused levels
  if (!identical(grouping_variables, NA)) {
    for (f in grouping_variables) {
      if (!is.factor(data[[f]])) {
        data[[f]] <- as.factor(data[[f]])
      }
      data[[f]] <- droplevels(data[[f]])
    }
  }

  # Check that poslabel is 0 or 1
  if (length(poslabel) != 1 || !poslabel %in% c(0, 1)) {
    stop("poslabel should be 0 or 1")
  }

  # direction is only used for the AUC; threshold based results always treat
  # higher predictions as the positive class
  if (direction != "<") {
    warning(paste0(
      "direction = \"", direction, "\" only affects the AUC. Sensitivity, ",
      "specificity, and other threshold based results treat higher ",
      "predictions as the positive class."
    ), call. = FALSE)
  }

  # Check if pred_varname is numeric
  if (!is.numeric(data[[pred_varname]])) {
    stop(paste(pred_varname, "should be numeric"))
  }

  # Check for missing values
  for (v in c(pred_varname, true_varname)) {
    n_missing <- sum(is.na(data[[v]]))
    if (n_missing > 0) {
      stop(paste0(
        v, " contains ", n_missing, " missing value(s). ",
        "Remove or impute missing values before calling dx()."
      ))
    }
  }

  # Check if pred_varname contains probabilities
  pred_range <- range(data[[pred_varname]])
  if (pred_range[1] < 0 || pred_range[2] > 1) {
    stop(paste0(
      pred_varname, " should contain predicted probabilities between 0 and 1, ",
      "but ranges from ", signif(pred_range[1], 3), " to ", signif(pred_range[2], 3), ". ",
      "If these are logits, convert them with plogis()."
    ))
  }

  # Convert logical, factor, or character truth coded as 0/1 to numeric
  truth <- data[[true_varname]]
  if (is.logical(truth) || is.factor(truth) || is.character(truth)) {
    if (is.logical(truth)) {
      truth <- as.numeric(truth)
    } else if (all(as.character(truth) %in% c("0", "1"))) {
      truth <- as.numeric(as.character(truth))
    }
    data[[true_varname]] <- truth
  }

  # Check if true_varname consists of only c(0,1)
  if (!all(data[[true_varname]] %in% c(0, 1))) {
    stop(paste(
      true_varname,
      "should be numeric vector consisting of only 0's and 1's"
    ))
  }

  # Check that both classes are present
  if (length(unique(data[[true_varname]])) < 2) {
    stop(paste0(
      true_varname, " should contain both 0's and 1's, but only contains ",
      unique(data[[true_varname]]), "'s"
    ))
  }

  # Check that predictions vary
  if (length(unique(data[[pred_varname]])) < 2) {
    stop(paste0(
      "All values of ", pred_varname, " are identical (",
      unique(data[[pred_varname]]), "). At least two distinct values are needed."
    ))
  }

  data$test_binary <- ifelse(
    data[[pred_varname]] < setthreshold, 0, 1
  )
  data$test_binaryf <- factor(data$test_binary,
    levels = c(0, 1),
    labels = classlabels
  )
  data$true_binaryf <- factor(data[[true_varname]],
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
    prevalence = prevalence,
    citype = citype,
    bootreps = bootreps,
    bootseed = bootseed,
    doboot = doboot,
    direction = direction
  )

  # Use bootseed so bootstrapped CIs are reproducible, and restore the
  # user's random number state on exit
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(bootseed)

  # Get all unique thresholds
  # Set thresholds may not be in threshold_range
  # Round so floating point differences (e.g. from seq()) don't create duplicates
  all_thresholds <- unique(round(c(options$setthreshold, options$threshold_range), 10))
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

  ###### Rank Analysis
  rank_analysis <- dx_rank(data, options)


  # Number of unique levels
  n_levels <- length(unique(threshold_measures$label))

  roc <- get_roc(true_varname, pred_varname, data, direction)

  # main confusion matrix
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  cm <- dx_cm(
    predprob = predprob,
    truth = truth,
    threshold = setthreshold,
    poslabel = options$poslabel
  )


  structure(list(
    data = data,
    options = options,
    measures = threshold_measures,
    thresholds = threshold_analysis,
    prevalence = prevalence_analysis,
    rank = rank_analysis,
    cm = cm,
    n_levels = n_levels,
    roc = roc
  ), class = "dx")
}
