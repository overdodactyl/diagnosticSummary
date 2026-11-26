#' Summarize a diagnostic test
#'
#' Calculates diagnostic accuracy measures, with confidence intervals, for
#' predicted probabilities compared to a binary reference standard: overall,
#' at each threshold in `threshold_range`, and for each subgroup in
#' `grouping_variables`.
#'
#' @param data A tbl.
#' @param classlabels Deprecated; has no effect.
#' @param pred_varname Column name containing the predictions (string). Values
#'     should be predicted probabilities that the outcome is 1, between 0 and 1,
#'     with no missing values.
#' @param setthreshold A number between 0 and 1 representing the threshold used
#'     to classify the predictions
#' @param threshold_range Optional. A numeric vector of thresholds between 0
#'     and 1 to loop over.
#' @param true_varname Column name containing the reference standard (string).
#'     Values should be 0 or 1 (numeric, logical, factor, or character), with
#'     no missing values, and both classes must be present.
#' @param poslabel The outcome class that is the event: 1 (default) or 0.
#'     Predictions are still probabilities that the outcome is 1, so with
#'     `poslabel = 0` a prediction below the threshold is a positive test.
#' @param outcome_label Deprecated; has no effect.
#' @param grouping_variables Character vector of variable names to
#'     be summarized by.  Variables are converted to factors if not already one.
#'     A column can't be named "Overall", which is used for the results of all
#'     cases.
#' @param prevalence A single number greater than 0 and less than 1,
#'     representing a target prevalence for additional NPV and PPV calculations.
#' @param citype Confidence interval type for proportions: one of the methods
#'     of [binom::binom.confint()], such as "exact" (default), "wilson", or "ac".
#'     The estimate is always the observed proportion.
#' @param bootreps Number of bootstrap samples used for the bootstrapped CIs.
#' @param bootseed Seed used for the bootstrapped CIs, so results are reproducible
#'     on the same R version and platform. Bootstrap draws can differ across
#'     platforms and R versions by Monte Carlo error.
#' @param doboot Logical. Generate percentile bootstrap CIs for the measures
#'     without a closed-form CI (F1, F2, MCC, balanced accuracy, informedness,
#'     markedness, G-mean, and Fowlkes-Mallows)? The confusion matrix cell
#'     counts are resampled from a multinomial distribution, which is
#'     equivalent to resampling cases.
#' @param direction Deprecated. Predictions are probabilities that the outcome
#'     is 1, so higher predictions always point to outcome 1; use `poslabel` to
#'     choose which outcome is the event. Values other than the default, "<",
#'     give a warning and only affect the AUC and ROC curve (see ?pROC::roc).
#'     "auto" lets pROC choose the direction that gives an AUC of at least 0.5,
#'     which hides models that perform worse than chance.
#' @param ... currently unused
#'
#' @return An object of class "dx": a list with the data, the options used,
#'     the measures at each threshold and subgroup (`measures`), measures across
#'     all thresholds (`thresholds`), a prevalence analysis (`prevalence`), the
#'     ranked predictions used by the gain, lift, KS, and CAP plots (`rank`), the
#'     confusion matrix at `setthreshold` (`cm`), and the [pROC::roc()] object
#'     (`roc`). Use [summary.dx()] or [as.data.frame.dx()] to view the measures.
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex")
#' )
#' summary(dx_obj)
#'
#' # Bootstrapped CIs for measures without a closed-form CI
#' dx_boot <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   setthreshold = .3,
#'   doboot = TRUE,
#'   bootreps = 500
#' )
#' summary(dx_boot, measure = c("F1 Score", "Matthews Correlation Coefficient"))
#' @export

dx <- function(data,
               classlabels = c("Negative", "Positive"),
               threshold_range = NA, outcome_label = NA, pred_varname, true_varname,
               setthreshold = .5, poslabel = 1, grouping_variables = NA, prevalence = NA,
               citype = "exact", bootreps = 2000, bootseed = 20191015,
               doboot = FALSE, direction = "<", ...) {
  # NULL or a missing value of any type means the option isn't used
  if (not_supplied(threshold_range)) threshold_range <- NA
  if (not_supplied(prevalence)) prevalence <- NA
  if (not_supplied(grouping_variables)) grouping_variables <- NA
  check_dx_options(
    poslabel, direction, setthreshold,
    citype = citype, threshold_range = threshold_range, prevalence = prevalence,
    doboot = doboot, bootreps = bootreps
  )
  if (!missing(classlabels)) warn_deprecated_unused("classlabels")
  if (!missing(outcome_label)) warn_deprecated_unused("outcome_label")
  data <- prepare_dx_data(data, pred_varname, true_varname, grouping_variables)

  options <- list(
    threshold_range = threshold_range,
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
  # Compare rounded values so floating point differences (e.g. from seq())
  # don't create duplicates, but keep the original values so predictions equal
  # to a threshold are classified the same way as in the confusion matrix
  all_thresholds <- c(options$setthreshold, options$threshold_range)
  all_thresholds <- all_thresholds[!is.na(all_thresholds)]
  all_thresholds <- all_thresholds[!duplicated(round(all_thresholds, 10))]

  # Confusion matrix at setthreshold
  cm <- dx_cm(
    predprob = data[[pred_varname]],
    truth = data[[true_varname]],
    threshold = setthreshold,
    poslabel = poslabel
  )

  # Overall measures at each threshold
  threshold_measures <- do.call(rbind, lapply(all_thresholds, function(t) {
    dx_measure(data, threshold = t, options = options)
  }))

  # Subgroup measures at setthreshold
  if (!identical(grouping_variables, NA)) {
    subgroups <- do.call(rbind, lapply(grouping_variables, function(v) {
      dx_group_measure(data = data, options = options, group_varname = v)
    }))
    threshold_measures <- rbind(subgroups, threshold_measures)
  }

  structure(list(
    data = data,
    options = options,
    measures = threshold_measures,
    thresholds = dx_thresholds(data, options),
    prevalence = dx_prevalence_analysis(cm),
    rank = dx_rank(data, options),
    cm = cm,
    n_levels = length(unique(threshold_measures$label)),
    roc = get_roc(true_varname, pred_varname, data, direction)
  ), class = "dx")
}

# Optional arguments default to NA. NULL, an empty vector, or a single missing
# value of another type (such as NA_real_) are treated the same way.
not_supplied <- function(x) {
  length(x) == 0 || (length(x) == 1 && is.atomic(x) && is.na(x))
}

# Check options that don't depend on the data
check_dx_options <- function(poslabel, direction, setthreshold, citype = "exact",
                             threshold_range = NA, prevalence = NA,
                             doboot = FALSE, bootreps = 2000) {
  # Check that poslabel is 0 or 1
  if (length(poslabel) != 1 || !poslabel %in% c(0, 1)) {
    stop("poslabel should be 0 or 1", call. = FALSE)
  }

  if (!is.numeric(setthreshold) || length(setthreshold) != 1 || is.na(setthreshold) ||
    setthreshold < 0 || setthreshold > 1) {
    stop("setthreshold should be a single number between 0 and 1", call. = FALSE)
  }

  # binom::binom.confint() matches method names partially, and "all" would
  # return one row per method
  ci_methods <- c(
    "exact", "ac", "agresti-coull", "asymptotic", "wilson", "prop.test",
    "bayes", "logit", "cloglog", "probit", "profile", "lrt"
  )
  if (!is.character(citype) || length(citype) != 1 || is.na(pmatch(citype, ci_methods))) {
    stop(paste0(
      "citype should be one of: ", paste(setdiff(ci_methods, "agresti-coull"), collapse = ", ")
    ), call. = FALSE)
  }

  # NA (the default) means no extra thresholds
  if (!not_supplied(threshold_range)) {
    if (!is.numeric(threshold_range) || anyNA(threshold_range) ||
      any(threshold_range < 0 | threshold_range > 1)) {
      stop("threshold_range should be a numeric vector of values between 0 and 1", call. = FALSE)
    }
  }

  # A prevalence of 0 or 1 leaves nothing to estimate
  if (!not_supplied(prevalence)) {
    if (!is.numeric(prevalence) || length(prevalence) != 1 || is.na(prevalence) ||
      prevalence <= 0 || prevalence >= 1) {
      stop("prevalence should be a single number greater than 0 and less than 1", call. = FALSE)
    }
  }

  if (!is.logical(doboot) || length(doboot) != 1 || is.na(doboot)) {
    stop("doboot should be TRUE or FALSE", call. = FALSE)
  }
  if (doboot) {
    if (!is.numeric(bootreps) || length(bootreps) != 1 || is.na(bootreps) ||
      bootreps < 1 || bootreps != round(bootreps)) {
      stop("bootreps should be a single whole number of at least 1", call. = FALSE)
    }
  }

  if (!is.character(direction) || length(direction) != 1 || !direction %in% c("<", ">", "auto")) {
    stop("direction should be \"<\", \">\", or \"auto\"", call. = FALSE)
  }

  # direction is deprecated and only used for the AUC and ROC curve
  if (direction != "<") {
    warning(paste0(
      "direction is deprecated and will be removed in a future version. ",
      "Predictions should be probabilities that the outcome is 1; use poslabel ",
      "to choose the event. direction = \"", direction, "\" only affects the AUC ",
      "and ROC curve, not sensitivity, specificity, or other threshold based results."
    ), call. = FALSE)
  }
}

# Warn about arguments that are deprecated and have no effect
warn_deprecated_unused <- function(arg) {
  warning(paste0(
    arg, " is deprecated and will be removed in a future version. ",
    "It has no effect."
  ), call. = FALSE)
}

# Validate the prediction and truth columns, convert truth to numeric 0/1,
# and convert grouping variables to factors without unused levels
prepare_dx_data <- function(data, pred_varname, true_varname, grouping_variables,
                            allow_missing_pred = FALSE) {
  for (v in c(pred_varname, true_varname)) {
    if (!v %in% names(data)) {
      stop(paste(v, "was not found in `data`"), call. = FALSE)
    }
  }

  if (!not_supplied(grouping_variables)) {
    if (!is.character(grouping_variables) || anyNA(grouping_variables)) {
      stop("grouping_variables should be a character vector of column names", call. = FALSE)
    }
    not_found <- setdiff(grouping_variables, names(data))
    if (length(not_found) > 0) {
      stop(paste0(
        "grouping_variables not found in `data`: ", paste(not_found, collapse = ", ")
      ), call. = FALSE)
    }
    if (anyDuplicated(grouping_variables)) {
      stop(paste0(
        "grouping_variables should not contain duplicates: ",
        paste(unique(grouping_variables[duplicated(grouping_variables)]), collapse = ", ")
      ), call. = FALSE)
    }
    # Results for all cases are stored under the variable "Overall"
    if ("Overall" %in% grouping_variables) {
      stop(paste(
        "grouping_variables should not include a column named \"Overall\",",
        "which is used for the results of all cases. Rename the column."
      ), call. = FALSE)
    }
    for (f in grouping_variables) {
      data[[f]] <- droplevels(as.factor(data[[f]]))
    }
  }

  pred <- data[[pred_varname]]
  if (!is.numeric(pred)) {
    stop(paste(pred_varname, "should be numeric"), call. = FALSE)
  }

  # Missing predictions are ungradable results in dx_itd(), but truth must
  # always be known
  check_missing <- if (allow_missing_pred) true_varname else c(pred_varname, true_varname)
  for (v in check_missing) {
    n_missing <- sum(is.na(data[[v]]))
    if (n_missing > 0) {
      msg <- paste0(
        v, " contains ", n_missing, " missing value(s). ",
        "Remove or impute missing values before calling ",
        if (allow_missing_pred) "dx_itd()." else "dx()."
      )
      if (v == pred_varname) {
        msg <- paste(
          msg, "To analyze missing predictions as ungradable results, use dx_itd()."
        )
      }
      stop(msg, call. = FALSE)
    }
  }

  if (allow_missing_pred) {
    if (all(is.na(pred))) {
      stop(paste0(
        "All values of ", pred_varname, " are missing. ",
        "At least one prediction is needed."
      ), call. = FALSE)
    }
    pred <- pred[!is.na(pred)]
  }

  pred_range <- range(pred)
  if (pred_range[1] < 0 || pred_range[2] > 1) {
    stop(paste0(
      pred_varname, " should contain predicted probabilities between 0 and 1, ",
      "but ranges from ", signif(pred_range[1], 3), " to ", signif(pred_range[2], 3), ". ",
      "If these are logits, convert them with plogis()."
    ), call. = FALSE)
  }

  if (!allow_missing_pred && length(unique(pred)) < 2) {
    stop(paste0(
      "All values of ", pred_varname, " are identical (",
      unique(pred), "). At least two distinct values are needed."
    ), call. = FALSE)
  }

  # Convert logical, factor, or character truth coded as 0/1 to numeric
  truth <- data[[true_varname]]
  if (is.logical(truth)) {
    truth <- as.numeric(truth)
  } else if ((is.factor(truth) || is.character(truth)) &&
    all(as.character(truth) %in% c("0", "1"))) {
    truth <- as.numeric(as.character(truth))
  }

  if (!all(truth %in% c(0, 1))) {
    stop(paste(
      true_varname,
      "should be numeric vector consisting of only 0's and 1's"
    ), call. = FALSE)
  }

  if (length(unique(truth)) < 2) {
    stop(paste0(
      true_varname, " should contain both 0's and 1's, but only contains ",
      unique(truth), "'s"
    ), call. = FALSE)
  }

  data[[true_varname]] <- truth
  data
}
