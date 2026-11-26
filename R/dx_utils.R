#' Summary
#' @param object An object of class "dx"
#' @param thresh The threshold to return values from
#' @param variable Variable to include in returned values
#' @param label Labels to include in returned values
#' @param show_var Include variable column in returned data?
#' @param show_label Include label in returned data?
#' @param measure Measures to include
#' @param ... additional arguments to be passed to or from methods
#' @return A table of the measures and their summaries at `thresh`, made with
#'     [knitr::kable()]. If knitr is not installed, a data frame.
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   setthreshold = .3,
#'   grouping_variables = "Sex"
#' )
#' summary(dx_obj)
#' summary(dx_obj, variable = "Overall", show_var = FALSE)
#' @export
summary.dx <- function(object, thresh = object$options$setthreshold,
                       variable = NA, label = NA, show_var = TRUE,
                       show_label = TRUE, measure = NA, ...) {
  if (length(thresh) != 1 || is.na(thresh)) {
    stop("Must pass a numeric value to thresh")
  }

  tmp <- as.data.frame(object,
    thresh = thresh,
    variable = variable,
    label = label,
    measure = measure
  )

  tmp <- tmp[, c("variable", "label", "measure", "summary")]

  rownames(tmp) <- NULL

  if (!show_var) tmp <- subset(tmp, select = -c(variable))
  if (!show_label) tmp <- subset(tmp, select = -c(label))

  caption <- paste("Threshold=", thresh)

  rownames(tmp) <- NULL
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(tmp, caption = caption, row.names = FALSE)
  } else {
    tmp
  }
}

#' Print a diagnostic summary
#'
#' Prints a short overview of a `dx`, `dx_itd`, or `dx_compare` object: the
#' inputs and the main results. Use [summary()] or [as.data.frame()] for all
#' of the measures.
#'
#' @param x An object of class "dx", "dx_itd", or "dx_compare"
#' @param ... additional arguments to be passed to or from methods
#' @return `x`, invisibly.
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   setthreshold = .3,
#'   grouping_variables = "Sex"
#' )
#' dx_obj
#' @export
print.dx <- function(x, ...) {
  o <- x$options
  cat("Diagnostic summary (dx)\n")
  print_inputs(x)
  other <- setdiff(unique(x$measures$threshold), o$setthreshold)
  if (length(other) > 0) {
    cat("  Other thresholds: ", paste(other, collapse = ", "), "\n", sep = "")
  }
  print_measures(
    as.data.frame(x, variable = "Overall", thresh = o$setthreshold),
    c(
      "AUC ROC", "Sensitivity", "Specificity",
      "Positive Predictive Value", "Negative Predictive Value"
    )
  )
  cat("\nUse summary() or as.data.frame() for all measures.\n")
  invisible(x)
}

#' @rdname print.dx
#' @export
print.dx_compare <- function(x, ...) {
  cat(
    "Comparison of ", length(x$dx_list), " models (dx_compare): ",
    paste(names(x$dx_list), collapse = ", "), "\n\n",
    sep = ""
  )
  tests <- as.data.frame(x$tests)[c("models", "test", "summary", "p_value")]
  tests$p_value <- vapply(tests$p_value, format_pvalue, character(1), accuracy = 0.001)
  print(tests, row.names = FALSE, right = FALSE)
  cat("\nAll results are in the `tests` element.\n")
  invisible(x)
}

# The inputs shared by dx and dx_itd objects, for their print methods
print_inputs <- function(x) {
  o <- x$options
  truth <- x$data[[o$true_varname]]
  n <- length(truth)
  events <- sum(truth == o$poslabel)
  cat("  Predictions: ", o$pred_varname, "\n", sep = "")
  cat("  Reference standard: ", o$true_varname, " (event: ", o$poslabel, ")\n", sep = "")
  cat(
    "  Cases: ", comma(n), ", ", comma(events), " with the event (",
    format_number(events / n, percent = TRUE), ")\n",
    sep = ""
  )
  cat("  Threshold: ", o$setthreshold, "\n", sep = "")
  if (!identical(o$grouping_variables, NA)) {
    cat("  Subgroups: ", paste(o$grouping_variables, collapse = ", "), "\n", sep = "")
  }
}

# The chosen overall measures, for the print methods
print_measures <- function(measures, names) {
  measures <- as.data.frame(measures)
  measures <- measures[match(names, measures$measure), c("measure", "summary")]
  cat("\n")
  print(measures, row.names = FALSE, right = FALSE)
}

#' Convert to a data frame
#' @param x An object of class "dx"
#' @param thresh The threshold to return values from
#' @param row.names NULL or a character vector giving the row names for the
#'     data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'     names (to syntactic names: see make.names) is optional.
#' @param variable Variable to include in returned values
#' @param label Labels to include in returned values
#' @param measure Measures to include
#' @param ... additional arguments to be passed to or from methods
#' @return A data frame with one row per measure, variable, label, and threshold,
#'     with the estimate, confidence interval, and formatted summary.
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   setthreshold = .3
#' )
#' head(as.data.frame(dx_obj))
#' as.data.frame(dx_obj, measure = c("Sensitivity", "Specificity"))
#' @export
as.data.frame.dx <- function(x, row.names = NULL, optional = TRUE, thresh = NA,
                             variable = NA, label = NA, measure = NA, ...) {
  tmp <- x$measures

  if (!identical(thresh, NA)) {
    # Match with a tolerance to allow for floating point differences
    keep <- vapply(tmp$threshold, function(t) any(abs(t - thresh) < 1e-8), logical(1))
    tmp <- tmp[keep, ]
  }

  filter_measures(tmp, variable = variable, label = label, measure = measure)
}

# Keep the rows of a measures data frame for the given variables, labels, and
# measures. NA keeps all values.
filter_measures <- function(tmp, variable = NA, label = NA, measure = NA) {
  if (!identical(variable, NA)) {
    tmp <- tmp[tmp$variable %in% variable, ]
  }
  if (!identical(label, NA)) {
    tmp <- tmp[tmp$label %in% label, ]
  }
  if (!identical(measure, NA)) {
    tmp <- tmp[tmp$measure %in% measure, ]
  }

  tmp
}

comma <- function(x) {
  y <- prettyNum(x, big.mark = ",")
  y[y == "NA"] <- ""
  return(y)
}

format_pvalue <- function(p, accuracy = 0.01) {
  # Define the smallest non-zero number representable with the given accuracy
  smallest_nonzero <- accuracy

  if (is.na(p)) {
    return("-")
  } else if (p < smallest_nonzero) {
    # Construct the smallest non-zero representation based on accuracy
    smallest_nonzero_str <- formatC(accuracy, format = "f", digits = -log10(accuracy))
    return(paste0("p<", smallest_nonzero_str))
  } else {
    # Round and format the p-value
    rounded_p <- round(p, -log10(accuracy))
    return(paste("p=", formatC(rounded_p, format = "f", digits = -log10(accuracy)), sep = ""))
  }
}

# Format a number with a fixed number of decimals, e.g. accuracy = .01 for 2,
# optionally as a percent. Missing values are shown as "-".
format_number <- function(num, accuracy = .1, percent = FALSE) {
  if (is.na(num)) {
    "-"
  } else if (percent) {
    paste0(formatC(num * 100, format = "f", digits = -log10(accuracy)), "%")
  } else {
    formatC(num, format = "f", digits = -log10(accuracy), big.mark = ",")
  }
}

conf_int <- function(est, lower, upper, accuracy = .1, percent = FALSE) {
  paste0(
    format_number(est, accuracy, percent), " (",
    format_number(lower, accuracy, percent), ", ",
    format_number(upper, accuracy, percent), ")"
  )
}

backtick <- function(x) {
  paste0("`", x, "`")
}

# Stop if any of `pkgs` is not installed, naming the function that needs them
check_package <- function(pkgs, calling_func = NULL) {
  # The calling function can be a name or a call such as pkg::fun
  if (is.null(calling_func)) {
    calling_func <- tryCatch(
      paste(deparse(sys.call(-1)[[1]]), collapse = ""),
      error = function(e) NA_character_
    )
  }

  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0(
        backtick(pkg), " must be installed to use ",
        if (is.na(calling_func)) "this function" else backtick(calling_func), "."
      ), call. = FALSE)
    }
  }
}

dx_breslow_day <- function(data, options, group_varname) {
  predprob <- data[[options$pred_varname]]
  pred <- ifelse(predprob >= options$setthreshold, 1, 0)

  tmp <- data.frame(
    predprob = predprob,
    truth = data[[options$true_varname]],
    pred = pred,
    group = data[[group_varname]]
  )

  tmp <- tmp[!is.na(tmp$group), ]

  # Only subgroups with both outcome classes and both predicted classes
  # contribute to the test
  informative <- vapply(levels(tmp$group), function(g) {
    in_group <- tmp$group %in% g
    length(unique(tmp$truth[in_group])) == 2 && length(unique(tmp$pred[in_group])) == 2
  }, logical(1))

  one_class <- vapply(levels(tmp$group), function(g) {
    length(unique(tmp$truth[tmp$group %in% g])) < 2
  }, logical(1))
  no_pred_variation <- !informative & !one_class
  if (any(no_pred_variation)) {
    warning(paste0(
      "Breslow-Day test for ", group_varname, " excludes subgroup(s) where ",
      "all predictions fall on one side of the threshold: ",
      paste(levels(tmp$group)[no_pred_variation], collapse = ", "), "."
    ), call. = FALSE)
  }

  # The test needs at least two informative subgroups
  if (sum(informative) < 2) {
    return(NULL)
  }

  tmp <- tmp[tmp$group %in% levels(tmp$group)[informative], ]
  tmp$group <- droplevels(tmp$group)

  tab <- table(
    factor(tmp$pred, levels = c(0, 1)),
    factor(tmp$truth, levels = c(0, 1)),
    tmp$group
  )

  bd <- breslow_day_test(tab, odds_ratio = NA, correct = FALSE)
  undefined <- is.na(bd$p_value)

  measure <- measure_df(
    measure = "Breslow-Day",
    estimate = if (undefined) "-" else format_pvalue(bd$p_value[[1]], accuracy = 0.01),
    fraction = "",
    ci_type = "",
    notes = if (undefined) {
      "Undefined: the Mantel-Haenszel OR is 0 or infinite"
    } else {
      "Mantel-Haenszel OR estimate"
    },
    estimate_raw = bd$p_value,
    lci_raw = NA,
    uci_raw = NA
  )

  measure$n <- nrow(tmp)

  measure$variable <- group_varname
  measure$label <- "Overall"
  measure$threshold <- options$setthreshold


  return(measure)
}

# Percentile bootstrap CI for a metric that depends only on the 2x2 table.
# Resampling n cases with replacement from the table is the same as drawing
# the four cell counts from a multinomial distribution with the observed cell
# proportions, so draw the counts directly and compute every replicate at once.
# Metrics that use the predictions themselves (e.g. AUC) would need case
# resampling instead.
boot_metric <- function(cm, metric_func, metric_args, bootreps, measure_name) {
  counts <- stats::rmultinom(bootreps, size = cm$n, prob = c(cm$tp, cm$fn, cm$tn, cm$fp))
  storage.mode(counts) <- "double"
  cm_boot <- cm_from_counts(tp = counts[1, ], fn = counts[2, ], tn = counts[3, ], fp = counts[4, ])
  boot_metrics <- do.call(metric_func, c(list(cm = cm_boot), metric_args))

  na_proportion <- mean(is.na(boot_metrics))
  if (na_proportion > 0.05) {
    warning(
      paste0(
        "High proportion of NAs in bootstrapped samples for ", measure_name,
        " (", round(na_proportion, digits = 2), ")"
      )
    )
  }

  metric_ci <- stats::quantile(boot_metrics, probs = c(0.025, 0.975), na.rm = TRUE)
  list(lower = metric_ci[[1]], upper = metric_ci[[2]])
}

evaluate_metric <- function(cm, metric_func, measure_name, detail, boot, bootreps, ...) {
  validate_detail(detail)
  # Calculate the metric using the provided function
  metric_raw <- metric_func(cm, ...)

  if (detail == "simple") {
    return(metric_raw)
  } else if (detail == "full") {
    if (boot) {
      ci_bounds <- boot_metric(
        cm = cm,
        metric_func = metric_func,
        metric_args = list(...),
        bootreps = bootreps,
        measure_name = measure_name
      )
      ci_lower <- ci_bounds$lower
      ci_upper <- ci_bounds$upper

      # Build and return the result with confidence intervals
      return(measure_df(
        measure = measure_name,
        estimate = conf_int(metric_raw, ci_lower, ci_upper, accuracy = .01),
        estimate_raw = metric_raw,
        ci_type = "Bootstrapped",
        lci_raw = ci_lower,
        uci_raw = ci_upper
      ))
    } else {
      # Return results without bootstrapping
      return(measure_df(
        measure = measure_name,
        estimate = format_number(metric_raw, accuracy = .01),
        estimate_raw = metric_raw,
        ci_type = NA_character_,
        lci_raw = NA_real_,
        uci_raw = NA_real_,
        notes = "Specify `boot = TRUE` for CIs"
      ))
    }
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}


return_df <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(x))
  } else {
    return(x)
  }
}

validate_dx_list <- function(dx_list) {
  if (!is.list(dx_list)) {
    stop("dx_list must be a list of `dx` objects")
  }
  if (!length(dx_list) >= 2) {
    stop("dx_list must contain two or more `dx_objects")
  }
  for (x in dx_list) {
    if (!"dx" %in% class(x)) {
      stop("All elements in `dx_list` must be `dx` objects")
    }
  }
  if (is.null(names(dx_list))) {
    names(dx_list) <- paste("Model", seq_along(dx_list))
  }
  for (i in seq_along(dx_list)) {
    if (names(dx_list)[i] == "") {
      names(dx_list)[i] <- paste("Model", i)
    }
  }
  if (any(duplicated(names(dx_list)))) {
    stop("Each `dx` object in dx_list must have a unique name")
  }
  return(dx_list)
}


pluck_probabilities <- function(dx) {
  dx$data[[dx$options$pred_varname]]
}

pluck_predicted <- function(dx) {
  as.numeric(pluck_probabilities(dx) >= dx$options$setthreshold)
}

pluck_truths <- function(dx) {
  dx$data[[dx$options$true_varname]]
}

two_model_name <- function(x, y) {
  paste(x, "vs.", y)
}


#' Create a Data Frame for Metric Measures
#'
#' This internal function creates a data frame for storing metric measures, including
#' the measure name, its estimate, confidence interval type, and any additional notes.
#'
#' @param measure The name of the measure.
#' @param estimate The formatted estimate of the measure.
#' @param fraction The fraction representing the measure (if applicable).
#' @param ci_type The type of confidence interval used.
#' @param notes Additional notes or interpretation about the measure.
#' @param estimate_raw The raw estimate value.
#' @param lci_raw The lower limit of the confidence interval (raw).
#' @param uci_raw The upper limit of the confidence interval (raw).
#' @return A data frame with the specified measure details.
#' @noRd
measure_df <- function(measure = "", estimate = "", fraction = "",
                       ci_type = "", notes = "", estimate_raw = NA,
                       lci_raw = NA, uci_raw = NA) {
  metric <- data.frame(
    measure = measure,
    summary = estimate,
    estimate = estimate_raw,
    conf_low = lci_raw,
    conf_high = uci_raw,
    fraction = fraction,
    conf_type = ci_type,
    notes = notes,
    stringsAsFactors = FALSE
  )

  return_df(metric)
}

compare_df <- function(models = "",
                       test = "",
                       summary = "",
                       p_value = NA,
                       estimate = NA,
                       conf_low = NA,
                       conf_high = NA,
                       statistic = NA,
                       notes = "") {
  metric <- data.frame(
    models = models,
    test = test,
    summary = summary,
    p_value = p_value,
    estimate = estimate,
    conf_low = conf_low,
    conf_high = conf_high,
    statistic = statistic,
    notes = notes,
    stringsAsFactors = FALSE
  )

  return_df(metric)
}

validate_detail <- function(detail) {
  invisible(match.arg(detail, choices = c("full", "simple")))
}
