#' Intention-to-diagnose (3x2) analysis
#'
#' Summarize a diagnostic test when some predictions are missing, for example
#' because a case failed quality control or a pre-defined rule stopped the
#' model from running. Instead of excluding these cases, a missing prediction
#' is treated as an ungradable result. Results are summarized in a 3x2 table,
#' with positive, negative, and ungradable test results in the rows and the
#' reference standard in the columns (Schuetz et al., 2012).
#'
#' @param data A tbl.
#' @param pred_varname Column name containing AI prediction (string). Values
#'     should be predicted probabilities that the outcome is 1, between 0 and 1.
#'     Missing values (`NA`) are ungradable results.
#' @param true_varname Column name containing AI reference standard (string).
#'     Values should be 0 or 1 (numeric, logical, factor, or character), with
#'     no missing values, and both classes must be present.
#' @param setthreshold A number between 0 and 1 representing the threshold used
#'     to identify AI prediction. Predictions at or above the threshold
#'     predict outcome 1.
#' @param poslabel The outcome class that is the event: 1 (default) or 0.
#'     Predictions are still probabilities that the outcome is 1, so with
#'     `poslabel = 0` a prediction below the threshold is a positive test.
#' @param grouping_variables Character vector of variable names to
#'     be summarized by.  Variables are converted to factors if not already one.
#' @param citype Confidence interval type for proportions (see
#'     [binom::binom.confint()]).
#'
#' @details
#' With TP, FN, and UP the positive, negative, and ungradable results among
#' cases with the event, and FP, TN, and UN the same among cases without it:
#'
#' | Measure | Definition |
#' |---|---|
#' | Diagnostic yield (disease positive) | (TP + FN) / (TP + FN + UP) |
#' | Diagnostic yield (disease negative) | (FP + TN) / (FP + TN + UN) |
#' | Diagnostic yield (overall) | Gradable results / N |
#' | Pre-test disease risk | (TP + FN + UP) / N |
#' | Positive predictive value | TP / (TP + FP) |
#' | Negative predictive value | TN / (TN + FN) |
#' | Post-test disease risk (positive) | TP / (TP + FP) |
#' | Post-test disease risk (negative) | FN / (TN + FN) |
#' | Post-test disease risk (ungradable) | UP / (UP + UN) |
#' | Likelihood ratio (positive) | (TP / (TP + FN + UP)) / (FP / (FP + TN + UN)) |
#' | Likelihood ratio (negative) | (FN / (TP + FN + UP)) / (TN / (FP + TN + UN)) |
#' | Likelihood ratio (ungradable) | (UP / (TP + FN + UP)) / (UN / (FP + TN + UN)) |
#' | Worst-case sensitivity | TP / (TP + FN + UP) |
#' | Worst-case specificity | TN / (FP + TN + UN) |
#' | Worst-case Youden's J | Worst-case sensitivity + worst-case specificity - 1 |
#'
#' Diagnostic yield is the proportion of cases with a gradable (positive or
#' negative) result. The worst case counts every ungradable result as
#' incorrect: as a negative result for cases with the event, and as a positive
#' result for cases without it. Predictive values only use gradable results.
#'
#' The likelihood ratios use all cases with and without the event as
#' denominators, including ungradable results, so the positive and negative
#' likelihood ratios differ from [dx_lrt_pos()] and [dx_lrt_neg()] on the
#' gradable cases. A likelihood ratio near 1 for ungradable results means
#' they are about as common with and without the event.
#'
#' Proportions use binomial confidence intervals (`citype`). Likelihood ratios
#' use a large sample interval on the log scale. If the 2x2 table of one row
#' against the others has a zero cell, 0.5 is added to each of its cells.
#' Youden's J uses a Wald interval that treats the two outcome classes as
#' independent, truncated to -1 to 1. The interval is too narrow when the
#' worst-case sensitivity or specificity is 0% or 100%, and has no width when
#' both are, which is noted in the results. Likelihood ratios are `NA` for a
#' result no case has, such as ungradable results when no predictions are
#' missing.
#'
#' At least one prediction is needed, but a subgroup in `grouping_variables`
#' can have every prediction missing, for example a site where every case
#' failed quality control. It is kept with a warning, with a diagnostic yield
#' of 0%.
#'
#' @return An object of class `dx_itd`, a list with `data`, `options`,
#'     `measures` (a data frame with one row per measure), and `cm` (the 3x2
#'     cell counts at `setthreshold`).
#' @references
#' Schuetz GM, Schlattmann P, Dewey M. Use of 3x2 tables with an intention to
#' diagnose approach to assess clinical performance of diagnostic tests:
#' meta-analytical evaluation of coronary CT angiography studies. BMJ.
#' 2012;345:e6717.
#' @seealso [dx_itd_table()] for the 3x2 table, [dx()] when every prediction
#'     is available.
#' @examples
#' data <- dx_heart_failure
#' data$predicted[seq(5, 261, by = 20)] <- NA
#' fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
#' summary(fit)
#' dx_itd_table(fit)
#' @export
dx_itd <- function(data, pred_varname, true_varname, setthreshold = .5,
                   poslabel = 1, grouping_variables = NA, citype = "exact") {
  check_dx_options(poslabel, direction = "<", setthreshold)
  data <- prepare_dx_data(
    data, pred_varname, true_varname, grouping_variables,
    allow_missing_pred = TRUE
  )

  options <- list(
    pred_varname = pred_varname,
    true_varname = true_varname,
    setthreshold = setthreshold,
    poslabel = poslabel,
    grouping_variables = grouping_variables,
    citype = citype
  )

  measures <- dx_measure_itd(data, options)

  if (!identical(grouping_variables, NA)) {
    subgroups <- do.call(rbind, lapply(grouping_variables, function(v) {
      dx_group_measure_itd(data = data, options = options, group_varname = v)
    }))
    measures <- rbind(subgroups, measures)
  }

  structure(list(
    data = data,
    options = options,
    measures = measures,
    cm = dx_cm_itd(
      predprob = data[[pred_varname]],
      truth = data[[true_varname]],
      threshold = setthreshold,
      poslabel = poslabel
    )
  ), class = "dx_itd")
}

# 3x2 cell counts: positive, negative, and ungradable (missing) results by
# outcome class. Gradable results are classified the same way as dx_cm().
dx_cm_itd <- function(predprob, truth, threshold, poslabel) {
  gradable <- !is.na(predprob)
  trueresult <- as.numeric(truth == poslabel)

  cm <- dx_cm(
    predprob = predprob[gradable],
    truth = truth[gradable],
    threshold = threshold,
    poslabel = poslabel
  )

  ug_pos <- sum(!gradable & trueresult == 1)
  ug_neg <- sum(!gradable & trueresult == 0)

  cm <- data.frame(
    tp = cm$tp,
    fn = cm$fn,
    ug_pos = ug_pos,
    fp = cm$fp,
    tn = cm$tn,
    ug_neg = ug_neg,
    dispos = cm$dispos + ug_pos,
    disneg = cm$disneg + ug_neg,
    n = cm$n + ug_pos + ug_neg,
    testpos = cm$testpos,
    testneg = cm$testneg,
    testug = ug_pos + ug_neg
  )

  class(cm) <- c("dx_cm_itd", class(cm))
  cm
}

dx_measure_itd <- function(data, options, var = "Overall", label = "Overall") {
  cm <- dx_cm_itd(
    predprob = data[[options$pred_varname]],
    truth = data[[options$true_varname]],
    threshold = options$setthreshold,
    poslabel = options$poslabel
  )
  gradable_cm <- cm_from_counts(tp = cm$tp, fn = cm$fn, tn = cm$tn, fp = cm$fp)
  citype <- options$citype

  binomial <- function(x, n, name) {
    metric_binomial(x, n, name = name, citype = citype)
  }

  metrics_list <- list(
    yield_pos = binomial(cm$tp + cm$fn, cm$dispos, "Diagnostic Yield (Disease Positive)"),
    yield_neg = binomial(cm$fp + cm$tn, cm$disneg, "Diagnostic Yield (Disease Negative)"),
    yield = binomial(cm$n - cm$testug, cm$n, "Diagnostic Yield (Overall)"),
    pre_test_risk = binomial(cm$dispos, cm$n, "Pre-Test Disease Risk"),
    ppv = dx_ppv(gradable_cm, citype = citype),
    npv = dx_npv(gradable_cm, citype = citype),
    post_test_risk_pos = binomial(cm$tp, cm$testpos, "Post-Test Disease Risk (Positive)"),
    post_test_risk_neg = binomial(cm$fn, cm$testneg, "Post-Test Disease Risk (Negative)"),
    post_test_risk_ug = binomial(cm$ug_pos, cm$testug, "Post-Test Disease Risk (Ungradable)"),
    lr_pos = metric_row_lr(cm$tp, cm$fp, cm, "Likelihood Ratio (Positive)"),
    lr_neg = metric_row_lr(cm$fn, cm$tn, cm, "Likelihood Ratio (Negative)"),
    lr_ug = metric_row_lr(cm$ug_pos, cm$ug_neg, cm, "Likelihood Ratio (Ungradable)"),
    sensitivity = binomial(cm$tp, cm$dispos, "Worst-Case Sensitivity"),
    specificity = binomial(cm$tn, cm$disneg, "Worst-Case Specificity"),
    youden = metric_youden_wald(cm, "Worst-Case Youden's J")
  )

  results <- do.call(rbind, metrics_list)

  results$threshold <- options$setthreshold
  results$variable <- var
  results$label <- label
  results$n <- nrow(data)

  to_first <- c("variable", "label")
  results[c(to_first, setdiff(names(results), to_first))]
}

dx_group_measure_itd <- function(data, options, group_varname) {
  # Use factor level order
  group_labels <- levels(data[[group_varname]])

  truth <- data[[options$true_varname]]
  one_class <- vapply(group_labels, function(g) {
    length(unique(truth[data[[group_varname]] %in% g])) < 2
  }, logical(1))
  if (any(one_class)) {
    warning(paste0(
      "Subgroup(s) of ", group_varname, " with only one outcome class: ",
      paste(group_labels[one_class], collapse = ", "), ". ",
      "Measures that need both outcome classes are undefined for these subgroups."
    ), call. = FALSE)
  }

  # Subgroups with no gradable results are kept, e.g. a site where every case
  # failed quality control, though dx_itd() on that subgroup alone gives an error
  pred <- data[[options$pred_varname]]
  all_missing <- vapply(group_labels, function(g) {
    all(is.na(pred[data[[group_varname]] %in% g]))
  }, logical(1))
  if (any(all_missing)) {
    warning(paste0(
      "Subgroup(s) of ", group_varname, " with all predictions missing: ",
      paste(group_labels[all_missing], collapse = ", "), ". ",
      "Their diagnostic yield is 0% and measures of gradable results are undefined."
    ), call. = FALSE)
  }

  # Levels were dropped in prepare_dx_data(), so every level has rows
  datalist <- lapply(group_labels, function(g) {
    dx_measure_itd(
      data = data[data[[group_varname]] %in% g, ],
      options = options,
      var = group_varname,
      label = g
    )
  })
  do.call(rbind, datalist)
}

# Likelihood ratio for one row of a 3x2 table, with x and y the row's counts
# with and without the event. The CI comes from the 2x2 table of this row
# against the others, with 0.5 added to each cell if any cell is zero.
metric_row_lr <- function(x, y, cm, name) {
  if (cm$dispos == 0 || cm$disneg == 0) {
    return(measure_df(
      measure = name,
      estimate = conf_int(NA, NA, NA),
      notes = "Requires both outcome classes",
      estimate_raw = NA_real_
    ))
  }

  # Undefined when no cases have this result, e.g. no ungradable results
  if (x + y == 0) {
    return(measure_df(
      measure = name,
      estimate = conf_int(NA, NA, NA),
      notes = "No cases with this result",
      estimate_raw = NA_real_
    ))
  }

  cells <- c(x, cm$dispos - x, y, cm$disneg - y)
  continuity_correction <- if (any(cells == 0)) 0.5 else 0
  x <- x + continuity_correction
  y <- y + continuity_correction
  dispos <- cm$dispos + 2 * continuity_correction
  disneg <- cm$disneg + 2 * continuity_correction

  ratio <- (x / dispos) / (y / disneg)
  ratio_sd <- sqrt(1 / x - 1 / dispos + 1 / y - 1 / disneg)

  ci_ratio(
    tp = x, tn = NA, fp = y, fn = NA, ratio = ratio, ratio_sd = ratio_sd,
    name = name, continuity_correction = continuity_correction
  )
}

# Youden's J from the worst-case sensitivity and specificity, with a Wald CI
# that treats the two outcome classes as independent binomials
metric_youden_wald <- function(cm, name) {
  sens <- cm$tp / cm$dispos
  spec <- cm$tn / cm$disneg
  j <- sens + spec - 1

  se <- sqrt(sens * (1 - sens) / cm$dispos + spec * (1 - spec) / cm$disneg)
  z_value <- 1.95996398454005 # 95% CI
  lower <- max(-1, j - z_value * se)
  upper <- min(1, j + z_value * se)

  measure_df(
    measure = name,
    estimate = conf_int(j, lower, upper, accuracy = .01),
    estimate_raw = j,
    lci_raw = lower,
    uci_raw = upper,
    ci_type = "Wald",
    notes = if (any(c(sens, spec) %in% c(0, 1))) {
      "Wald interval is too narrow when sensitivity or specificity is 0% or 100%"
    } else {
      ""
    }
  )
}

#' 3x2 table for an intention-to-diagnose analysis
#'
#' Lay out the results of [dx_itd()] as a 3x2 table: counts of positive,
#' negative, and ungradable results by outcome class, with predictive values,
#' post-test disease risks, and likelihood ratios for each row, and the
#' diagnostic yield and worst-case performance below.
#'
#' @param x An object of class `dx_itd`.
#' @param variable,label The subgroup to show. Defaults to all cases.
#' @param show_ci Show confidence intervals? If `FALSE`, only estimates are shown.
#' @return A data frame of formatted strings, one row per table row. The
#'     pre-test disease risk is in the `predictive_value` column of the
#'     "Total" row, and Youden's J in the `total` column of the
#'     "Worst-case performance" row.
#' @examples
#' data <- dx_heart_failure
#' data$predicted[seq(5, 261, by = 20)] <- NA
#' fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
#' dx_itd_table(fit)
#' dx_itd_table(fit, show_ci = FALSE)
#' @export
dx_itd_table <- function(x, variable = "Overall", label = "Overall", show_ci = TRUE) {
  if (!inherits(x, "dx_itd")) {
    stop("x should be a dx_itd object", call. = FALSE)
  }

  m <- x$measures[x$measures$variable == variable & x$measures$label == label, ]
  if (nrow(m) == 0) {
    stop(paste0("No results for variable = ", variable, " and label = ", label), call. = FALSE)
  }

  data <- x$data
  if (variable != "Overall") {
    data <- data[data[[variable]] %in% label, ]
  }
  cm <- dx_cm_itd(
    predprob = data[[x$options$pred_varname]],
    truth = data[[x$options$true_varname]],
    threshold = x$options$setthreshold,
    poslabel = x$options$poslabel
  )

  value <- function(name) {
    row <- m[m$measure == name, ]
    if (show_ci) {
      row$summary
    } else if (grepl("Likelihood|Youden", name)) {
      format_number(row$estimate, accuracy = .01)
    } else {
      format_number(row$estimate, percent = TRUE)
    }
  }

  data.frame(
    row = c(
      "Positive", "Negative", "Ungradable", "Total",
      "Diagnostic yield", "Worst-case performance"
    ),
    disease_positive = c(
      comma(c(cm$tp, cm$fn, cm$ug_pos, cm$dispos)),
      value("Diagnostic Yield (Disease Positive)"),
      value("Worst-Case Sensitivity")
    ),
    disease_negative = c(
      comma(c(cm$fp, cm$tn, cm$ug_neg, cm$disneg)),
      value("Diagnostic Yield (Disease Negative)"),
      value("Worst-Case Specificity")
    ),
    total = c(
      comma(c(cm$testpos, cm$testneg, cm$testug, cm$n)),
      value("Diagnostic Yield (Overall)"),
      paste("Youden's J =", value("Worst-Case Youden's J"))
    ),
    predictive_value = c(
      value("Positive Predictive Value"), value("Negative Predictive Value"),
      "", paste("Pre-test disease risk =", value("Pre-Test Disease Risk")), "", ""
    ),
    post_test_risk = c(
      value("Post-Test Disease Risk (Positive)"),
      value("Post-Test Disease Risk (Negative)"),
      value("Post-Test Disease Risk (Ungradable)"),
      "", "", ""
    ),
    likelihood_ratio = c(
      value("Likelihood Ratio (Positive)"),
      value("Likelihood Ratio (Negative)"),
      value("Likelihood Ratio (Ungradable)"),
      "", "", ""
    ),
    stringsAsFactors = FALSE
  )
}

#' Summary
#' @param object An object of class "dx_itd"
#' @param variable Variable to include in returned values
#' @param label Labels to include in returned values
#' @param show_var Include variable column in returned data?
#' @param show_label Include label in returned data?
#' @param measure Measures to include
#' @param ... additional arguments to be passed to or from methods
#' @export
summary.dx_itd <- function(object, variable = NA, label = NA, show_var = TRUE,
                           show_label = TRUE, measure = NA, ...) {
  tmp <- as.data.frame(object, variable = variable, label = label, measure = measure)
  tmp <- tmp[, c("variable", "label", "measure", "summary")]
  rownames(tmp) <- NULL

  if (!show_var) tmp <- subset(tmp, select = -c(variable))
  if (!show_label) tmp <- subset(tmp, select = -c(label))

  caption <- paste("Threshold=", object$options$setthreshold)

  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::kable(tmp, caption = caption, row.names = FALSE)
  } else {
    print(caption)
    print(tmp)
  }
}

#' Convert to a data frame
#' @param x An object of class "dx_itd"
#' @param row.names NULL or a character vector giving the row names for the
#'     data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'     names (to syntactic names: see make.names) is optional.
#' @param variable Variable to include in returned values
#' @param label Labels to include in returned values
#' @param measure Measures to include
#' @param ... additional arguments to be passed to or from methods
#' @export
as.data.frame.dx_itd <- function(x, row.names = NULL, optional = TRUE,
                                 variable = NA, label = NA, measure = NA, ...) {
  filter_measures(x$measures, variable = variable, label = label, measure = measure)
}
