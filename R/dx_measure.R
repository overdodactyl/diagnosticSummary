dx_thresholds <- function(data, options) {
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  # Predictions can include 0 or 1, so remove duplicates after adding them
  thresholds <- sort(unique(c(0, predprob, 1)), decreasing = TRUE)

  # Count predictions at or above each threshold in each outcome class
  n_at_or_above <- function(x) {
    length(x) - findInterval(thresholds, sort(x), left.open = TRUE)
  }
  above_1 <- n_at_or_above(predprob[truth == 1])
  above_0 <- n_at_or_above(predprob[truth == 0])
  n_1 <- sum(truth == 1)
  n_0 <- sum(truth == 0)

  # Predictions at or above the threshold are positive; poslabel = 0 swaps
  # the positive class
  if (options$poslabel == 1) {
    tp <- above_1
    fp <- above_0
    fn <- n_1 - above_1
    tn <- n_0 - above_0
  } else {
    tp <- n_0 - above_0
    fp <- n_1 - above_1
    fn <- above_0
    tn <- above_1
  }

  # Store counts as doubles
  cm <- cm_from_counts(
    tp = as.numeric(tp),
    fn = as.numeric(fn),
    tn = as.numeric(tn),
    fp = as.numeric(fp)
  )

  # Net benefit (Vickers & Elkin, 2006): true positives and false positives
  # per patient, with false positives weighted by the odds at the threshold
  # probability of the event. Predictions are probabilities of outcome 1, so
  # with poslabel = 0 the event's threshold probability is 1 - threshold.
  event_threshold <- if (options$poslabel == 1) thresholds else 1 - thresholds
  weight <- event_threshold / (1 - event_threshold)
  net_benefit <- cm$tp / cm$n - weight * cm$fp / cm$n

  res <- data.frame(
    threshold = thresholds,
    event_threshold = event_threshold,
    sensitivity = dx_sensitivity(cm, detail = "simple"),
    specificity = dx_specificity(cm, detail = "simple"),
    precision = dx_precision(cm, detail = "simple"),
    net_benefit = net_benefit,
    informedness = dx_informedness(cm, detail = "simple"),
    npv = dx_npv(cm, detail = "simple"),
    ppv = dx_ppv(cm, detail = "simple"),
    f1 = dx_f1(cm, detail = "simple"),
    fpr = dx_fpr(cm, detail = "simple"),
    tp = cm$tp,
    tn = cm$tn,
    fp = cm$fp,
    fn = cm$fn
  )

  return_df(res)
}

# PPV and NPV across a range of prevalences, from the sensitivity and
# specificity in a confusion matrix
dx_prevalence_analysis <- function(cm) {
  sensitivity <- dx_sensitivity(cm, detail = "simple")
  specificity <- dx_specificity(cm, detail = "simple")
  prevalence <- seq(0.01, 0.99, by = 0.005)

  data.frame(
    prevalence = prevalence,
    ppv = (sensitivity * prevalence) /
      (sensitivity * prevalence + (1 - specificity) * (1 - prevalence)),
    npv = (specificity * (1 - prevalence)) /
      ((1 - sensitivity) * prevalence + specificity * (1 - prevalence))
  )
}

dx_rank <- function(data, options) {
  truth <- data[[options$true_varname]]
  predprob <- data[[options$pred_varname]]

  # Rank cases from most to least likely to be the event. Predictions are
  # probabilities of outcome 1, so with poslabel = 0 the lowest come first.
  data <- data.frame(truth, predprob)
  data <- data[order(if (options$poslabel == 1) -data$predprob else data$predprob), ]
  event <- as.numeric(data$truth == options$poslabel)

  # Calculate cumulative true positives
  data$cumulativeTruePositives <- cumsum(event)

  # Calculate the fraction of positives at each threshold
  totalPositives <- sum(event)
  data$gain <- data$cumulativeTruePositives / totalPositives

  # Calculate the lift for each percentile
  n <- nrow(data)
  data$percentile <- (1:n) / n # Represents the percentile of the population up to each instance
  data$randomModelGain <- data$percentile * totalPositives
  data$lift <- ifelse(
    data$randomModelGain > 0,
    data$cumulativeTruePositives / data$randomModelGain,
    0
  )

  # Calculate cumulative true positive and true negative rates for KS Plot
  data$cumulativeTPR <- data$cumulativeTruePositives / totalPositives # True Positive Rate
  data$cumulativeFPR <- cumsum(!event) / sum(!event) # False Positive Rate

  # Tied predictions are all above or all below any threshold, so only the
  # last row of each tie group can be reached by a threshold
  data$reachable <- !duplicated(data$predprob, fromLast = TRUE)

  return(data)
}




dx_measure <- function(data, threshold, options, var = "Overall",
                       label = "Overall",
                       threshold_analysis = dx_thresholds(data, options)) {

  # create vector for the prediction
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  cm <- dx_cm(
    predprob = predprob,
    truth = truth,
    threshold = threshold,
    poslabel = options$poslabel
  )

  # Common arguments for metrics that use a confusion matrix and citype
  common_cm_args <- list(cm = cm, citype = options$citype)
  common_boot_args <- list(cm = cm, boot = options$doboot, bootreps = options$bootreps)
  common_ratio_args <- list(cm = cm)

  metric_calculations <- list(
    auc = list(fun = dx_auc, params = list(truth = truth, predprob = predprob, direction = options$direction)),
    accuracy = list(fun = dx_accuracy, params = common_cm_args),
    sensitivity = list(fun = dx_sensitivity, params = common_cm_args),
    specificity = list(fun = dx_specificity, params = common_cm_args),
    ppv = list(fun = dx_ppv, params = common_cm_args),
    npv = list(fun = dx_npv, params = common_cm_args),
    lrt_pos = list(fun = dx_lrt_pos, params = common_ratio_args),
    lrt_neg = list(fun = dx_lrt_neg, params = common_ratio_args),
    or = list(fun = dx_odds_ratio, params = common_ratio_args),
    f1 = list(fun = dx_f1, params = common_boot_args),
    f2 = list(fun = dx_f2, params = common_boot_args),
    prevalence = list(fun = dx_prevalence, params = common_cm_args),
    fnr = list(fun = dx_fnr, params = common_cm_args),
    fpr = list(fun = dx_fpr, params = common_cm_args),
    fdr = list(fun = dx_fdr, params = common_cm_args),
    pr_auc = list(
      fun = dx_auc_pr,
      params = list(
        precision = threshold_analysis$precision,
        recall = threshold_analysis$sensitivity)
      ),
    kappa = list(fun = dx_cohens_kappa, params = list(cm = cm)),
    mcc = list(fun = dx_mcc, params = common_boot_args),
    balanced_accuracy = list(fun = dx_balanced_accuracy, params = common_boot_args),
    informedness = list(fun = dx_informedness, params = common_boot_args),
    markedness = list(fun = dx_markedness, params = common_boot_args),
    g_mean = list(fun = dx_g_mean, params = common_boot_args),
    fowlkes_mallows = list(fun = dx_fowlkes_mallows, params = common_boot_args),
    brier = list(fun = dx_brier, params = list(predprob = predprob, truth = truth)),
    chisquare = list(fun = dx_chi_square, params = common_ratio_args),
    fishers = list(fun = dx_fishers_exact, params = common_ratio_args),
    gtest = list(fun = dx_g_test, params = common_ratio_args)
  )

  # Function to call each metric function with parameters
  call_metric <- function(metric) {
    do.call(metric$fun, metric$params)
  }

  # Apply the generic function to all metrics and combine results
  metrics_list <- lapply(metric_calculations, call_metric)

  if (!is.na(options$prevalence)) {
    metrics_list$npv_prevalence <- dx_npv_prevalence(cm, prevalence = options$prevalence)
    metrics_list$ppv_prevalence <- dx_ppv_prevalence(cm, prevalence = options$prevalence)
  }


  # Combine all metric results into one data frame
  results <- do.call(rbind, metrics_list)

  results$threshold <- threshold
  results$variable <- var
  results$label <- label
  results$n <- nrow(data)

  # Adjust column order
  to_first <- c("variable", "label")
  results[c(to_first, setdiff(names(results), to_first))]
}


dx_group_measure <- function(data, options, group_varname) {
  # Use factor level order
  group_labels <- levels(data[[group_varname]])

  # Warn about subgroups that only contain one outcome class
  truth <- data[[options$true_varname]]
  one_class <- vapply(group_labels, function(g) {
    length(unique(truth[data[[group_varname]] %in% g])) < 2
  }, logical(1))
  if (any(one_class)) {
    warning(paste0(
      "Subgroup(s) of ", group_varname, " with only one outcome class: ",
      paste(group_labels[one_class], collapse = ", "), ". ",
      "AUC, odds ratio, and likelihood ratios are NA, and these subgroups ",
      "are excluded from the Breslow-Day test."
    ), call. = FALSE)
  }

  # Levels were dropped in dx(), so every level has rows
  datalist <- lapply(group_labels, function(g) {
    dx_measure(
      data = data[data[[group_varname]] %in% g, ],
      threshold = options$setthreshold,
      options = options,
      var = group_varname,
      label = g
    )
  })
  res <- do.call(rbind, datalist)
  bd <- dx_breslow_day(data, options, group_varname)

  rbind(res, bd)
}


#' Compare Multiple Classification Models
#'
#' Compares multiple classification models pairwise using various statistical tests
#' to assess differences in performance metrics. It supports both paired and unpaired
#' comparisons.
#'
#' @param dx_list A list of `dx` objects representing the models to be compared.
#'                Each `dx` object should be the result of a call to `dx()`.
#' @param paired Logical, indicating whether the comparisons should be treated as paired.
#'               Paired comparisons are appropriate when models are evaluated on the
#'               same set of instances (e.g., cross-validation or repeated measures).
#'
#' @return A `dx_compare` object containing a list of `dx` objects and a data frame of
#'         pairwise comparison results for each test conducted.
#'
#' @details This function is a utility to perform a comprehensive comparison between
#'          multiple classification models. Based on the value of `paired`, it will
#'          perform appropriate tests.  The resulting object can be used it further
#'          functions like `dx_plot_rocs.`
#'
#' @examples
#' dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
#' dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
#' dx_list <- list(dx_glm, dx_rf)
#' dx_comp <- dx_compare(dx_list, paired = TRUE)
#' print(dx_comp$tests)
#' @seealso [dx_delong()], [dx_z_test()], [dx_mcnemars()]
#' for more details on the tests used for comparisons.
#'
#' @export
dx_compare <- function(dx_list, paired = TRUE) {
  dx_list <- validate_dx_list(dx_list)


  combinations <- utils::combn(names(dx_list), 2)

  res <- NULL

  for (i in seq_len(ncol(combinations))) {
    n1 <- combinations[1, i]
    n2 <- combinations[2, i]
    dx1 <- dx_list[[n1]]
    dx2 <- dx_list[[n2]]
    pair_res <- dx_delong(dx1, dx2, paired = paired)

    if (!paired) {
      metrics <- c("accuracy", "ppv", "npv", "fnr", "fpr", "fdr", "sensitivity", "specificity")
      for (metric in metrics) {
        ztest <- dx_z_test(dx1, dx2, metric = metric)
        pair_res <- rbind(pair_res, ztest)
      }
    } else {
      mcnemars <- dx_mcnemars(dx1, dx2)
      pair_res <- rbind(pair_res, mcnemars)
    }

    pair_res$models <- two_model_name(n1, n2)
    res <- rbind(res, pair_res)
  }

  structure(
    list(
      dx_list = dx_list,
      tests = res
    ),
    class = "dx_compare"
  )
}
