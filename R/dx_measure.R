dx_thresholds <- function(data, options) {

  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  unique_thresholds <- unique(data[[options$pred_varname]])
  unique_thresholds <- sort(c(0, unique_thresholds, 1), decreasing = TRUE)

  # Initialize vectors to store sensitivity and specificity values
  sensitivities <- numeric(length(unique_thresholds))
  specificities <- numeric(length(unique_thresholds))
  precision <- numeric(length(unique_thresholds))
  net_benefits <- numeric(length(unique_thresholds))
  informedness <- numeric(length(unique_thresholds))

  sensitivities <- specificities <- numeric(length(unique_thresholds))
  precision <- net_benefits <- informedness <- sensitivities
  tp <- fp <- fn <- tn <- sensitivities


  # recall <- numeric(length(unique_thresholds))

  # Step 2: Calculate sensitivity and specificity for each unique threshold
  for (i in seq_along(unique_thresholds)) {
    threshold <- unique_thresholds[i]
    perf <- dx_cm(predprob, truth, threshold, options$poslabel)

    sensitivities[i] <- dx_sensitivity(perf, detail = "simple")
    specificities[i] <- dx_specificity(perf, detail = "simple")
    precision[i] <- dx_precision(perf, detail = "simple")
    informedness[i] <- dx_informedness(perf, detail = "simple")
    tp[i] <- perf$tp
    fp[i] <- perf$fp
    fn[i] <- perf$fn
    tn[i] <- perf$tn

    # Calculate net benefit
    weight <- threshold / (1 - threshold)
    tp_rate <- sensitivities[i]  # True positive rate is sensitivity
    fp_rate <- 1 - specificities[i]  # False positive rate is 1 - specificity
    net_benefits[i] <- tp_rate - weight * fp_rate


  }

  res <- data.frame(
    threshold = unique_thresholds,
    sensitivity = sensitivities,
    specificity = specificities,
    precision = precision,
    net_benefit = net_benefits,
    informedness = informedness,
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn
  )

  return_df(res)

}

dx_prevalence_analysis <- function(data, options) {
  # create vector for the prediction
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  perfdf <- dx_cm(
    predprob = predprob,
    truth = truth,
    threshold = options$setthreshold,
    poslabel = options$poslabel
  )

  sensitivity <- dx_sensitivity(perfdf, detail = "simple")
  specificity <-  dx_specificity(perfdf, detail = "simple")

  prevalences <- seq(0.01, 0.99, by = 0.005)

  # Initialize vectors for PPV and NPV
  ppv_values <- numeric(length(prevalences))
  npv_values <- numeric(length(prevalences))

  # Iterate over prevalences to calculate PPV and NPV
  for (i in seq_along(prevalences)) {
    prevalence <- prevalences[i]

    # Calculate PPV and NPV using prevalence, sensitivity, and specificity
    ppv_values[i] <- (sensitivity * prevalence) / (sensitivity * prevalence + (1 - specificity) * (1 - prevalence))
    npv_values[i] <- (specificity * (1 - prevalence)) / ((1 - sensitivity) * prevalence + specificity * (1 - prevalence))
  }

  data.frame(
    prevalence = prevalences,
    ppv = ppv_values,
    npv = npv_values
  )

}

dx_rank <- function(data, options) {
  truth <- data[[options$true_varname]]
  predprob <- data[[options$pred_varname]]

  # Combine and sort by predicted probability in descending order
  data <- data.frame(truth, predprob)
  data <- data[order(-data$predprob),]

  # Calculate cumulative true positives
  data$cumulativeTruePositives <- cumsum(data$truth)

  # Calculate the fraction of positives at each threshold
  totalPositives <- sum(data$truth)
  data$gain <- data$cumulativeTruePositives / totalPositives

  # Calculate the lift for each percentile
  n <- nrow(data)
  data$percentile <- (1:n) / n  # Represents the percentile of the population up to each instance
  data$randomModelGain <- data$percentile * totalPositives
  data$lift <- ifelse(data$randomModelGain > 0, data$cumulativeTruePositives / data$randomModelGain, 0)

  # Calculate cumulative true positive and true negative rates for KS Plot
  data$cumulativeTPR <- data$cumulativeTruePositives / totalPositives  # True Positive Rate
  data$cumulativeFPR <- cumsum(!data$truth) / sum(!data$truth)  # False Positive Rate

  return(data)
}




dx_measure <- function(data, threshold, options, var = "Overall",
                       label = "Overall") {

  # create vector for the prediction
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  cm <- dx_cm(
    predprob = predprob,
    truth = truth,
    threshold = threshold,
    poslabel = options$poslabel
  )

  threshold_analysis <- dx_thresholds(data, options)

  # metrics_list <- list(
  #   auc = dx_auc(truth, predprob),
  #   accuracy = dx_accuracy(cm, citype = options$citype),
  #   sensitivity = dx_sensitivity(cm, citype = options$citype),
  #   specificity = dx_specificity(cm, citype = options$citype),
  #   ppv = dx_ppv(cm, citype = options$citype),
  #   npv = dx_npv(cm, citype = options$citype),
  #   lrt_pos = dx_lrt_pos(cm),
  #   lrt_neg = dx_lrt_neg(cm),
  #   or = dx_odds_ratio(cm),
  #   f1 = dx_f1(cm, bootreps = options$bootreps),
  #   prevalence = dx_prevalence(cm, citype = options$citype),
  #   fnr = dx_fnr(cm, citype = options$citype),
  #   fpr = dx_fpr(cm, citype = options$citype),
  #   fdr = dx_fdr(cm, citype = options$citype),
  #   pr_auc =  dx_auc_pr(threshold_analysis$precision, threshold_analysis$sensitivity),
  #   kappa = dx_cohens_kappa(cm),
  #   mcc = dx_mcc(cm, bootreps = options$bootreps)
  # )
  #
  # # Combine all metric results into one data frame
  # results <- do.call(rbind, metrics_list)


  # Common arguments for metrics that use a confusion matrix and citype
  common_cm_args <- list(cm = cm, citype = options$citype)
  common_boot_args <- list(cm = cm, boot = options$doboot, bootreps = options$bootreps)
  common_ratio_args <- list(cm = cm)

  metric_calculations <- list(
    auc = list(fun = dx_auc, params = list(truth = truth, predprob = predprob)),
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
    pr_auc = list(fun = dx_auc_pr, params = list(precision = threshold_analysis$precision, recall = threshold_analysis$sensitivity)),  # Assuming precision and recall are named thus in threshold_analysis
    kappa = list(fun = dx_cohens_kappa, params = list(cm = cm)),
    mcc = list(fun = dx_mcc, params = common_boot_args),
    balanced_accuracy = list(fun = dx_balanced_accuracy, params = common_boot_args),
    informedness = list(fun = dx_informedness, params = common_boot_args),
    markedness = list(fun = dx_markedness, params = common_boot_args),
    g_mean = list(fun = dx_g_mean, params = common_boot_args),
    fowlkes_mallows = list(fun = dx_fowlkes_mallows, params = common_boot_args),
    brier = list(fun = dx_brier, params = list(predprob = predprob, truth = truth)),
    chisquare = list(fun = dx_chi_square, params = common_ratio_args),
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

  group_col <- unique(data[[group_varname]])

  # Convert them to character
  group_labels <- as.character(group_col)
  group_labels <- group_labels[!is.na(group_labels)]

  datalist <- list()

  for (i in seq_along(group_labels)) {

    subsetdata <- data[data[[group_varname]] == group_labels[i] , ]

    if (dim(subsetdata)[1] > 0) {
      datalist[[i]] <- dx_measure(
        data = subsetdata,
        threshold = options$setthreshold,
        options = options,
        var = group_varname,
        label = group_labels[i]
      )
    }
  }
  res <- do.call(rbind, datalist)
  bd <- dx_breslow_day(data, options, group_varname)

  rbind(res, bd)

}
