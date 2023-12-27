dx_thresholds <- function(data, options) {

  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  unique_thresholds <- unique(data[[options$pred_varname]])
  unique_thresholds <- sort(c(0, unique_thresholds, 1), decreasing = TRUE)

  # Initialize vectors to store sensitivity and specificity values
  sensitivities <- numeric(length(unique_thresholds))
  specificities <- numeric(length(unique_thresholds))
  precision <- numeric(length(unique_thresholds))
  # recall <- numeric(length(unique_thresholds))

  # Step 2: Calculate sensitivity and specificity for each unique threshold
  for (i in seq_along(unique_thresholds)) {
    threshold <- unique_thresholds[i]
    perf <- dx_confusion_core(predprob, truth, threshold, options$poslabel)

    sensitivities[i] <- calc_sensitivity(perf$tp, perf$dispos)
    specificities[i] <- calc_specificity(perf$tn, perf$disneg)
    precision[i] <- calc_ppv(perf$tp, perf$testpos)

  }

  data.frame(
    threshold = unique_thresholds,
    sensitivity = sensitivities,
    specificity = specificities,
    precision = precision
  )


}



dx_measure <- function(data, threshold, options, var = "Overall",
                       label = "Overall") {

  # create vector for the prediction
  predprob <- data[[options$pred_varname]]
  truth <- data[[options$true_varname]]

  perfdf <- dx_confusion_core(
    predprob = predprob,
    truth = truth,
    threshold = threshold,
    poslabel = options$poslabel
  )

  prevalence  <- dx_prevalence(perfdf$dispos, perfdf$n, options$citype)
  lrt_neg <- dx_lrtneg(tp = perfdf$tp, tn = perfdf$tn, fp = perfdf$fp, fn = perfdf$fn)
  lrt_pos <- dx_lrtpos(tp = perfdf$tp, tn = perfdf$tn, fp = perfdf$fp, fn = perfdf$fn)
  dx_or <- dx_odds_ratio(perfdf$tp, perfdf$tn, perfdf$fp, perfdf$fn)
  senres <- dx_sensitivity(
    perfdf$tp,
    perfdf$dispos,
    options$citype,
    notes = paste0(">=", threshold)
  )
  specres <- dx_specificity(
    perfdf$tn,
    perfdf$disneg,
    citype = options$citype,
    notes = paste0("<", threshold)
  )
  accres <- dx_accuracy(perfdf$correct, perfdf$n, options$citype)
  ppvres <- dx_ppv(perfdf$tp, perfdf$testpos, options$citype)
  npvres <- dx_npv(perfdf$tn, perfdf$testneg, options$citype)
  precision <- dx_precision(perfdf$tp, perfdf$fp)
  recall <- dx_recall(perfdf$tp, perfdf$fn)
  f1 <- dx_f1(
    predprob, truth, threshold, options$poslabel, bootreps = 1000, doboot = FALSE
  )
  auc <- dx_auc(truth, predprob)

  fnr <- dx_fnr(perfdf$fn, perfdf$dispos, options$citype)
  fpr <- dx_fpr(perfdf$fp, perfdf$disneg, options$citype)
  fdr <- dx_fdr(perfdf$fp, perfdf$testpos, options$citype)

  pr_auc <- dx_pr_auc(data, options)

  # set data in order we want to appear
  results <- rbind(
    auc, accres, senres, specres,
    ppvres, npvres, lrt_pos, lrt_neg, dx_or, f1,
    prevalence,
    fnr,
    fpr,
    fdr,
    pr_auc
  )

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
