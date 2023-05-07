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



  lrt_neg <- dx_lrtneg(tp = perfdf$tp, tn = perfdf$tn, fp = perfdf$fp, fn = perfdf$fn)
  lrt_pos <- dx_lrtpos(tp = perfdf$tp, tn = perfdf$tn, fp = perfdf$fp, fn = perfdf$fn)
  dx_or <- dx_odds_ratio(perfdf$tp, perfdf$tn, perfdf$fp, perfdf$fn)
  senres <- dx_sensitivity(
    tp = perfdf$tp, dispos = perfdf$dispos,
    options$citype, threshold
  )
  specres <- dx_specificity(perfdf$tn, perfdf$disneg, options$citype, threshold)
  accres <- dx_accuracy(perfdf$correct, perfdf$n, options$citype)
  ppvres <- dx_ppv(perfdf$tp, perfdf$testpos, options$citype)
  npvres <- dx_npv(perfdf$tn, perfdf$testneg, options$citype)
  precision <- dx_precision(perfdf$tp, perfdf$fp)
  recall <- dx_recall(perfdf$tp, perfdf$fn)
  f1 <- dx_f1(
    predprob, truth, precision, recall, options$bootreps,
    options$doboot
  )
  auc <- dx_auc(truth, predprob)

  # set data in order we want to appear
  results <- dplyr::bind_rows(
    auc, accres, senres, specres,
    ppvres, npvres, lrt_pos, lrt_neg, dx_or, f1
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
  group_labels <- data %>%
    dplyr::select(group_varname) %>%
    dplyr::distinct() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::pull(group_varname)
  group_labels <- group_labels[!is.na(group_labels)]

  datalist <- list()

  for (i in seq_along(group_labels)) {
    subsetdata <- data %>%
      dplyr::filter(!!as.name(group_varname) == group_labels[i])
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

  dplyr::bind_rows(res, bd)

}
