#' Summarize diagnostic results for a model
#'
#' @export

dx_summary <- function(data, pred_varname, threshold, true_varname, poslabel=1,
                       citype="exact",bootreps=2000,bootseed=20190617,doboot=TRUE){

  set.seed(bootseed)

  # create vector for the prediction
  predprob <- data[[pred_varname]]
  truth <- data[[true_varname]]

  perfdf <- dx_confusion_core(predprob = predprob, truth = truth, threshold = threshold, poslabel = poslabel)


  dx_or <- dx_odds_ratio(perfdf$tp, perfdf$tn, perfdf$fp, perfdf$fn)
  senres <- dx_sensitivity(tp = perfdf$tp, dispos = perfdf$dispos, citype, threshold)
  specres <- dx_specificity(perfdf$tn, perfdf$disneg, citype, threshold)
  accres <- dx_accuracy(perfdf$correct, perfdf$n, citype)
  ppvres <- dx_ppv(perfdf$tp, perfdf$testpos, citype)
  npvres <- dx_npv(perfdf$tn, perfdf$testneg, citype)
  precision <- dx_precision(perfdf$tp, perfdf$fp)
  recall <- dx_recall(perfdf$tp, perfdf$fn)
  f1 <- dx_f1(predprob, truth, precision, recall, bootreps, doboot)
  auc <- dx_auc(truth, predprob)

  #set data in order we want to appear
  results_print <- dplyr::bind_rows(auc, accres, senres, specres, ppvres, npvres, dx_or, f1)

  return(results_print)
}

#' Grouped Summary
#' @export
dx_grouped_summary <- function(data, group_varname, pred_varname,
                                threshold, true_varname, poslabel, citype, bootreps, bootseed,doboot){

  grouped_df <- data %>% dplyr::select(group_varname) %>% dplyr::distinct()
  group_labels <- as.vector(grouped_df[,1])
  group_labels <- group_labels[!is.na(group_labels)]

  datalist = list()

  j <- 1
  for (i in group_labels){
    subsetdata <- data %>% dplyr::filter(!!as.name(group_varname) == i)
    if (dim(subsetdata)[1] > 0 ) {
      # print(paste0("Grouped Results for ", group_varname, "  ", i))
      temp <- dx_summary(data = subsetdata,
                          pred_varname = pred_varname,
                          threshold = threshold,
                          true_varname = true_varname,
                          poslabel = poslabel,
                          citype = citype,
                          bootreps = bootreps,
                          bootseed = bootseed,
                          doboot = doboot)
      res <- temp %>%
        dplyr::mutate(Variable = group_varname, Group = i) %>%
        dplyr::select(Variable, Group, dplyr::everything())

      datalist[[j]] <- res
      j <- j + 1
    }

  }
  do.call(rbind, datalist)
}
