#' Summarize diagnostic results for a model
#'
#' @param data A tbl
#' @param pred_varname Colomn name containing AI prediction (string)
#' @param threshold A numeric value represnting the threshold used to identify AI prediction
#' @param true_varname Column name containt AI reference standard (string)
#' @param poslabel Positive class.  Variable should be coded as 0/1 with 1 being the event
#' @param citpye Confidence interval type.
#' @param bootreps Number of bootstrap samples used to generate F1 score CI
#' @param bootseed Seed value to be used when calculating bootsraped CI's
#' @param doboot Logical.  Generate bootstrap estimate of F1 confidence interval?


true_varname <- "AS_label_gen"  # text string with variable name for the reference standard
pred_varname <- "est_AS_pos"  # text string for the AI prediction
outcomelabel <- "EF <= 35%"
classlabels <- c("Negative", "Positive")  ## needs to be 0, 1 order
## ~~~~~~~~~~~~~~~~~~ Set some Program Parameters ~~~~~~~~~~~~~~~~~~~~~~~ ##
## Set confidence interval type and postive class value

setthreshold <- .5
generaterange<-TRUE  # use the start and stop values below to generate sub tables
startthreshold <- .3
stopthreshold <- .6
bythreshold <- .1 #Have numboot, below, small if using a range of thresholds and bootstrapping F1

confinttype<- "exact"  # Just leave this as exact
posclass <- 1  # Data should be coded 0/1 with 1 being the event
validcodes <- c(0,1)  # should be binary for this program
numboot <- 2000 # number of bootstrap samples used to generate F1 score CI
bootseed <- 20191015
bootf1 <-FALSE
doboot <- FALSE

citype=confinttype

threshold <- .3
poslabel <- posclass

load("sample_data.Rda")
data <- working_df

dx_summary <- function(data, pred_varname, threshold, true_varname, poslabel=1,
                       citype="exact",bootreps=2000,bootseed=20190617,doboot=TRUE){


  # create vector for the prediction
  predprob <- data[[pred_varname]]
  truth <- data[[true_varname]]

  perfdf <- dx_confusion_core(predprob = predprob, truth = truth, threshold = threshold)


  library(boot)
  set.seed(bootseed)


  ## generate odds ratio
  dx_or <- dx_odds_ratio(perfdf$tp, perfdf$tn, perfdf$fp, perfdf$fn)
  # perfdf <- cbind(perfdf, dx_or)

  senres <- dx_sensitivity(tp = perfdf$tp, dispos = perfdf$dispos, citype)
  specres <- dx_specificity(perfdf$tn, perfdf$disneg, citype)
  accres <- dx_accuracy(perfdf$correct, perfdf$n, citype)
  ppvres <- dx_ppv(perfdf$tp, perfdf$testpos, citype)
  npvres <- dx_npv(perfdf$tn, perfdf$testneg, citype)

  ## Set up f1 score
  precision <- dx_precision(perfdf$tp, perfdf$fp)
  recall <- dx_recall(perfdf$tp, perfdf$fn)
  f1 <- dx_f1(predprob, truth, precision, recall, bootreps, doboot)

  ## generate AUC
  auc <- dx_auc(truth, predprob)

  # ss <- dplyr::bind_cols(perfdf, senres, specres, accres, ppvres, npvres,
  #                        precision = precision, recall = recall, f1, auc)


  df1 <- dx_measure_df(measure = "Sensitivity",
                       estimate = senres$sensitivity,
                       fraction = senres$sens_frac,
                       ci_type = citype,
                       notes = paste0(">=", threshold))

  df2 <- dx_measure_df(measure = "Specificity",
                       estimate = specres$specificity,
                       fraction = specres$spec_frac,
                       ci_type = citype,
                       notes = paste0("<", threshold))

  df3 <- dx_measure_df(measure = "Accuracy",
                       estimate = accres$accuracy,
                       fraction = accres$acc_frac,
                       ci_type = citype)

  df4 <- dx_measure_df(measure = "Positive Predictive Value",
                       estimate = ppvres$ppv,
                       fraction = ppvres$ppv_frac,
                       ci_type = citype)

  df5 <- dx_measure_df(measure = "Negative Predictive Value",
                       estimate = npvres$npv,
                       fraction = npvres$npv_frac,
                       ci_type = citype)

  df6 <- dx_measure_df(measure = "Odds Ratio",
                       estimate = dx_or$or,
                       ci_type = "Large sample",
                       notes = dx_or$ornote,
                       estimate_raw = dx_or$or_raw,
                       lci_raw = exp(dx_or$lnor_l),
                       uci_raw = exp(dx_or$lnor_u))

  df8 <- dx_measure_df(measure = "F1 Score",
                       estimate = f1$f1,
                       ci_type = if (doboot) "Bootstrapped" else "",
                       notes = if (doboot) paste0("B=", bootreps, "") else "")

  df9 <- dx_measure_df(measure = "AUC",
                       estimate = auc$auc,
                       ci_type = "DeLong")

  results_print <- dplyr::bind_rows(df9, df3,df1,df2,df4,df5,df6,  df8) #set data in order we want to appear

  return(results_print)
}

dx_summary(working_df,
           pred_varname=pred_varname,
           threshold=setthreshold,
           true_varname=true_varname,
           poslabel=posclass,
           citype=confinttype,
           bootreps=numboot,
           bootseed=bootseed,
           doboot=doboot)
