comma <- function(x) {
  y <- prettyNum(x, big.mark = ",")
  y[y == "NA"] <- ""
}

format_pvalue <- function(p, accuracy = 0.01) {
  # Define the smallest non-zero number representable with the given accuracy
  smallest_nonzero <- accuracy

  if (p < smallest_nonzero) {
    # Construct the smallest non-zero representation based on accuracy
    smallest_nonzero_str <- formatC(accuracy, format = "f", digits = -log10(accuracy))
    return(paste0("p<", smallest_nonzero_str))
  } else {
    # Round and format the p-value
    rounded_p <- round(p, -log10(accuracy))
    return(paste("p=", formatC(rounded_p, format = "f", digits = -log10(accuracy)), sep = ""))
  }
}

conf_int <- function(est, lower, upper, accuracy = .1, percent = FALSE) {
  format_num <- function(num) {
    if (percent) {
      paste0(formatC(num * 100, format = "f", digits = -log10(accuracy)), "%")
    } else {
      formatC(num, format = "f", digits = -log10(accuracy), big.mark = ",")
    }
  }

  paste0(format_num(est), " (", format_num(lower), ", ", format_num(upper), ")")
}

dx_measure_df <- function(measure = "", estimate = "", fraction = "",
                          ci_type = "", notes = "", estimate_raw = NA,
                          lci_raw = NA, uci_raw = NA) {

  # Create data frame with base R
  tmp <- data.frame(
    measure = measure,
    estimate = estimate,
    fraction = fraction,
    ci_type = ci_type,
    notes = as.character(notes),
    rawestime = estimate_raw,
    rawlci = lci_raw,
    rawuci = uci_raw,
    stringsAsFactors = FALSE  # ensure that strings are not converted to factors
  )

  # Convert factors to characters if any
  tmp[] <- lapply(tmp, function(x) if(is.factor(x)) as.character(x) else x)

  return(tmp)
}
dx_measure_df <- function(measure = "", estimate = "", fraction = "",
                          ci_type = "", notes = "", estimate_raw = NA,
                          lci_raw = NA, uci_raw = NA) {
  # Create data frame with base R
  tmp <- data.frame(
    measure = measure,
    estimate = estimate,
    fraction = fraction,
    ci_type = ci_type,
    notes = notes,
    rawestime = estimate_raw,
    rawlci = lci_raw,
    rawuci = uci_raw,
    stringsAsFactors = FALSE  # ensure that strings are not converted to factors
  )

  # Convert factors to characters if any
  tmp[] <- lapply(tmp, function(x) if(is.factor(x)) as.character(x) else x)

  return(tmp)
}


dx_confusion_core <- function(predprob, truth, threshold, poslabel) {
  # Calculate predictions based on threshold
  pred <- as.numeric(predprob >= threshold)

  # Determine test and true results
  testresult <- as.numeric(pred == poslabel)
  trueresult <- as.numeric(truth == poslabel)

  # Calculate confusion matrix components
  tp <- sum(testresult == 1 & trueresult == 1)
  fn <- sum(testresult == 0 & trueresult == 1)
  tn <- sum(testresult == 0 & trueresult == 0)
  fp <- sum(testresult == 1 & trueresult == 0)

  # Calculate additional metrics
  dispos <- tp + fn  # Actual positives
  disneg <- tn + fp  # Actual negatives
  n <- length(truth) # Total observations, assuming 'truth' is complete
  correct <- tp + tn # Correct predictions
  testpos <- tp + fp # Predicted positives
  testneg <- tn + fn # Predicted negatives

  # Compile performance dataframe
  perfdf <- data.frame(tp, fn, tn, fp, dispos, disneg, n, correct, testpos, testneg)

  # Replace NAs with zeros - though there should be none in this context
  perfdf[is.na(perfdf)] <- 0

  return(perfdf)
}

createDiagnosticFunction <- function(calc_func, measure_name) {
  function(correct, n, citype, notes = "", ...) {
    # Perform the specific calculation for the measure
    measure_raw <- calc_func(correct, n, ...)

    # Calculate binomial confidence interval using binom package
    confint_res <- binom::binom.confint(correct, n, conf.level = 0.95, methods = citype)

    # Formatting results
    formatted_estimate <- conf_int(
      measure_raw,
      confint_res$lower,
      confint_res$upper,
      percent = TRUE
    )

    # Constructing the fraction string
    fraction <- paste0(comma(correct), "/", comma(n))

    # Returning structured output
    dx_measure_df(
      measure = measure_name,
      estimate = formatted_estimate,
      fraction = fraction,
      ci_type = citype,
      estimate_raw = measure_raw,
      lci_raw = confint_res$lower,
      uci_raw = confint_res$upper,
      notes = as.character(notes)
    )
  }
}

createRatioFunction <- function(calc_func, calc_sd_func, measure_name) {
  function(tp, tn, fp, fn) {
    # Apply continuity correction if any cell is zero
    continuity_correction <- if (tp == 0 | tn == 0 | fp == 0 | fn == 0) 0.5 else 0

    # Adjust values if necessary
    tp <- tp + continuity_correction
    tn <- tn + continuity_correction
    fp <- fp + continuity_correction
    fn <- fn + continuity_correction

    # Calculate the ratio (Odds or Likelihood)
    ratio <- calc_func(tp, tn, fp, fn)

    # Calculate standard deviation for log-transformed Ratio
    ratio_sd <- calc_sd_func(tp, tn, fp, fn)

    # Calculate confidence interval
    z_value <- 1.95996398454005  # 95% CI
    ratio_log <- log(ratio)
    ratio_ci_l <- exp(ratio_log - z_value * ratio_sd)
    ratio_ci_u <- exp(ratio_log + z_value * ratio_sd)

    # Format the result
    ratio_est <- conf_int(ratio, ratio_ci_l, ratio_ci_u, accuracy = .01)

    # Return structured output
    dx_measure_df(
      measure = measure_name,
      estimate = ratio_est,
      fraction = "",  # adjust as needed
      ci_type = if (continuity_correction > 0) "Adjusted for zero cells" else "Large sample",
      estimate_raw = ratio,
      lci_raw = ratio_ci_l,
      uci_raw = ratio_ci_u
    )
  }
}


calc_sensitivity <- function(tp, dispos) {
  tp / dispos
}

calc_specificity <- function(tn, disneg) {
  tn / disneg
}

calc_ppv <- function(tp, testpos) {
  tp / testpos
}

calc_npv <- function(tn, testneg) {
  tn / testneg
}

calc_accuracy <- function(correct, n) {
  correct / n
}

calc_sd_or <- function(tp, tn, fp, fn) {
  sqrt(sum(1 / c(tp, tn, fn, fp)))
}

calc_sd_lrtpos <- function(tp, tn, fp, fn) {
  sqrt(1/tp - 1/(tp + fn) + 1/fp - 1/(fp + tn))
}

calc_sd_lrtneg <- function(tp, tn, fp, fn) {
  sqrt(1/fn - 1/(tp + fn) + 1/tn - 1/(fp + tn))
}

calc_odds_ratio <- function(tp, tn, fp, fn) {
  (tp * tn) / (fp * fn)
}


calc_lr_neg <- function(tp, tn, fp, fn) {
  (fn / (tp + fn)) / (tn / (fp + tn))
}

calc_lr_pos <- function(tp, tn, fp, fn) {
  (tp / (tp + fn)) / (fp / (fp + tn))
}

calc_prevalence <- function(pos, n) {
  pos / n
}

calc_precision <- function(tp, fp) {
  tp / (tp + fp)
}

calc_recall <- function(tp, fn) {
  tp / (tp + fn)
}

calc_f1 <- function(tp, fp, fn) {
  # Calculate precision and recall directly within the function
  precision <- if(tp + fp == 0) 0 else tp / (tp + fp)
  recall <- if(tp + fn == 0) 0 else tp / (tp + fn)

  # Calculate F1 score
  if (precision + recall == 0) {
    return(0)  # Return 0 to handle division by zero
  } else {
    return(2 * (precision * recall) / (precision + recall))
  }
}



dx_auc <- function(truth, predprob) {
  rocest <- pROC::roc(truth, predprob, ci = T, quiet = TRUE)
  aucest <- pROC::auc(rocest)
  auctext <- as.character(pROC::ci(aucest))
  auc_raw <- as.numeric(auctext[2])
  auc_lci <- as.numeric(auctext[1])
  auc_uci <- as.numeric(auctext[3])
  auc <- conf_int(auc_raw, auc_lci, auc_uci, percent = F, accuracy = .001)

  dx_measure_df(
    measure = "AUC",
    estimate = auc,
    ci_type = "DeLong",
    estimate_raw = auc_raw,
    lci_raw = auc_lci,
    uci_raw = auc_uci
  )
}


dx_f1 <- function(predprob, truth, threshold, poslabel, bootreps = 1000, doboot = FALSE) {
  cm <- dx_confusion_core(predprob, truth, threshold, poslabel)

  f1_raw <- calc_f1(cm$tp, cm$fp, cm$fn)

  # Continue with bootstrapping or direct calculation as before
  # Bootstrapping for confidence interval
  if (doboot & requireNamespace("boot", quietly = TRUE)) {
    boot_res <- boot::boot(
      data = cbind(truth, predprob >= threshold),
      statistic = f1boot,
      R = bootreps
    )
    boot_ci_res <- boot::boot.ci(boot_res, type = "basic")
    f1_lci <- boot_ci_res$basic[4]
    f1_uci <- boot_ci_res$basic[5]
    f1 <- conf_int(f1_raw, f1_lci, f1_uci, percent = TRUE)
  } else {
    f1 <- formatC(round(f1_raw * 100, 1), format = "f", digits = 1)
    f1_lci <- NA
    f1_uci <- NA
  }

  dx_measure_df(
    measure = "F1 Score",
    estimate = f1,
    estimate_raw = f1_raw,
    ci_type = if (doboot) "Bootstrapped" else "",
    notes = if (doboot) paste0("Bootstraps: ", bootreps) else "",
    lci_raw = f1_lci,
    uci_raw = f1_uci
  )
}

f1boot <- function(data, indices) {
  # Extract data based on indices for bootstrapping
  d <- data[indices,]

  # Calculate confusion matrix components
  tp <- sum(d[,1] == 1 & d[,2] == 1)
  fp <- sum(d[,1] == 0 & d[,2] == 1)
  fn <- sum(d[,1] == 1 & d[,2] == 0)

  f1 <- calc_f1(tp, fp, fn)

  return(f1)
}




dx_sensitivity <- createDiagnosticFunction(calc_sensitivity, "Sensitivity")
dx_specificity <- createDiagnosticFunction(calc_specificity, "Specificity")
dx_ppv <- createDiagnosticFunction(calc_ppv, "Positive Predictive Value")
dx_npv <- createDiagnosticFunction(calc_npv, "Negative Predictive Value")
dx_accuracy <- createDiagnosticFunction(calc_accuracy, "Accuracy")
dx_prevalence <- createDiagnosticFunction(calc_prevalence, "Prevalence")

dx_recall <- calc_recall
dx_precision <- calc_precision

dx_odds_ratio <- createRatioFunction(calc_odds_ratio, calc_sd_or, "Odds Ratio")
dx_lrtneg <- createRatioFunction(calc_lr_neg, calc_sd_lrtneg, "LRT-")
dx_lrtpos <- createRatioFunction(calc_lr_pos, calc_sd_lrtpos, "LRT+")

check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))  {
    stop(paste0(pkg, " must be must be installed to use this function"))
  }
}


dx_breslow_day <- function(data, options, group_varname) {

  if (requireNamespace("DescTools", quietly = TRUE))  {

    predprob = data[[options$pred_varname]]
    pred = ifelse(predprob >= options$setthreshold, 1, 0)

    tmp <- data.frame(
      predprob = predprob,
      truth = data[[options$true_varname]],
      pred = pred,
      group = data[[group_varname]]
    )

    tmp <- tmp[!is.na(tmp$group), ]

    tab <- table(tmp$pred, tmp$truth, tmp$group)

    suppressWarnings({
      bd <- DescTools::BreslowDayTest(tab, OR = NA, correct = FALSE)
    })


    measure <- dx_measure_df(
      measure = "Breslow-Day",
      estimate = format_pvalue(bd$p.value, accuracy = 0.01),
      fraction = "",
      ci_type = "",
      notes = "Mantel-Haenszel OR estimate",
      estimate_raw = bd$p.value,
      lci_raw = NA,
      uci_raw = NA
    )

    # measure$variable <- group_varname
    # measure$label <- "Overall"
    # measure$threshold <- options$setthreshold
    # measure$n <- nrow(tmp)

    measure$n <- nrow(tmp)

  } else {
    measure <- dx_measure_df(
      measure = "Breslow-Day",
      estimate = NA_real_,
      fraction = "",
      ci_type = "",
      notes = "Test requires the package DescTools",
      estimate_raw = NA_real_,
      lci_raw = NA_real_,
      uci_raw = NA_real_
    )

    measure$n <- NA_real_

  }

  measure$variable <- group_varname
  measure$label <- "Overall"
  measure$threshold <- options$setthreshold


  return(measure)


}



