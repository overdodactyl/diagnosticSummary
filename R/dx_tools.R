conf_int <- function(est, lower, upper, accuracy = .1, percent = F) {
  if (percent) {
    est <- scales::percent(est, accuracy = accuracy)
    lower <- scales::percent(lower, accuracy = accuracy)
    upper <- scales::percent(upper, accuracy = accuracy)
  } else {
    est <- scales::comma(est, accuracy = accuracy)
    lower <- scales::comma(lower, accuracy = accuracy)
    upper <- scales::comma(upper, accuracy = accuracy)
  }
  paste0(est, " (", lower, ", ", upper, ")")
}


dx_confusion_core <- function(predprob, truth, threshold, poslabel) {
  pred <- ifelse(predprob >= threshold, 1, 0)

  tempdf <- data.frame(pred, truth)
  tempdf$testresult <- ifelse(tempdf$pred == poslabel, 1, 0)
  tempdf$trueresult <- ifelse(tempdf$truth == poslabel, 1, 0)

  neg <- dplyr::filter(tempdf, trueresult == 0)
  pos <- dplyr::filter(tempdf, trueresult == 1)

  ## now count the tps and fns
  senscount <- pos %>%
    dplyr::group_by(testresult) %>%
    dplyr::summarize(counts = dplyr::n())
  speccount <- neg %>%
    dplyr::group_by(testresult) %>%
    dplyr::summarize(counts = dplyr::n())

  tp_1 <- dplyr::ungroup(senscount) %>%
    dplyr::filter(testresult == 1) %>%
    dplyr::select(counts) %>%
    as.vector()
  tp <- ifelse(length(tp_1[, 1]) == 0, 0, tp_1[1, 1]) %>% as.numeric()
  fn_1 <- dplyr::ungroup(senscount) %>%
    dplyr::filter(testresult == 0) %>%
    dplyr::select(counts) %>%
    as.vector()
  fn <- ifelse(length(fn_1[, 1]) == 0, 0, fn_1[1, 1]) %>% as.numeric()
  tn_1 <- dplyr::ungroup(speccount) %>%
    dplyr::filter(testresult == 0) %>%
    dplyr::select(counts) %>%
    as.vector()
  tn <- ifelse(length(tn_1[, 1]) == 0, 0, tn_1[1, 1]) %>% as.numeric()
  fp_1 <- dplyr::ungroup(speccount) %>%
    dplyr::filter(testresult == 1) %>%
    dplyr::select(counts) %>%
    as.vector()
  fp <- ifelse(length(fp_1[, 1]) == 0, 0, fp_1[1, 1]) %>% as.numeric()

  perfdf <- data.frame(tp, fn, tn, fp) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 0, .))

  dispos <- perfdf$tp + perfdf$fn # Actaul positives
  disneg <- perfdf$tn + perfdf$fp # Actual negatives
  n <- dispos + disneg # Total observations
  correct <- perfdf$tp + perfdf$tn # Correct
  testpos <- perfdf$tp + perfdf$fp # Predicted positive
  testneg <- perfdf$tn + perfdf$fn # Predicted negative

  perfdf <- cbind(perfdf, dispos, disneg, n, correct, testpos, testneg)

  # fixing some zero cell issues
  dplyr::mutate_all(perfdf, ~ ifelse(is.na(.), 0, .))
}

# Compute diagnositc odds ratio
dx_odds_ratio <- function(tp, tn, fp, fn) {
  if (tp == 0 | tn == 0 | fp == 0 | fn == 0) {
    or_raw <- (tp + 0.5) * (tn + 0.5) / ((fp + 0.5) * (fn + 0.5))
    sqrtsdlnor <- sqrt(1 / (tp + 0.5) + 1 / (tn + 0.5) +
      1 / (fn + 0.5) + 1 / (fp + 0.5))
    ornote <- "Empirical Logit"
  } else {
    or_raw <- tp * tn / (fp * fn)
    sqrtsdlnor <- sqrt(1 / tp + 1 / tn + 1 / fn + 1 / fp)
    ornote <- " "
  }

  lnor <- log(or_raw)
  lnor_l <- lnor - stats::qnorm(.975) * sqrtsdlnor
  lnor_u <- lnor + stats::qnorm(.975) * sqrtsdlnor
  or <- conf_int(or_raw, exp(lnor_l), exp(lnor_u))

  dx_measure_df(
    measure = "Odds Ratio",
    estimate = or,
    ci_type = "Large sample",
    notes = ornote,
    estimate_raw = or_raw,
    lci_raw = exp(lnor_l),
    uci_raw = exp(lnor_u)
  )
}

dx_sensitivity <- function(tp, dispos, citype, threshold) {
  sensitivity_raw <- tp / dispos
  sens_frac <- paste0(scales::comma(tp), "/", scales::comma(dispos))
  senres <- binom::binom.confint(tp, dispos,
    conf.level = 0.95,
    methods = citype
  )
  senres <- dplyr::rename(senres,
    y1_check = x, n1_check = n,
    p1_check = mean, sens_lci = lower, sens_uci = upper
  )
  sensitivity <- conf_int(sensitivity_raw, senres$sens_lci,
    senres$sens_uci,
    percent = TRUE
  )
  dx_measure_df(
    measure = "Sensitivity",
    estimate = sensitivity,
    fraction = sens_frac,
    ci_type = citype,
    notes = paste0(">=", threshold),
    estimate_raw = sensitivity_raw,
    lci_raw = senres$sens_lci,
    uci_raw = senres$sens_uci
  )
}

dx_specificity <- function(tn, disneg, citype, threshold) {
  specificity_raw <- tn / disneg
  spec_frac <- paste0(scales::comma(tn), "/", scales::comma(disneg))
  specres <- binom::binom.confint(tn, disneg,
    conf.level = 0.95, methods = citype
  )
  specres <- dplyr::rename(specres,
    y2_check = x, n2_check = n,
    p2_check = mean, spec_lci = lower, spec_uci = upper
  )
  specificity <- conf_int(specificity_raw, specres$spec_lci,
    specres$spec_uci,
    percent = TRUE
  )
  dx_measure_df(
    measure = "Specificity",
    estimate = specificity,
    fraction = spec_frac,
    ci_type = citype,
    notes = paste0("<", threshold),
    estimate_raw = specificity_raw,
    lci_raw = specres$spec_lci,
    uci_raw = specres$spec_uci
  )
}

dx_accuracy <- function(correct, n, citype) {
  accuracy_raw <- correct / n
  acc_frac <- paste0(scales::comma(correct), "/", scales::comma(n))
  accres <- binom::binom.confint(correct, n,
    conf.level = 0.95,
    methods = citype
  )
  accres <- dplyr::rename(accres,
    y3_check = x, n3_check = n,
    p3_check = mean, acc_lci = lower, acc_uci = upper
  )
  accuracy <- conf_int(accuracy_raw, accres$acc_lci,
    accres$acc_uci,
    percent = TRUE
  )

  dx_measure_df(
    measure = "Accuracy",
    estimate = accuracy,
    fraction = acc_frac,
    ci_type = citype,
    estimate_raw = accuracy_raw,
    lci_raw = accres$acc_lci,
    uci_raw = accres$acc_uci
  )
}

dx_ppv <- function(tp, testpos, citype) {
  ppv_raw <- tp / testpos
  ppv_frac <- paste0(scales::comma(tp), "/", scales::comma(testpos))
  ppvres <- binom::binom.confint(tp, testpos,
    conf.level = 0.95,
    methods = citype
  )
  ppvres <- dplyr::rename(ppvres,
    y4_check = x, n4_check = n,
    p4_check = mean, ppv_lci = lower, ppv_uci = upper
  )
  ppv <- conf_int(ppv_raw, ppvres$ppv_lci, ppvres$ppv_uci, percent = TRUE)

  dx_measure_df(
    measure = "Positive Predictive Value",
    estimate = ppv,
    fraction = ppv_frac,
    ci_type = citype,
    estimate_raw = ppv_raw,
    lci_raw = ppvres$ppv_lci,
    uci_raw = ppvres$ppv_uci
  )
}

dx_npv <- function(tn, testneg, citype) {
  npv_raw <- tn / testneg
  npv_frac <- paste0(scales::comma(tn), "/", scales::comma(testneg))
  npvres <- binom::binom.confint(tn, testneg,
    conf.level = 0.95,
    methods = citype
  )
  npvres <- dplyr::rename(npvres,
    y5_check = x, n5_check = n, p5_check = mean,
    npv_lci = lower, npv_uci = upper
  )
  npv <- conf_int(npv_raw, npvres$npv_lci, npvres$npv_uci, percent = TRUE)

  dx_measure_df(
    measure = "Negative Predictive Value",
    estimate = npv,
    fraction = npv_frac,
    ci_type = citype,
    estimate_raw = npv_raw,
    lci_raw = npvres$npv_lci,
    uci_raw = npvres$npv_uci
  )
}

dx_precision <- function(tp, fp) {
  tp / (tp + fp)
}

dx_recall <- function(tp, fn) {
  tp / (tp + fn)
}

dx_f1 <- function(predprob, truth, precision, recall, bootreps, doboot) {
  f1_raw <- 2 * (precision * recall) / (precision + recall)

  # Make sure boot is available if using
  if (doboot & !requireNamespace("boot", quietly = TRUE)) {
    warning("boot package not installed.  Skipping bootsrapped CI. ")
    doboot <- FALSE
  }

  #### Generate bootstrap estimate of F1 confidence interval
  if (doboot) {
    set.seed(bootseed)

    pred <- ifelse(predprob >= threshold, 1, 0)
    tempmat <- data.frame(pred, truth)

    boot_res <- boot::boot(data = tempmat, statistic = f1boot, R = bootreps)
    # get 95% confidence interval
    boot_ci_res <- boot::boot.ci(boot_res, type = "basic")
    dim(boot_ci_res$basic)
    f1_lci <- boot_ci_res$basic[4]
    f1_uci <- boot_ci_res$basic[5]

    f1 <- conf_int(f1_raw, f1_lci, f1_uci, percent = T)

    f1 <- cbind(f1, f1_lci, f1_uci)
  } else {
    f1 <- formatC(round(f1_raw * 100, 1), format = "f", digits = 1)
  }

  dx_measure_df(
    measure = "F1 Score",
    estimate = f1,
    estimate_raw = f1_raw,
    ci_type = if (doboot) "Bootstrapped" else "",
    notes = if (doboot) paste0("B=", bootreps, "") else ""
  )
}


### Function to bootstrap the F1 statistic
f1boot <- function(data, indices, poslabel = 1) {
  ## data coming in needs to be a two column data frame:
  # Column 1 is the truth,
  # column 2 is the prediction. both are the binary classifications
  d <- data[indices, ] # allows boot to select sample

  truth <- d[, 1]
  pred <- d[, 2]


  tempdf <- data.frame(pred, truth)
  tempdf$testresult <- ifelse(tempdf$pred == poslabel, 1, 0)
  tempdf$trueresult <- ifelse(tempdf$truth == poslabel, 1, 0)

  neg <- dplyr::filter(tempdf, trueresult == 0)
  pos <- dplyr::filter(tempdf, trueresult == 1)

  ## now count the tps and fns
  senscount <- pos %>%
    dplyr::group_by(testresult) %>%
    dplyr::summarize(counts = n())
  speccount <- neg %>%
    dplyr::group_by(testresult) %>%
    dplyr::summarize(counts = n())

  tp <- dplyr::ungroup(senscount) %>%
    dplyr::filter(testresult == 1) %>%
    dplyr::select(counts) %>%
    as.vector()
  fn <- dplyr::ungroup(senscount) %>%
    dplyr::filter(testresult == 0) %>%
    dplyr::select(counts) %>%
    as.vector()
  tn <- dplyr::ungroup(speccount) %>%
    dplyr::filter(testresult == 0) %>%
    dplyr::select(counts) %>%
    as.vector()
  fp <- dplyr::ungroup(speccount) %>%
    dplyr::filter(testresult == 1) %>%
    dplyr::select(counts) %>%
    as.vector()
  perfdf <- data.frame(tp, fn, tn, fp)
  names(perfdf) <- c("tp", "fn", "tn", "fp")
  precision <- perfdf$tp / (perfdf$tp + perfdf$fp)
  recall <- perfdf$tp / (perfdf$tp + perfdf$fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(f1)
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


dx_lrtneg <- function(tp, tn, fp, fn) {
  ## account for zero cells if present
  if (tp != 0 & tn != 0 & fp != 0 & fn != 0) {
    citype <- "Large sample"
  } else {
    tp <- tp + .5
    fp <- fp + .5
    tn <- tn + .5
    fn <- fn + .5
    citype <- "Large sample plus 0.5 for all cells"
  }


  ## Estimate the LRT-
  lrtneg <- (fn / (tp + fn)) / ( tn / (fp + tn))

  ## Estimate the CI
  zcritical <- stats::qnorm(.975)

  ## Derived based on the delta method and log LRT
  lrtneg_sd <- sqrt( 1/fn - 1/(tp + fn) + 1/tn - 1/(fp + tn) )

  lrtneg_ci_l <- lrtneg * exp( -zcritical * lrtneg_sd)
  lrtneg_ci_u <- lrtneg * exp( zcritical * lrtneg_sd)


  ## Save the estimates
  lrtneg_est <- conf_int(lrtneg, lrtneg_ci_l,lrtneg_ci_u, accuracy = .01)
  # print(lrtneg_est)
  dx_measure_df(
    measure = "LRT-",
    estimate = lrtneg_est,
    fraction = "",
    ci_type = citype,
    estimate_raw = lrtneg,
    lci_raw = lrtneg_ci_l,
    uci_raw = lrtneg_ci_u
  )
}


dx_lrtpos <- function(tp, tn, fp, fn) {
  ## account for zero cells if present
  if (tp != 0 & tn != 0 & fp != 0 & fn != 0) {
    citype <- "Large sample"
  } else {
    tp <- tp + .5
    fp <- fp + .5
    tn <- tn + .5
    fn <- fn + .5
    citype <- "Large sample plus 0.5 for all cells"
  }


  ## Estimate the LRT+
  lrtpos <- (tp / (tp + fn)) / ( fp / (fp + tn))

  ## Estimate the CI
  zcritical <- stats::qnorm(.975)
  lrtpos_sd <- sqrt(1/tp - 1/(tp + fn) + 1/fp - 1/(fp + tn) )



  lrtpos_ci_l <- lrtpos * exp( -zcritical * lrtpos_sd)
  lrtpos_ci_u <- lrtpos * exp( zcritical * lrtpos_sd)



  ## Save the estimates
  lrtpos_est <- conf_int(lrtpos, lrtpos_ci_l,lrtpos_ci_u, accuracy = .01)
  # print(lrtpos_est)
  dx_measure_df(
    measure = "LRT+",
    estimate = lrtpos_est,
    fraction = "",
    ci_type = citype,
    estimate_raw = lrtpos,
    lci_raw = lrtpos_ci_l,
    uci_raw = lrtpos_ci_u
  )
}




dx_measure_df <- function(measure = "", estimate = "", fraction = "",
                          ci_type = "", notes = "", estimate_raw = NA,
                          lci_raw = NA, uci_raw = NA) {
  tmp <- data.frame(
    measure = measure,
    estimate = estimate,
    fraction = fraction,
    ci_type = ci_type,
    notes = notes,
    rawestime = estimate_raw,
    rawlci = lci_raw,
    rawuci = uci_raw,
    stringsAsFactors = FALSE
  )

  tmp %>% dplyr::mutate_if(is.factor, as.character)
}
