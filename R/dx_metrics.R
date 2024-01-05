#' Shared Parameters for Diagnostic Metrics
#'
#' @param cm A dx_cm object created by \code{\link{dx_cm}}.
#' @param detail Character specifying the level of detail in the output:
#'               "simple" for raw estimate, "full" for detailed estimate
#'               including 95\% confidence intervals.
#' @param ... Additional arguments to pass to metric_binomial function, such as
#'            `citype` for type of confidence interval method.
#' @param boot Logical specifying if confidence intervals should be generated
#'         via bootstrapping.  Note, this can be slow.
#' @param bootreps The number of bootstrap replications for calculating confidence intervals.
#' @param predprob Numeric vector of predicted probabilities associated with the positive class.
#' @param truth Numeric vector of true binary outcomes, typically 0 or 1, with the same length as `predprob`.
#' @param dx1 A `dx` object
#' @param dx2 A `dx` object
#' @return Depending on the `detail` parameter, returns a numeric value
#'         representing the calculated metric or a data frame/tibble with
#'         detailed diagnostics including confidence intervals and possibly other
#'         metrics relevant to understanding the metric.
#' @name metrics-params
#' @keywords internal
metricparams <- function() {}

#' Calculate Accuracy
#'
#' Calculates the proportion of correct predictions (True Positives + True Negatives)
#' over all cases from a confusion matrix object, providing a measure
#' of the classifier's overall correctness.
#'
#' @inheritParams metrics-params
#'
#' @details
#' \eqn{Accuracy = \frac{True Positives + True Negatives}{Total Cases}}{Accuracy = (TP + TN) / N}
#'
#' Accuracy is one of the most intuitive performance measures and it is simply a ratio
#' of correctly predicted observation to the total observations. It's a common starting
#' point for evaluating the performance of a classifier. However, it's not suitable for
#' unbalanced classes due to its tendency to be misleadingly high when the class of
#' interest is underrepresented. For detailed diagnostics, including confidence intervals,
#' specify detail = "full".
#'
#' @examples
#' cm <- dx_cm(
#'   dx_heart_failure$predicted,
#'   dx_heart_failure$predicted,
#'   threshold = 0.3, poslabel = 1
#' )
#' simple_accuracy <- dx_accuracy(cm, detail = "simple")
#' detailed_accuracy <- dx_accuracy(cm)
#' print(simple_accuracy)
#' print(detailed_accuracy)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @concept metrics
#' @export
dx_accuracy <- function(cm, detail = "full", ...) {
  metric_binomial(cm$correct, cm$n, name = "Accuracy", detail = detail, ...)
}

#' Calculate Positive Predictive Value (PPV, Precision)
#'
#' Calculates the proportion of true positives out of the total predicted positives
#' (true positives + false positives).  PPV is also known as precision.Note that
#' PPV can be influenced by the prevalence of the condition and should be used
#' alongside other metrics.
#'
#' @inheritParams metrics-params
#'
#' @details
#' PPV, also known as precision, is the ratio of true positives to the sum of true
#' and false positives. It reflects the classifier's ability to identify only
#' relevant instances. However, like accuracy, it may not be suitable for unbalanced
#' datasets. For detailed diagnostics, including confidence intervals, specify detail = "full".
#'
#' The formula for PPV is:
#' \deqn{PPV = \frac{True Positives}{True Positives + False Positives}}{PPV = TP / (TP + FP)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_ppv <- dx_ppv(cm, detail = "simple")
#' detailed_ppv <- dx_ppv(cm)
#' print(simple_ppv)
#' print(detailed_ppv)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @name ppv
#' @concept metrics
#' @aliases dx_precision
#' @export
dx_ppv <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tp, cm$testpos, name = "Positive Predictive Value", detail = detail, ...)
}

#' @rdname ppv
dx_precision <- dx_ppv

#' Calculate Negative Predictive Value (NPV)
#'
#' Calculates the proportion of true negatives out of the total predicted negatives
#' (true negatives + false negatives), known as the Negative Predictive Value (NPV).
#' This metric is a measure of the classifier's ability to identify negatives correctly.
#' Note that NPV, like other metrics, may not fully represent classifier performance
#' in unbalanced datasets and should be used alongside other metrics.
#'
#' @inheritParams metrics-params
#'
#' @details
#' NPV is the ratio of true negatives to the sum of true and false negatives. It is an
#' indicator of how well the classifier can identify negative instances. High NPV means
#' that the classifier is reliable in its negative classifications. However, it may be
#' influenced by the prevalence of the condition and is best used in conjunction with
#' other metrics like PPV, sensitivity, and specificity for a comprehensive evaluation.
#'
#' The formula for NPV is:
#' \deqn{NPV = \frac{True Negatives}{True Negatives + False Negatives}}{NPV = TN / (TN + FN)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_npv <- dx_npv(cm, detail = "simple")
#' detailed_npv <- dx_npv(cm)
#' print(simple_npv)
#' print(detailed_npv)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_npv <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tn, cm$testneg, name = "Negative Predictive Value", detail = detail, ...)
}

#' Calculate False Negative Rate (FNR)
#'
#' Calculates the False Negative Rate (FNR), which is the proportion of actual positives
#' that were incorrectly identified as negatives by the classifier. FNR is also known
#' as the miss rate and is a critical measure in evaluating the performance of a classifier,
#' especially in scenarios where failing to detect positives is costly.
#'
#' @inheritParams metrics-params
#'
#' @details
#' FNR is an important measure in situations where the cost of missing a positive classification
#' is high. It complements the True Positive Rate (sensitivity) and helps in understanding the
#' trade-offs between identifying positives and avoiding false alarms. A lower FNR is generally
#' desirable and indicates a more sensitive classifier.
#'
#' The formula for FNR is:
#' \deqn{FNR = \frac{False Negatives}{False Negatives + True Positives}}{FNR = FN / (FN + TP)}
#' @aliases dx_miss_rate
#' @name fnr
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_fnr <- dx_fnr(cm, detail = "simple")
#' detailed_fnr <- dx_fnr(cm)
#' print(simple_fnr)
#' print(detailed_fnr)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_fnr <- function(cm, detail = "full", ...) {
  metric_binomial(cm$fn, cm$dispos, name = "False Negative Rate", detail = detail, ...)
}

#' @rdname fnr
dx_miss_rate <- dx_fnr

#' Calculate False Positive Rate (FPR)
#'
#' Calculates the False Positive Rate (FPR), which is the proportion of actual negatives
#' that were incorrectly identified as positives by the classifier. FPR is also known
#' as the fall-out rate and is crucial in evaluating the specificity of a classifier.
#'
#' @inheritParams metrics-params
#'
#' @details
#' FPR is particularly important in contexts where false alarms are costly. It is
#' used alongside True Negative Rate (specificity) to understand the classifier's
#' ability to correctly identify negative instances. A lower FPR indicates a classifier
#' that is better at correctly identifying negatives and not alarming false positives.
#'
#' The formula for FPR is:
#' \deqn{FPR = \frac{False Positives}{False Positives + True Negatives}}{FPR = FP / (FP + TN)}
#' @aliases dx_fall_out
#' @name fpr
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_fpr <- dx_fpr(cm, detail = "simple")
#' detailed_fpr <- dx_fpr(cm)
#' print(simple_fpr)
#' print(detailed_fpr)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_fpr <- function(cm, detail = "full", ...) {
  metric_binomial(cm$fp, cm$disneg, name = "False Positive Rate", detail = detail, ...)
}

#' @rdname fpr
dx_fall_out <- dx_fpr

#' Calculate False Discovery Rate (FDR)
#'
#' Calculates the False Discovery Rate (FDR), which is the proportion of false positives
#' among all positive predictions. FDR is a critical measure in many classification contexts,
#' particularly where the cost of a false positive is high.
#'
#' @inheritParams metrics-params
#'
#' @details
#' FDR is an important measure when the consequences of false discoveries (false positives)
#' are significant. It helps in understanding the error rate among the positive predictions made
#' by the classifier. A lower FDR indicates a better precision of the classifier in identifying
#' only the true positives.
#'
#' The formula for FDR is:
#' \deqn{FDR = \frac{False Positives}{False Positives + True Positives}}{FDR = FP / (FP + TP)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_fdr <- dx_fdr(cm, detail = "simple")
#' detailed_fdr <- dx_fdr(cm)
#' print(simple_fdr)
#' print(detailed_fdr)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_fdr <- function(cm, detail = "full", ...) {
  metric_binomial(cm$fp, cm$testpos, name = "False Discovery Rate", detail = detail, ...)
}

#' Calculate Sensitivity (True Positive Rate, Recall)
#'
#' Calculates Sensitivity, also known as the True Positive Rate (TPR) or recall, which
#' is the proportion of actual positives that are correctly identified as such by the
#' classifier. Sensitivity is a key measure in evaluating the effectiveness of a classifier
#' in identifying positive instances.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Sensitivity or TPR is an important measure in scenarios where missing a positive
#' identification has serious consequences. It essentially measures the proportion of
#' actual positives that are correctly identified, giving insight into the ability of
#' the classifier to detect positive instances. A higher sensitivity indicates a better
#' performance in recognizing positive instances.
#'
#' The formula for Sensitivity is:
#' \deqn{Sensitivity = \frac{True Positives}{True Positives + False Negatives}}{Sensitivity = TP / (TP + FN)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_sensitivity <- dx_sensitivity(cm, detail = "simple")
#' detailed_sensitivity <- dx_sensitivity(cm)
#' print(simple_sensitivity)
#' print(detailed_sensitivity)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @name sensitivity
#' @aliases dx_recall, dx_tpr
#' @export
#' @concept metrics
dx_sensitivity <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tp, cm$dispos, name = "Sensitivity", detail = detail, ...)
}

#' @rdname sensitivity
dx_recall <- dx_sensitivity

#' @rdname sensitivity
dx_tpr <- dx_sensitivity

#' Calculate Specificity (True Negative Rate)
#'
#' Calculates Specificity, also known as the True Negative Rate (TNR), which is the
#' proportion of actual negatives that are correctly identified as such by the classifier.
#' Specificity is a key measure in evaluating the effectiveness of a classifier in
#' identifying negative instances.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Specificity or TNR measures how well the classifier can identify negative instances,
#' which is critical in situations where false positives carry a high cost. A higher
#' specificity indicates a better performance in recognizing negative instances and
#' avoiding false alarms.
#'
#' The formula for Specificity is:
#' \deqn{Specificity = \frac{True Negatives}{True Negatives + False Positives}}{Specificity = TN / (TN + FP)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_specificity <- dx_specificity(cm, detail = "simple")
#' detailed_specificity <- dx_specificity(cm)
#' print(simple_specificity)
#' print(detailed_specificity)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @name specificity
#' @aliases dx_tnr
#' @export
#' @concept metrics
dx_specificity <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tn, cm$disneg, name = "Specificity", detail = detail, ...)
}

#' @rdname specificity
dx_tnr <- dx_specificity



#' Calculate Balanced Accuracy
#'
#' Calculates Balanced Accuracy, which is the average of sensitivity (recall) and specificity.
#' This metric is particularly useful for imbalanced datasets as it accounts for both the
#' positive and negative classes equally and doesn't inherently favor the majority class.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Balanced Accuracy mitigates the issue of the regular accuracy metric favoring models
#' that predict the majority class in an imbalanced dataset. By taking the average of
#' sensitivity and specificity, it gives a better measure of the overall performance
#' especially when classes are imbalanced or when costs of different errors vary greatly.
#'
#' The formula for Balanced Accuracy is:
#' \deqn{Balanced Accuracy = \frac{Sensitivity + Specificity}{2}}{Balanced Accuracy = (Sensitivity + Specificity) / 2}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' simple_balanced_accuracy <- dx_balanced_accuracy(cm, detail = "simple")
#' detailed_balanced_accuracy <- dx_balanced_accuracy(cm)
#' print(simple_balanced_accuracy)
#' print(detailed_balanced_accuracy)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a 'dx_cm' object.
#' @seealso \code{\link{dx_sensitivity}}, \code{\link{dx_specificity}} for the components of balanced accuracy.
#' @export
#' @concept metrics
dx_balanced_accuracy <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(
    cm,
    calculate_balanced_accuracy,
    "Balanced Accuracy",
    detail,
    boot,
    bootreps
  )
}

calculate_balanced_accuracy <- function(cm) {
  sens <- dx_sensitivity(cm, "simple")
  spec <- dx_specificity(cm, "simple")
  (sens + spec) / 2
}

#' Calculate Prevalence
#'
#' Calculates Prevalence, which is the proportion of cases that are positive for
#' the condition of interest over the total number of cases. Prevalence provides
#' a measure of how widespread a condition is within the population at a given time.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Prevalence is a measure of the burden of a condition or disease in a population.
#' It is an important measure in epidemiology and health service planning as it helps
#' to understand the level of disease in a population at a given time. Unlike other
#' metrics that are based on the classifier's performance, prevalence is a measure of
#' the actual condition being tested.
#'
#' The formula for Prevalence is:
#' \deqn{Prevalence = \frac{Number of Current Cases (Positives)}{Total Number of Cases}}{Prevalence = Pos / N}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_prevalence <- dx_prevalence(cm, detail = "simple")
#' detailed_prevalence <- dx_prevalence(cm)
#' print(simple_prevalence)
#' print(detailed_prevalence)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_prevalence <- function(cm, detail = "full", ...) {
  metric_binomial(cm$dispos, cm$n, name = "Prevalence", detail = detail, ...)
}


#' Calculate Detection Prevalence
#'
#' Calculates Detection Prevalence, which is the proportion of cases that are predicted
#' positive by the classifier over the total number of cases. Detection Prevalence provides
#' a measure of how often the condition is identified by the model, regardless of its actual prevalence.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Detection Prevalence is a measure of the frequency with which a classifier predicts
#' a condition as positive. It is different from the actual condition prevalence in the population
#' and is influenced by the classifier's threshold and performance characteristics. High detection
#' prevalence could indicate a tendency of the model to predict more positive cases, which
#' might be useful or detrimental depending on the specific application and the cost of false
#' positives. It is important to compare Detection Prevalence with the actual condition
#' prevalence to assess the model's performance.
#'
#' The formula for Detection Prevalence is:
#' \deqn{Detection Prevalence = \frac{Number of Predicted Positives}{Total Number of Cases}}{Detection Prevalence = TestPos / N}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_detection_prevalence <- dx_detection_prevalence(cm, detail = "simple")
#' detailed_detection_prevalence <- dx_detection_prevalence(cm)
#' print(simple_detection_prevalence)
#' print(detailed_detection_prevalence)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_detection_prevalence <- function(cm, detail = "full", ...) {
  metric_binomial(cm$testpos, cm$n, name = "Detection Prevalence", detail = detail, ...)
}



#' Calculate Cohen's Kappa
#'
#' Calculates Cohen's Kappa, a statistical measure of inter-rater reliability
#' or agreement for qualitative (categorical) items. It is generally thought
#' to be a more robust measure than simple percent agreement calculation since
#' Kappa takes into account the agreement occurring by chance.
#'
#' @inheritParams metrics-params
#'
#' @return If `detail` is "simple", returns a single numeric value of Cohen's Kappa.
#' If `detail` is "full", returns a list or data frame that includes Cohen's Kappa,
#' its standard error, 95% confidence intervals, and interpretative notes.
#'
#' @details
#' Cohen's Kappa is used to measure the agreement between two raters who each classify
#' items into mutually exclusive categories. The formula for Cohen's Kappa is:
#' \deqn{kappa = (po - pe) / (1 - pe)}
#' where \eqn{po} is the relative observed agreement among raters, and \eqn{pe} is the
#' hypothetical probability of chance agreement. The value of kappa can range from -1
#' (total disagreement) to 1 (perfect agreement), with 0 indicating the amount of
#' agreement that can be expected from random chance.
#'
#' Interpretation of Cohen's Kappa varies, but generally, a higher value indicates
#' better agreement. Typical benchmarks for interpreting Cohen's Kappa are:
#' - < 0: Less than chance agreement
#' - 0.01-0.20: Slight agreement
#' - 0.21-0.40: Fair agreement
#' - 0.41-0.60: Moderate agreement
#' - 0.61-0.80: Substantial agreement
#' - 0.81-0.99: Almost perfect agreement
#'
#' @examples
#' # Assuming you have a confusion matrix cm with appropriate structure
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' kappa_simple <- dx_cohens_kappa(cm, detail = "simple")
#' kappa_full <- dx_cohens_kappa(cm)
#' print(kappa_simple)
#' print(kappa_full)
#'
#' @export
#' @concept metrics
dx_cohens_kappa <- function(cm, detail = "full") {
  # Calculate observed agreement (po)
  po <- (cm$tp + cm$tn) / cm$n

  # Calculate expected agreement (pe)
  pe <- ((cm$tp + cm$fp) * (cm$tp + cm$fn) + (cm$fn + cm$tn) * (cm$fp + cm$tn)) / (cm$n^2)

  # Calculate Cohen's Kappa
  kappa <- (po - pe) / (1 - pe)

  if (detail == "simple") {
    return(kappa)
  } else if (detail == "full") {

    # Calculate variance of kappa (standard method)
    var_kappa <- (po * (1 - po)) / (cm$n * (1 - pe)^2)

    # Calculate standard error of kappa
    se_kappa <- sqrt(var_kappa)

    # Confidence interval using normal approximation, typically Z = 1.96 for 95%
    z <- 1.96
    ci_lower <- kappa - z * se_kappa
    ci_upper <- kappa + z * se_kappa

    if (kappa < 0) {
      note <- "Less than chance agreement"
    } else if (kappa < 0.2) {
      note <- "Slight Agreement"
    } else if (kappa < .4) {
      note <- "Fair Agreement"
    } else if (kappa < .6) {
      note <- "Moderate Agreement"
    } else if (kappa < .8) {
      note <- "Substantial Agreement"
    } else if (kappa < 1) {
      note <- "Almost Perfect Agreement"
    }


    res <- measure_df(
      measure = "Cohen's Kappa",
      estimate = conf_int(kappa, ci_lower, ci_upper, accuracy = .01),
      estimate_raw = kappa,
      ci_type = "Standard Error+Normal Approximation",
      notes = note,
      lci_raw = ci_lower,
      uci_raw = ci_upper
    )

    return(res)
  }


}

#' Calculate Matthews Correlation Coefficient (MCC)
#'
#' Calculates the Matthews Correlation Coefficient (MCC), a measure of the quality
#' of binary classifications. It returns a value between -1 and +1 where +1 indicates
#' perfect prediction, 0 no better than random prediction, and -1 indicates total
#' disagreement between prediction and observation. The function can also return a
#' confidence interval for the MCC value using bootstrapping if detail is set to "full".
#'
#' @inheritParams metrics-params
#'
#' @return If `detail` is "simple", returns a single numeric value of MCC.
#' If `detail` is "full", returns a data frame that includes MCC, its
#' bootstrapped confidence interval, and other key details
#'
#' @details
#' The Matthews Correlation Coefficient is used in machine learning as a measure of the
#' quality of binary (two-class) classifications. It takes into account true and false
#' positives and negatives and is generally regarded as a balanced measure which can be
#' used even if the classes are of very different sizes.
#' The formula for MCC is:
#' \deqn{MCC = \frac{(TP \times TN) - (FP \times FN)}{\sqrt{(TP + FP)(TP + FN)(TN + FP)(TN + FN)}}}
#' where TP, TN, FP, and FN represent the counts of true positives, true negatives,
#' false positives, and false negatives, respectively.
#'
#' For "full" details, bootstrap methods are used to estimate the confidence interval
#' for the MCC value, providing a more robust understanding of its stability.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' mcc_simple <- dx_mcc(cm, detail = "simple")
#' mcc_full <- dx_mcc(cm)
#' print(mcc_simple)
#' print(mcc_full)
#'
#' @export
#' @concept metrics
dx_mcc <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_mcc, "Matthews Correlation Coefficient", detail, boot, bootreps)
}

calculate_mcc <- function(cm) {
  # Extract counts from the confusion matrix
  tp <- as.numeric(cm$tp)
  tn <- as.numeric(cm$tn)
  fp <- as.numeric(cm$fp)
  fn <- as.numeric(cm$fn)

  # Calculate elements needed for MCC
  numerator <- (tp * tn) - (fp * fn)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

  # Handle cases where the denominator is 0 to avoid division by zero
  if (denominator == 0) return(0)

  # Calculate and return MCC
  numerator / denominator
}


#' Calculate a Binomial Metric
#'
#' This internal function calculates a binomial metric and its confidence interval
#' based on the provided details and method type. It can return a simple estimate
#' or a more detailed data frame with confidence intervals.
#'
#' @param x The number of successes.
#' @param n The number of trials.
#' @param name The name of the measure being calculated.
#' @param detail The level of detail for the output ('simple' or 'full').
#' @param citype The type of confidence interval method to use.
#' @param ... Additional arguments for the confidence interval calculation.
#' @return Depending on the `detail` parameter, returns a single numeric value
#'         or a data frame with the metric and confidence intervals.
#' @noRd
metric_binomial <- function(x, n, name, detail = "full", citype = "exact", ...) {
  # if (check_zero_denominator(n, name)) return(NA)

  if (detail == "simple") {
    return(x / n)
  } else if (detail == "full") {
    return(ci_binomial(x, n, measure = name, citype, ...))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}

#' Calculate Confidence Interval for Binomial Metric
#'
#' This internal function calculates the confidence interval for a binomial metric
#' using the specified method. It returns a data frame with the formatted estimate
#' and raw confidence interval values.
#'
#' @param x The number of successes.
#' @param n The number of trials.
#' @param citype The type of confidence interval method to use ('exact', 'wilson', etc.).
#' @param ... Additional arguments for the confidence interval calculation.
#' @return A data frame with the calculated binomial metric and confidence intervals.
#' @noRd
ci_binomial <- function(x, n, citype = "exact", ...) {

  res <- binom::binom.confint(x, n, conf.level = 0.95, methods = citype)

  formatted_estimate <- conf_int(
    res$mean,
    res$lower,
    res$upper,
    percent = TRUE
  )

  measure_df(
    estimate = formatted_estimate,
    estimate_raw = res$mean,
    lci_raw = res$lower,
    uci_raw = res$upper,
    fraction = paste0(comma(x), "/", comma(n)),
    ci_type = paste0("Binomial: ", citype),
    ...
  )

}


#' Calculate Odds Ratio
#'
#' Calculates the Odds Ratio (OR) from a confusion matrix object. OR is a measure of
#' association between exposure and an outcome. It represents the odds that an outcome
#' will occur given a particular exposure, compared to the odds of the outcome occurring
#' in the absence of that exposure.
#'
#' @inheritParams metrics-params
#'
#' @details
#' The odds ratio is calculated as (TP * TN) / (FP * FN). It is used in case-control
#' studies to estimate the strength of the association between exposure and outcome.
#' Note that a value of 1 indicates no association, greater than 1 indicates increased
#' odds of the outcome with exposure, and less than 1 indicates decreased odds.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_or <- dx_odds_ratio(cm, detail = "simple")
#' detailed_or <- dx_odds_ratio(cm)
#' print(simple_or)
#' print(detailed_or)
#' @export
#' @concept metrics
dx_odds_ratio <- function(cm, detail = "full", ...) {
  ratio <- function(tp, tn, fp, fn) (tp * tn) / (fp * fn)
  sd <- function(tp, tn, fp, fn) sqrt(sum(1 / c(tp, tn, fn, fp)))
  metric_ratio(cm, ratio, sd, detail, name = "Odds Ratio", ...)
}

#' Calculate Negative Likelihood Ratio
#'
#' Calculates the Negative Likelihood Ratio (LR-) from a confusion matrix object.
#' LR- compares the probability of a negative test result among patients with the
#' disease to the probability of a negative test result among patients without the disease.
#'
#' @inheritParams metrics-params
#'
#' @details
#' The negative likelihood ratio is calculated as (FN / (TP + FN)) / (TN / (FP + TN)).
#' It is used to assess the diagnostic usefulness of a test. A LR- closer to 0 indicates
#' a good diagnostic test that can confidently rule out the disease when the test is negative.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_lrn <- dx_lrt_neg(cm, detail = "simple")
#' detailed_lrn <- dx_lrt_neg(cm)
#' print(simple_lrn)
#' print(detailed_lrn)
#' @export
#' @concept metrics
dx_lrt_neg <- function(cm, detail = "full", ...) {
  ratio <- function(tp, tn, fp, fn) (fn / (tp + fn)) / (tn / (fp + tn))
  sd <- function(tp, tn, fp, fn) sqrt(1/fn - 1/(tp + fn) + 1/tn - 1/(fp + tn))
  metric_ratio(cm, ratio, sd, detail, name = "LRT-", ...)
}

#' Calculate Positive Likelihood Ratio
#'
#' Calculates the Positive Likelihood Ratio (LR+) from a confusion matrix object.
#' LR+ compares the probability of a positive test result among patients with the
#' disease to the probability of a positive test result among patients without the disease.
#'
#' @inheritParams metrics-params
#'
#' @details
#' The positive likelihood ratio is calculated as (TP / (TP + FN)) / (FP / (FP + TN)).
#' It is used to assess the diagnostic usefulness of a test. A LR+ much greater than 1
#' indicates a good diagnostic test that can confidently confirm the disease when the
#' test is positive.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_lrp <- dx_lrt_pos(cm, detail = "simple")
#' detailed_lrp <- dx_lrt_pos(cm)
#' print(simple_lrp)
#' print(detailed_lrp)
#' @export
#' @concept metrics
dx_lrt_pos <- function(cm, detail = "full", ...) {
  ratio <- function(tp, tn, fp, fn) (tp / (tp + fn)) / (fp / (fp + tn))
  sd <- function(tp, tn, fp, fn) sqrt(1/tp - 1/(tp + fn) + 1/fp - 1/(fp + tn))
  metric_ratio(cm, ratio, sd, detail, name = "LRT+", ...)
}

#' Generalized Ratio Metric Calculation
#'
#' This internal function calculates ratio metrics such as odds ratio or likelihood ratio
#' from a given confusion matrix and specific functions to calculate the ratio and its
#' standard deviation. It supports a simple return of the calculated ratio or a full return
#' with confidence intervals.
#'
#' @param cm A confusion matrix object with elements 'tp', 'tn', 'fp', 'fn'.
#' @param dx_ratio_func A function to calculate the desired ratio metric.
#' @param dx_sd_func A function to calculate the standard deviation of the log-transformed ratio.
#' @param detail Character string specifying the level of detail in the output: "simple"
#'        for just the ratio value, "full" for the ratio value along with confidence intervals.
#' @param ... Additional arguments to pass to the confidence interval calculation.
#' @return Depending on the `detail` parameter, returns a single numeric value or a
#'         data frame with the metric and confidence intervals.
#' @noRd
metric_ratio <- function(cm, dx_ratio_func, dx_sd_func, detail = "full", ...) {
  # Extract counts from confusion matrix
  tp <- cm$tp
  tn <- cm$tn
  fp <- cm$fp
  fn <- cm$fn

  # Apply continuity correction if any cell is zero
  continuity_correction <- if (tp == 0 | tn == 0 | fp == 0 | fn == 0) 0.5 else 0

  # Adjust values if necessary
  tp <- tp + continuity_correction
  tn <- tn + continuity_correction
  fp <- fp + continuity_correction
  fn <- fn + continuity_correction

  # Calculate the ratio and its standard deviation using passed functions
  ratio <- dx_ratio_func(tp, tn, fp, fn)
  ratio_sd <- dx_sd_func(tp, tn, fp, fn)

  if (detail == "simple") {
    return(ratio)
  } else if (detail == "full") {
    return(ci_ratio(tp, tn, fp, fn, ratio, ratio_sd, continuity_correction=continuity_correction, ...))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}

#' Confidence Interval Calculation for Ratio Metrics
#'
#' This internal function calculates the confidence intervals for ratio metrics such as
#' odds ratio or likelihood ratio using the log transformation method. It is typically used
#' in conjunction with a generalized ratio calculation function.
#'
#' @param tp True positives count.
#' @param tn True negatives count.
#' @param fp False positives count.
#' @param fn False negatives count.
#' @param ratio The calculated ratio value.
#' @param ratio_sd The standard deviation of the log-transformed ratio.
#' @param name The name of the measure for which confidence intervals are being calculated.
#' @param continuity_correction The value of continuity correction applied if any count was zero.
#' @param ... Additional arguments for the confidence interval calculation.
#' @return A data frame with the formatted estimate, raw ratio, confidence intervals, and notes.
#' @noRd
ci_ratio <- function(tp, tn, fp, fn, ratio, ratio_sd, name, continuity_correction, ...) {
  # Calculate confidence interval for log-transformed Ratio
  z_value <- 1.95996398454005  # 95% CI
  ratio_log <- log(ratio)
  ratio_ci_l <- exp(ratio_log - z_value * ratio_sd)
  ratio_ci_u <- exp(ratio_log + z_value * ratio_sd)

  formatted_estimate <- conf_int(
    ratio,
    ratio_ci_l,
    ratio_ci_u,
    accuracy = .01
  )

  return(measure_df(
    measure = name,
    estimate = formatted_estimate,
    estimate_raw = ratio,
    lci_raw = ratio_ci_l,
    uci_raw = ratio_ci_u,
    ci_type = if (continuity_correction > 0) "Adjusted for zero cells" else "Large sample",
    ...
  ))
}

#' Calculate Area Under the Precision-Recall Curve (AUC-PR)
#'
#' This function calculates the Area Under the Curve (AUC) for the Precision-Recall curve
#' using the trapezoidal rule. It ensures proper alignment of precision and recall values
#' by adding a starting point at recall=0 with the first observed precision and an ending
#' point at recall=1 with the last observed precision.
#'
#' @param precision Numeric vector of precision values corresponding to different thresholds.
#' @param recall Numeric vector of recall values corresponding to different thresholds.
#' @param detail Character string specifying the level of detail in the output: "simple"
#'        for just the AUC value, "full" for the AUC value along with confidence intervals.
#' @return Depending on the `detail` parameter, returns a single numeric value of AUC or
#'         a data frame with the AUC and its confidence intervals.
#' @details The function prepares the precision and recall vectors by ensuring they are ordered
#'          by increasing recall values. It then calculates the AUC using the trapezoidal rule,
#'          which is the sum of areas of trapezoids formed between each consecutive pair of points.
#'          The first and last points are added to cover the entire recall range from 0 to 1.
#' @examples
#' # Assuming pr_data is your dataframe with precision and recall columns
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' auc_pr <- dx_auc_pr(dx_obj$thresholds$precision, dx_obj$thresholds$sensitivity)
#' print(auc_pr)
#' @export
#' @concept metrics
dx_auc_pr <- function(precision, recall, detail = "full") {
  # Remove any NA values that could cause issues in the calculation
  valid_indices <- !is.na(precision) & !is.na(recall)
  precision <- precision[valid_indices]
  recall <- recall[valid_indices]

  # Ensure the vectors are ordered by recall
  ord <- order(recall, decreasing = FALSE)
  precision <- precision[ord]
  recall <- recall[ord]

  # Add a starting point for recall at 0 with the first precision
  recall <- c(0, recall)
  precision <- c(precision[1], precision)

  # Add an ending point for recall at 1 with the last precision
  recall <- c(recall, 1)
  precision <- c(precision, precision[length(precision)])

  # Calculate differences in recall (x-axis)
  delta_recall <- diff(recall)

  # Calculate the average precision for each segment (y-axis)
  avg_precision <- (precision[-length(precision)] + precision[-1]) / 2

  # Calculate the area under the curve (AUC)
  auc <- sum(delta_recall * avg_precision)

  if (detail == "simple") {
    return(auc)
  } else {
    res <- measure_df(
      measure = "AUC PR",
      estimate = as.character(round(auc, 2)),
      estimate_raw = auc,
    )
    return(res)
  }


}

#' Calculate Area Under the ROC Curve (AUC)
#'
#' Calculates the Area Under the Receiver Operating Characteristic (ROC) Curve from
#' prediction probabilities and true binary outcomes. AUC is a measure of the ability
#' of a classifier to distinguish between classes and is used as a summary of the ROC curve.
#'
#' @param truth Vector of true binary class outcomes (0 and 1).
#' @param predprob Vector of prediction probabilities corresponding to the true outcomes.
#' @param detail Character string specifying the level of detail in the output: "simple"
#'        for just the AUC value, "full" for the AUC value along with confidence intervals.
#' @return Depending on the `detail` parameter, returns a single numeric value of AUC or
#'         a data frame with the AUC and its confidence intervals.
#' @examples
#' # Assuming you have a vector of true class labels and predicted probabilities
#' true_classes <- c(1, 0, 1, 1, 0, 0, 1)
#' predicted_probs <- c(0.9, 0.1, 0.8, 0.75, 0.33, 0.25, 0.67)
#' simple_auc <- dx_auc(true_classes, predicted_probs, detail = "simple")
#' detailed_auc <- dx_auc(true_classes, predicted_probs)
#' print(simple_auc)
#' print(detailed_auc)
#' @export
#' @concept metrics
dx_auc <- function(truth, predprob, detail = "full") {
  rocest <- pROC::roc(truth, predprob, ci = T, quiet = TRUE)
  aucest <- pROC::auc(rocest)
  auctext <- as.character(pROC::ci(aucest))
  auc_raw <- as.numeric(auctext[2])

  if (detail == "simple") {
    return(auc_raw)
  } else if (detail == "full") {
    auc_lci <- as.numeric(auctext[1])
    auc_uci <- as.numeric(auctext[3])
    auc <- conf_int(auc_raw, auc_lci, auc_uci, percent = F, accuracy = .001)

    return(measure_df(
      measure = "AUC ROC",
      estimate = auc,
      ci_type = "DeLong",
      estimate_raw = auc_raw,
      lci_raw = auc_lci,
      uci_raw = auc_uci
    ))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}


#' Calculate F-beta Score
#'
#' This internal function calculates the F-beta score given a confusion matrix
#' and a specific beta value. The F-beta score is a generalized form of the F1
#' score that weighs precision and recall.
#'
#' @param beta The beta value determining the weight of precision in the F-score.
#' @return The calculated F-beta score.
#' @noRd
calculate_fbeta <- function(cm, beta = 1) {
  tp <- cm$tp
  fp <- cm$fp
  fn <- cm$fn

  # Calculate precision and recall
  precision <- if(tp + fp == 0) 0 else tp / (tp + fp)
  recall <- if(tp + fn == 0) 0 else tp / (tp + fn)

  # Calculate F-beta score
  beta_sq <- beta^2
  if (precision + recall == 0) {
    0  # Return 0 to handle division by zero
  } else {
    (1 + beta_sq) * (precision * recall) / (beta_sq * precision + recall)
  }
}

#' Calculate F-beta Score with Confidence Intervals
#'
#' Calculates the F-beta score from a confusion matrix object with an option
#' to include bootstrapped confidence intervals. The F-beta score is a
#' generalization of the F1 score, allowing different importance to precision
#' and recall via the beta parameter.
#'
#' @inheritParams metrics-params
#' @param beta The beta value determining the weight of precision in the F-score.
#' @return Depending on the `detail` parameter, returns a single numeric value of
#'         F-beta or a data frame with the F-beta and its confidence intervals.
#' @seealso \code{\link{dx_f1}}, \code{\link{dx_f2}} for specific F-beta scores.
#' @export
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_f <- dx_fbeta(cm, beta = .5, detail = "simple")
#' detailed_f <- dx_fbeta(cm, beta = .5)
#' print(simple_f)
#' print(detailed_f)
#' @concept metrics
dx_fbeta <- function(cm, beta = 1, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_fbeta, paste0("F", beta, " Score"), detail, boot, bootreps, beta = beta)
}


#' Calculate F1 Score with Confidence Intervals
#'
#' Calculates the F1 score from a confusion matrix object with an option to
#' include bootstrapped confidence intervals. The F1 score is the harmonic
#' mean of precision and recall.
#'
#' @inheritParams dx_fbeta
#' @return Depending on the `detail` parameter, returns a single numeric value of
#'         F1 or a data frame with the F1 and its confidence intervals.
#' @seealso \code{\link{dx_fbeta}}, \code{\link{dx_f2}} for other F-beta scores.
#' @export
#' @concept metrics
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_f <- dx_f1(cm, detail = "simple")
#' detailed_f <- dx_f1(cm)
#' print(simple_f)
#' print(detailed_f)
dx_f1 <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  dx_fbeta(cm, beta = 1, detail, boot, bootreps)
}


#' Calculate F2 Score with Confidence Intervals
#'
#' Calculates the F2 score from a confusion matrix object with an option to
#' include bootstrapped confidence intervals. The F2 score weights recall
#' higher than precision.
#'
#' @inheritParams dx_fbeta
#' @return Depending on the `detail` parameter, returns a single numeric value of
#'         F2 or a data frame with the F2 and its confidence intervals.
#' @seealso \code{\link{dx_fbeta}}, \code{\link{dx_f1}} for other F-beta scores.
#' @export
#' @concept metrics
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold =
#'             0.5, poslabel = 1)
#' simple_f <- dx_f2(cm, detail = "simple")
#' detailed_f <- dx_f2(cm)
#' print(simple_f)
#' print(detailed_f)
dx_f2 <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  dx_fbeta(cm, beta = 2, detail, boot, bootreps)
}

#' Calculate Informedness
#'
#' Calculates Informedness for the provided confusion matrix. Informedness is a combined measure
#' of Sensitivity (True Positive Rate) and Specificity (True Negative Rate). It reflects the
#' probability that a classifier is informed about the true class, ranging from -1 to 1.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Informedness is defined as \code{Informedness = Sensitivity + Specificity - 1}. It is the sum of the true positive rate
#' and the true negative rate minus one. It's a useful measure when you want to consider both
#' the sensitivity and specificity of a test. A higher informedness indicates better overall performance
#' of the classifier in distinguishing between the classes.
#'
#' The formula for Informedness is:
#' \deqn{Informedness = Sensitivity + Specificity - 1}{Informedness = Sensitivity + Specificity - 1}
#' @name informedness
#' @aliases dx_youden_j
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' simple_informedness <- dx_informedness(cm, detail = "simple")
#' detailed_informedness <- dx_informedness(cm)
#' print(simple_informedness)
#' print(detailed_informedness)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a 'dx_cm' object.
#' @seealso \code{\link{dx_sensitivity}}, \code{\link{dx_specificity}} for components of informedness.
#' @export
#' @concept metrics
dx_informedness <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_informedness, "Informedness", detail, boot, bootreps)
}

calculate_informedness <- function(cm) {
  # Extract sensitivity and specificity from the confusion matrix
  sens <- dx_sensitivity(cm, "simple")
  spec <- dx_specificity(cm, "simple")

  # Calculate Youden's J Index
  sens + spec - 1
}

#' @rdname informedness
dx_youden_j <- dx_informedness


#' Calculate Markedness
#'
#' Calculates Markedness for the provided confusion matrix. Markedness is a combined measure
#' of PPV (Positive Predictive Value) and NPV (Negative Predictive Value). It reflects the
#' effectiveness of a classifier in marking class labels correctly, ranging from -1 to 1.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Markedness is defined as \code{Markedness = PPV + NPV - 1}. It is the sum of the proportions
#' of predicted positives that are true positives (PPV) and the proportion of predicted negatives
#' that are true negatives (NPV) minus one. It's a useful measure when you want to consider both
#' the positive and negative predictive values of a test. A higher markedness indicates better performance.
#'
#' The formula for Markedness is:
#' \deqn{Markedness = PPV + NPV - 1}{Markedness = PPV + NPV - 1}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' simple_markedness <- dx_markedness(cm, detail = "simple")
#' detailed_markedness <- dx_markedness(cm)
#' print(simple_markedness)
#' print(detailed_markedness)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a 'dx_cm' object.
#' @seealso \code{\link{dx_ppv}}, \code{\link{dx_npv}} for components of markedness.
#' @export
#' @concept metrics
dx_markedness <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_markedness, "Markedness", detail, boot, bootreps)
}


calculate_markedness <- function(cm) {
  # Extract PPV and NPV from the confusion matrix
  ppv <- dx_ppv(cm, "simple")
  npv <- dx_npv(cm, "simple")

  # Calculate Markedness
  ppv + npv - 1
}


#' Calculate G-mean
#'
#' Calculates the Geometric Mean (G-mean) for the provided confusion matrix.
#' G-mean is a measure of a model's performance that considers both the sensitivity
#' (True Positive Rate) and specificity (True Negative Rate), especially useful in
#' imbalanced datasets.
#'
#' @inheritParams metrics-params
#'
#' @details
#' G-mean is the geometric mean of sensitivity and specificity. It tries to maximize
#' the accuracy on each of the two classes while keeping these accuracies balanced.
#' For a classifier to achieve a high G-mean score, it must perform well on both
#' positive and negative classes.
#'
#' The formula for G-mean is:
#' \deqn{G-mean = \sqrt{Sensitivity \times Specificity}}{G-mean = sqrt(Sensitivity * Specificity)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' simple_g_mean <- dx_g_mean(cm, detail = "simple")
#' detailed_g_mean <- dx_g_mean(cm)
#' print(simple_g_mean)
#' print(detailed_g_mean)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a 'dx_cm' object.
#' @seealso \code{\link{dx_sensitivity}}, \code{\link{dx_specificity}} for components of G-mean.
#' @export
#' @concept metrics
dx_g_mean <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_g_mean, "G-mean", detail, boot, bootreps)
}


calculate_g_mean <- function(cm) {
  # Extract sensitivity and specificity from the confusion matrix
  sens <- dx_sensitivity(cm, "simple")
  spec <- dx_specificity(cm, "simple")

  # Calculate G-mean
  sqrt(sens * spec)
}

#' Calculate Fowlkes-Mallows Index
#'
#' Calculates the Fowlkes-Mallows Index (FM Index) for the provided confusion matrix.
#' FM Index is the geometric mean of precision and recall, providing a balance
#' measure between these two metrics.
#'
#' @inheritParams metrics-params
#'
#' @details
#' Fowlkes-Mallows Index is defined as the geometric mean of the precision (Positive Predictive Value)
#' and recall (True Positive Rate or Sensitivity). It's a useful measure when you want a balance between
#' precision and recall without the harshness of the harmonic mean used in F1 score. A higher Fowlkes-Mallows
#' Index indicates better precision and recall balance.
#'
#' The formula for Fowlkes-Mallows Index is:
#' \deqn{FM = \sqrt{Precision \times Recall}}{FM = sqrt(Precision * Recall)}
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' simple_fm_index <- dx_fowlkes_mallows(cm, detail = "simple")
#' detailed_fm_index <- dx_fowlkes_mallows(cm)
#' print(simple_fm_index)
#' print(detailed_fm_index)
#' @seealso \code{\link{dx_cm}} to understand how to create and interact with a 'dx_cm' object.
#' @seealso \code{\link{dx_ppv}}, \code{\link{dx_sensitivity}} for components of FM Index.
#' @export
#' @concept metrics
dx_fowlkes_mallows <- function(cm, detail = "full", boot = FALSE, bootreps = 1000) {
  evaluate_metric(cm, calculate_fowlkes_mallows, "Fowlkes-Mallows Index", detail, boot, bootreps)
}



calculate_fowlkes_mallows <- function(cm) {
  # Extract PPV (precision) and sensitivity (recall) from the confusion matrix
  ppv <- dx_ppv(cm, "simple")  # Positive Predictive Value or Precision
  recall <- dx_sensitivity(cm, "simple")  # Sensitivity or Recall

  # Calculate Fowlkes-Mallows Index
  sqrt(ppv * recall)
}


check_zero_denominator <- function(denominator, metric) {
  if (denominator == 0) {
    warning(paste0(metric, " is undefined because the denominator is 0. Returning NA."))
    return(TRUE)
  }
  return(FALSE)
}

#' Create a Confusion Matrix from Predictions and Truth
#'
#' This function calculates a confusion matrix from predicted probabilities,
#' true outcomes, a threshold for classification, and a designated positive label.
#' It calculates true positives, false negatives, true negatives, false positives,
#' and several other useful metrics.
#'
#' @param predprob Numeric vector of prediction probabilities.
#' @param truth Numeric vector of true binary class outcomes.
#' @param threshold Numeric value to determine the cutoff for classifying predictions as positive.
#' @param poslabel The label of the positive class in the truth data.
#' @return A dataframe object of class "dx_cm" containing the components of the confusion matrix and additional metrics:
#' \itemize{
#'   \item \code{tp}: True Positives
#'   \item \code{fn}: False Negatives
#'   \item \code{tn}: True Negatives
#'   \item \code{fp}: False Positives
#'   \item \code{dispos}: Number of Actual Positives
#'   \item \code{disneg}: Number of Actual Negatives
#'   \item \code{n}: Total Number of Observations
#'   \item \code{correct}: Number of Correct Predictions
#'   \item \code{testpos}: Number of Predicted Positives
#'   \item \code{testneg}: Number of Predicted Negatives
#' }
#' @details The function takes predicted probabilities and a threshold to create binary
#' predictions which are then compared to the true labels to create a confusion matrix.
#' It is useful for evaluating the performance of a binary classification model.
#' @examples
#' # Example usage:
#' true_labels <- c(1, 0, 1, 1, 0)
#' predicted_probs <- c(0.9, 0.3, 0.6, 0.8, 0.1)
#' cm <- dx_cm(predicted_probs, true_labels, threshold = 0.5, poslabel = 1)
#' print(cm)
#' @export
dx_cm <- function(predprob, truth, threshold, poslabel) {
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
  cm <- data.frame(tp, fn, tn, fp, dispos, disneg, n, correct, testpos, testneg)

  class(cm) <- c("dx_cm", class(cm))
  return(cm)

}

#' Return an pROC::roc object for a dx object
#' @param true_varname Column name containing AI reference standard (string)
#' @param pred_varname Column name containing AI prediction (string)
#' @param data A tbl.
#' @param direction Direction for roc comparison.  See ?pROC::roc
get_roc <- function(true_varname, pred_varname, data, direction) {
  # truth ~ predicted
  f <- stats::as.formula(paste0(true_varname, "~", pred_varname))
  # Use eval and bquote so the "Call" output of the model is human readable
  eval(bquote(
    pROC::roc(.(f), data = data, ci = TRUE, direction = .(direction), quiet = TRUE)
  ))
}

#' Calculate Brier Score
#'
#' @description
#' The Brier score is a proper score function that measures the accuracy of probabilistic predictions.
#' It is applicable to tasks in which predictions must assign probabilities to a set of mutually
#' exclusive discrete outcomes. For binary classification, the Brier score is a measure of how far
#' the predicted probabilities are from the actual outcomes.
#'
#' @inheritParams metrics-params
#'
#' @details
#' The formula for the Brier score in a binary classification is:
#'
#' \deqn{BS = \frac{1}{N} \sum_{i=1}^{N} (f_i - o_i)^2}{BS = (1/N) * sum((f_i - o_i)^2)}
#'
#' where:
#' - \(N\) is the number of predictions,
#' - \(f_i\) is the predicted probability of the occurrence of the positive class for the ith prediction,
#' - \(o_i\) is the actual outcome for the ith prediction, 0 or 1.
#'
#' The Brier score ranges from 0 to 1, where 0 represents a perfect model and 1 represents the worst model.
#' It is equivalent to the mean squared error used in regression and can be decomposed into calibration loss,
#' refinement loss, and uncertainty. This makes it a very informative metric for probabilistic forecasts,
#' providing a nuanced view of the model's predictive performance.
#'
#' @examples
#' predprob <- dx_heart_failure$predicted
#' truth <- dx_heart_failure$truth
#' simple <- dx_brier(predprob, truth, detail = "simple")
#' detailed <- dx_brier(predprob, truth)
#' print(simple)
#' print(detailed)
#' @concept metrics
#' @export
dx_brier <- function(predprob, truth, detail = "full") {
  # Ensuring that the length of predicted probabilities and actual outcomes are the same
  if (length(predprob) != length(truth)) {
    stop("The length of predicted probabilities and actual outcomes must be the same.")
  }

  brier <- calculate_brier(truth, predprob)
  if (detail == "simple") {
    return(brier)
  } else if (detail == "full") {
    return(measure_df(
      measure = "Brier Score",
      estimate = format(brier, digits = 2),
      estimate_raw = brier,
      notes = "CIs not yet implemented"
    ))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}



calculate_brier <- function(truth, predprob) {
  mean((predprob - truth) ^ 2)
}

#' Calculate No Information Rate (NIR)
#'
#' @description
#' The No Information Rate is the proportion of the largest class in the actual outcomes.
#' It represents the accuracy that a naive model would achieve by always predicting
#' the most frequent class. It's a baseline measure for classification performance.
#'
#' @inheritParams metrics-params
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' nir <- dx_nir(cm)
#' print(nir)
#' @export
#' @concept metrics
dx_nir <- function(cm, detail = "full") {
  # Calculate the total number of actual positives and negatives
  dispos <- cm$dispos  # Number of actual positives
  disneg <- cm$disneg  # Number of actual negatives

  # The NIR is the proportion of the largest class
  num <- max(dispos, disneg)
  den <- dispos + disneg
  nir <- num / den

  if (detail == "simple") {
    return(nir)
  } else if (detail == "full") {
    return(measure_df(
      measure = "No Information Rate",
      estimate = format(nir, digits = 2),
      estimate_raw = nir,
      fraction = paste0(comma(num), "/", comma(den))
    ))
  }


  return(nir)
}



