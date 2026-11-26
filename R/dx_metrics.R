#' Shared Parameters for Diagnostic Metrics
#'
#' @param cm A dx_cm object created by [dx_cm()].
#' @param detail Character specifying the level of detail in the output:
#'    "simple" for raw estimate, "full" for detailed estimate
#'     including 95% confidence intervals.
#' @param ... Additional arguments to pass to metric_binomial function, such as
#'    `citype` for type of confidence interval method.
#' @param boot Logical specifying if confidence intervals should be generated
#'    via a percentile bootstrap of the confusion matrix cell counts. Call
#'    [set.seed()] first for reproducible intervals.
#' @param bootreps The number of bootstrap replications for calculating confidence intervals.
#' @param predprob Numeric vector of predicted probabilities associated with the positive class.
#' @param truth Numeric vector of true binary outcomes, typically 0 or 1, with the same length as `predprob`.
#' @param dx1 A `dx` object
#' @param dx2 A `dx` object
#' @return Depending on the `detail` parameter, returns a numeric value
#'    representing the calculated metric or a data frame/tibble with
#'    detailed diagnostics including confidence intervals and possibly other
#'    metrics relevant to understanding the metric.
#' @name metrics-params
#' @keywords internal
NULL

#' Calculate Accuracy
#'
#' Calculates the proportion of correct predictions (True Positives + True Negatives)
#' over all cases from a confusion matrix object, providing a measure
#' of the classifier's overall correctness.
#'
#' @inherit metrics-params
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
#' @seealso [dx_cm()] to understand how to create and interact with a
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
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_ppv <- dx_ppv(cm, detail = "simple")
#' detailed_ppv <- dx_ppv(cm)
#' print(simple_ppv)
#' print(detailed_ppv)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @name ppv
#' @concept metrics
#' @aliases dx_precision
#' @export
dx_ppv <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tp, cm$testpos, name = "Positive Predictive Value", detail = detail, ...)
}

#' @rdname ppv
#' @export
dx_precision <- dx_ppv

#' Calculate Negative Predictive Value (NPV)
#'
#' Calculates the proportion of true negatives out of the total predicted negatives
#' (true negatives + false negatives), known as the Negative Predictive Value (NPV).
#' This metric is a measure of the classifier's ability to identify negatives correctly.
#' Note that NPV, like other metrics, may not fully represent classifier performance
#' in unbalanced datasets and should be used alongside other metrics.
#'
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_npv <- dx_npv(cm, detail = "simple")
#' detailed_npv <- dx_npv(cm)
#' print(simple_npv)
#' print(detailed_npv)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_npv <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tn, cm$testneg, name = "Negative Predictive Value", detail = detail, ...)
}

metric_npv_ppv_prevalence <- function(est, logit, varlogit, ...) {
  cc <- stats::qnorm(1 - (1 - 0.95) / 2)
  lci_logit <- logit - cc * sqrt(varlogit)
  uci_logit <- logit + cc * sqrt(varlogit)

  lower <- exp(lci_logit) / ( 1 + exp(lci_logit))
  upper <- exp(uci_logit) / ( 1 + exp(uci_logit))

  # The logit CI is undefined when sensitivity or specificity is 0 or 1
  if (!is.finite(logit) || !is.finite(varlogit)) {
    lower <- NA_real_
    upper <- NA_real_
  }

  formatted_estimate <- conf_int(
    est,
    lower,
    upper,
    percent = TRUE
  )

  measure_df(
    estimate = formatted_estimate,
    estimate_raw = est,
    lci_raw = lower,
    uci_raw = upper,
    ci_type = "Simple Logit (Mercaldo et al, 2007)",
    ...
  )
}

#' Calculate Negative Predictive Value (NPV) at Target Prevalence
#'
#' Computes the Negative Predictive Value (NPV) adjusted to a specified prevalence
#' level. This function is useful for understanding classifier performance in settings
#' where the actual prevalence of the condition may differ from that in the initial data.
#'
#' @inherit metrics-params
#'
#' @param prevalence Numeric value between 0 and 1, representing the target prevalence
#'        for adjusting the NPV calculation.
#'
#' @details
#' This function calculates the NPV using the formula:
#' \deqn{NPV = \frac{Specificity \times (1 - Prevalence)}{(1 - Sensitivity) \times Prevalence + Specificity \times (1 - Prevalence)}}
#' where `Specificity` is the true negative rate and `Sensitivity` is the true positive rate.
#' Adjusting NPV for prevalence allows better estimation of the classifier’s performance
#' in different population settings.
#'
#' Confidence intervales are calucated using a simple logit (Mercaldo et al, 2007)
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold = 0.5, poslabel = 1
#' )
#' # Calculate NPV at a prevalence of 0.1
#' dx_npv_prevalence(cm, prevalence = 0.1)
#'
#' @seealso [dx_cm()], [dx_npv()] for the non-prevalence adjusted NPV.
#' @export
#' @concept metrics
dx_npv_prevalence <- function(cm, prevalence, detail = "full", ...) {
  spec <- dx_specificity(cm, detail = "simple")
  sens <- dx_sensitivity(cm, detail = "simple")
  npv <- (spec * (1 - prevalence)) / ((1 - sens) * prevalence + spec * (1 - prevalence))
  if (detail == "simple") return(npv)
  logit <- log((spec * (1 - prevalence)) / ((1 - sens) * prevalence))
  varlogit <-  (sens / (1 - sens)) * 1 / cm$dispos + ((1 - spec) / spec ) * 1 / cm$disneg
  metric_npv_ppv_prevalence(
    est = npv,
    logit,
    varlogit,
    measure = "Negative Predictive Value (Target Prevalence)",
    notes = paste0("Prevalence value: ", prevalence)
  )
}

#' Calculate Positive Predictive Value (PPV) at Target Prevalence
#'
#' Computes the Positive Predictive Value (PPV) adjusted to a specified prevalence
#' level. This function is useful for understanding classifier performance in settings
#' where the actual prevalence of the condition may differ from that in the initial data.
#'
#' @inherit metrics-params
#'
#' @param prevalence Numeric value between 0 and 1, representing the target prevalence
#'        for adjusting the PPV calculation.
#' @details
#' This function calculates the PPV using the formula:
#' \deqn{PPV = \frac{Sensitivity \times Prevalence}{(Sensitivity \times Prevalence) + (1 - Specificity) \times (1 - Prevalence)}}
#' where `Sensitivity` is the true positive rate and `Specificity` is the true negative rate.
#' Adjusting PPV for prevalence allows a more accurate assessment of the classifier’s performance
#' in different population settings.
#'
#' Confidence intervales are calucated using a simple logit (Mercaldo et al, 2007)
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold = 0.5, poslabel = 1
#' )
#' # Calculate PPV at a prevalence of 0.1
#' dx_ppv_prevalence(cm, prevalence = 0.1)
#'
#' @seealso [dx_cm()], [dx_ppv()] for the non-prevalence adjusted PPV.
#' @export
#' @concept metrics
dx_ppv_prevalence <- function(cm, prevalence, detail = "full", ...) {
  spec <- dx_specificity(cm, detail = "simple")
  sens <- dx_sensitivity(cm, detail = "simple")
  ppv <- (sens * prevalence) / ((sens * prevalence) + (1-spec) * (1 - prevalence))

  if (detail == "simple") return(ppv)

  logit <- log((sens * prevalence) / ((1 - spec) * (1 - prevalence)))
  varlogit <- ((1 - sens) / sens ) * 1 / cm$dispos + (spec / (1 - spec) ) * 1 / cm$disneg
  metric_npv_ppv_prevalence(
    est = ppv,
    logit,
    varlogit,
    measure = "Positive Predictive Value (Target Prevalence)",
    notes = paste0("Prevalence value: ", prevalence)
  )
}

#' Calculate False Negative Rate (FNR)
#'
#' Calculates the False Negative Rate (FNR), which is the proportion of actual positives
#' that were incorrectly identified as negatives by the classifier. FNR is also known
#' as the miss rate and is a critical measure in evaluating the performance of a classifier,
#' especially in scenarios where failing to detect positives is costly.
#'
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_fnr <- dx_fnr(cm, detail = "simple")
#' detailed_fnr <- dx_fnr(cm)
#' print(simple_fnr)
#' print(detailed_fnr)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_fnr <- function(cm, detail = "full", ...) {
  metric_binomial(cm$fn, cm$dispos, name = "False Negative Rate", detail = detail, ...)
}

#' @rdname fnr
#' @export
dx_miss_rate <- dx_fnr

#' Calculate False Positive Rate (FPR)
#'
#' Calculates the False Positive Rate (FPR), which is the proportion of actual negatives
#' that were incorrectly identified as positives by the classifier. FPR is also known
#' as the fall-out rate and is crucial in evaluating the specificity of a classifier.
#'
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_fpr <- dx_fpr(cm, detail = "simple")
#' detailed_fpr <- dx_fpr(cm)
#' print(simple_fpr)
#' print(detailed_fpr)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_fpr <- function(cm, detail = "full", ...) {
  metric_binomial(cm$fp, cm$disneg, name = "False Positive Rate", detail = detail, ...)
}

#' @rdname fpr
#' @export
dx_fall_out <- dx_fpr

#' Calculate False Discovery Rate (FDR)
#'
#' Calculates the False Discovery Rate (FDR), which is the proportion of false positives
#' among all positive predictions. FDR is a critical measure in many classification contexts,
#' particularly where the cost of a false positive is high.
#'
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_fdr <- dx_fdr(cm, detail = "simple")
#' detailed_fdr <- dx_fdr(cm)
#' print(simple_fdr)
#' print(detailed_fdr)
#' @seealso [dx_cm()] to understand how to create and interact with a
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
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_sensitivity <- dx_sensitivity(cm, detail = "simple")
#' detailed_sensitivity <- dx_sensitivity(cm)
#' print(simple_sensitivity)
#' print(detailed_sensitivity)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @name sensitivity
#' @aliases dx_recall, dx_tpr
#' @export
#' @concept metrics
dx_sensitivity <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tp, cm$dispos, name = "Sensitivity", detail = detail, ...)
}

#' @rdname sensitivity
#' @export
dx_recall <- dx_sensitivity

#' @rdname sensitivity
#' @export
dx_tpr <- dx_sensitivity

#' Calculate Specificity (True Negative Rate)
#'
#' Calculates Specificity, also known as the True Negative Rate (TNR), which is the
#' proportion of actual negatives that are correctly identified as such by the classifier.
#' Specificity is a key measure in evaluating the effectiveness of a classifier in
#' identifying negative instances.
#'
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_specificity <- dx_specificity(cm, detail = "simple")
#' detailed_specificity <- dx_specificity(cm)
#' print(simple_specificity)
#' print(detailed_specificity)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @name specificity
#' @aliases dx_tnr
#' @export
#' @concept metrics
dx_specificity <- function(cm, detail = "full", ...) {
  metric_binomial(cm$tn, cm$disneg, name = "Specificity", detail = detail, ...)
}

#' @rdname specificity
#' @export
dx_tnr <- dx_specificity



#' Calculate Balanced Accuracy
#'
#' Calculates Balanced Accuracy, which is the average of sensitivity (recall) and specificity.
#' This metric is particularly useful for imbalanced datasets as it accounts for both the
#' positive and negative classes equally and doesn't inherently favor the majority class.
#'
#' @inherit metrics-params
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
#' @seealso [dx_cm()] to understand how to create and interact with a 'dx_cm' object.
#' @seealso [dx_sensitivity()], [dx_specificity()] for the components of balanced accuracy.
#' @export
#' @concept metrics
dx_balanced_accuracy <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_prevalence <- dx_prevalence(cm, detail = "simple")
#' detailed_prevalence <- dx_prevalence(cm)
#' print(simple_prevalence)
#' print(detailed_prevalence)
#' @seealso [dx_cm()] to understand how to create and interact with a
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
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_detection_prevalence <- dx_detection_prevalence(cm, detail = "simple")
#' detailed_detection_prevalence <- dx_detection_prevalence(cm)
#' print(simple_detection_prevalence)
#' print(detailed_detection_prevalence)
#' @seealso [dx_cm()] to understand how to create and interact with a
#'          'dx_cm' object.
#' @export
#' @concept metrics
dx_detection_prevalence <- function(cm, detail = "full", ...) {
  metric_binomial(cm$testpos, cm$n, name = "Detection Prevalence", detail = detail, ...)
}


get_kappa_interpretation <- function(kappa) {
  # Define the breaks and labels for the categories
  breaks <- c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c(
    "Less than chance agreement", "Slight Agreement", "Fair Agreement",
    "Moderate Agreement", "Substantial Agreement", "Almost Perfect Agreement"
  )

  # Use cut to find the category for each kappa value
  categories <- cut(kappa, breaks = breaks, labels = labels, right = FALSE)
  as.character(categories)
}



#' Calculate Cohen's Kappa
#'
#' Calculates Cohen's Kappa, a statistical measure of inter-rater reliability
#' or agreement for qualitative (categorical) items. It is generally thought
#' to be a more robust measure than simple percent agreement calculation since
#' Kappa takes into account the agreement occurring by chance.
#'
#' @inherit metrics-params
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
#' - <0: Less than chance agreement
#' - 0.-0.2: Slight agreement
#' - 0.2-0.4: Fair agreement
#' - 0.4-0.6: Moderate agreement
#' - 0.6-0.8: Substantial agreement
#' - 0.8-1.0: Almost perfect agreement
#'
#' The 95% confidence interval uses the large sample variance of Fleiss, Cohen,
#' and Everitt (1969) with a normal approximation, and is limited to -1 to 1.
#'
#' @references
#' Fleiss JL, Cohen J, Everitt BS. Large sample standard errors of kappa and
#' weighted kappa. Psychological Bulletin. 1969;72(5):323-327.
#'
#' @examples
#' # Assuming you have a confusion matrix cm with appropriate structure
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' kappa_simple <- dx_cohens_kappa(cm, detail = "simple")
#' kappa_full <- dx_cohens_kappa(cm)
#' print(kappa_simple)
#' print(kappa_full)
#'
#' @export
#' @concept metrics
dx_cohens_kappa <- function(cm, detail = "full") {
  validate_detail(detail)
  # Convert to numeric to avoid integer overflow
  tp <- as.numeric(cm$tp)
  tn <- as.numeric(cm$tn)
  fp <- as.numeric(cm$fp)
  fn <- as.numeric(cm$fn)
  n <- as.numeric(cm$n)

  # Calculate observed agreement (po)
  po <- (tp + tn) / n

  # Calculate expected agreement (pe)
  pe <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / (n^2)

  # Calculate Cohen's Kappa. When every case falls in one cell, expected
  # agreement is 1 and kappa is undefined, so return 0 like MCC and F1.
  undefined <- pe == 1
  kappa <- if (undefined) 0 else (po - pe) / (1 - pe)

  if (detail == "simple") {
    return(kappa)
  } else if (detail == "full") {

    # Large sample variance of Fleiss, Cohen, and Everitt (1969). Cell and
    # marginal proportions, with rows for the prediction and columns for truth.
    p11 <- tp / n
    p12 <- fp / n
    p21 <- fn / n
    p22 <- tn / n
    row1 <- p11 + p12
    row2 <- p21 + p22
    col1 <- p11 + p21
    col2 <- p12 + p22
    agree <- p11 * (1 - (row1 + col1) * (1 - kappa))^2 +
      p22 * (1 - (row2 + col2) * (1 - kappa))^2
    disagree <- (1 - kappa)^2 * (p12 * (col1 + row2)^2 + p21 * (col2 + row1)^2)
    var_kappa <- (agree + disagree - (kappa - pe * (1 - kappa))^2) / (n * (1 - pe)^2)

    # Rounding can make a variance that is essentially 0 slightly negative
    se_kappa <- sqrt(max(var_kappa, 0))

    # Confidence interval using normal approximation, typically Z = 1.96 for 95%
    z <- 1.96
    # Kappa is bounded by -1 and 1
    ci_lower <- if (undefined) NA_real_ else max(kappa - z * se_kappa, -1)
    ci_upper <- if (undefined) NA_real_ else min(kappa + z * se_kappa, 1)

    note <- get_kappa_interpretation(kappa)

    res <- measure_df(
      measure = "Cohen's Kappa",
      estimate = conf_int(kappa, ci_lower, ci_upper, accuracy = .01),
      estimate_raw = kappa,
      ci_type = "Fleiss et al. (1969) SE + normal approximation",
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
#' @inherit metrics-params
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
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' mcc_simple <- dx_mcc(cm, detail = "simple")
#' mcc_full <- dx_mcc(cm)
#' print(mcc_simple)
#' print(mcc_full)
#'
#' @export
#' @concept metrics
dx_mcc <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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

  # MCC is 0 when the denominator is 0
  ifelse(denominator == 0, 0, numerator / denominator)
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
  validate_detail(detail)
  if (detail == "simple") {
    return(x / n)
  } else if (detail == "full") {
    return(ci_binomial(x, n, measure = name, citype, ...))
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

  # Some methods (e.g. agresti-coull, asymptotic) can extend past 0 or 1
  res$lower <- pmax(res$lower, 0)
  res$upper <- pmin(res$upper, 1)

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
#' @inherit metrics-params
#'
#' @details
#' The odds ratio is calculated as (TP * TN) / (FP * FN). It is used in case-control
#' studies to estimate the strength of the association between exposure and outcome.
#' Note that a value of 1 indicates no association, greater than 1 indicates increased
#' odds of the outcome with exposure, and less than 1 indicates decreased odds.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
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
#' @inherit metrics-params
#'
#' @details
#' The negative likelihood ratio is calculated as (FN / (TP + FN)) / (TN / (FP + TN)).
#' It is used to assess the diagnostic usefulness of a test. A LR- closer to 0 indicates
#' a good diagnostic test that can confidently rule out the disease when the test is negative.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_lrn <- dx_lrt_neg(cm, detail = "simple")
#' detailed_lrn <- dx_lrt_neg(cm)
#' print(simple_lrn)
#' print(detailed_lrn)
#' @export
#' @concept metrics
dx_lrt_neg <- function(cm, detail = "full", ...) {
  ratio <- function(tp, tn, fp, fn) (fn / (tp + fn)) / (tn / (fp + tn))
  sd <- function(tp, tn, fp, fn) sqrt(1 / fn - 1 / (tp + fn) + 1 / tn - 1 / (fp + tn))
  metric_ratio(cm, ratio, sd, detail, name = "LRT-", ...)
}

#' Calculate Positive Likelihood Ratio
#'
#' Calculates the Positive Likelihood Ratio (LR+) from a confusion matrix object.
#' LR+ compares the probability of a positive test result among patients with the
#' disease to the probability of a positive test result among patients without the disease.
#'
#' @inherit metrics-params
#'
#' @details
#' The positive likelihood ratio is calculated as (TP / (TP + FN)) / (FP / (FP + TN)).
#' It is used to assess the diagnostic usefulness of a test. A LR+ much greater than 1
#' indicates a good diagnostic test that can confidently confirm the disease when the
#' test is positive.
#'
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_lrp <- dx_lrt_pos(cm, detail = "simple")
#' detailed_lrp <- dx_lrt_pos(cm)
#' print(simple_lrp)
#' print(detailed_lrp)
#' @export
#' @concept metrics
dx_lrt_pos <- function(cm, detail = "full", ...) {
  ratio <- function(tp, tn, fp, fn) (tp / (tp + fn)) / (fp / (fp + tn))
  sd <- function(tp, tn, fp, fn) sqrt(1 / tp - 1 / (tp + fn) + 1 / fp - 1 / (fp + tn))
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
  validate_detail(detail)

  # Ratios are undefined unless both outcome classes are present
  if (cm$dispos == 0 | cm$disneg == 0) {
    if (detail == "simple") {
      return(NA_real_)
    }
    return(measure_df(
      measure = list(...)$name,
      estimate = conf_int(NA, NA, NA),
      notes = "Requires both outcome classes",
      estimate_raw = NA_real_
    ))
  }

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
    return(ci_ratio(tp, tn, fp, fn, ratio, ratio_sd, continuity_correction = continuity_correction, ...))
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
  z_value <- 1.95996398454005 # 95% CI
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
#' Calculates the area under the precision-recall (PR) curve, using the
#' interpolation of Davis and Goadrich (2006) between thresholds.
#'
#' @param truth Numeric vector of true binary outcomes (0 or 1).
#' @param predprob Numeric vector of predicted probabilities that the outcome
#'   is 1, the same length as `truth`.
#' @param detail Character string specifying the level of detail in the output: "simple"
#'        for just the AUC value, "full" for a data frame of results.
#' @param poslabel The outcome class that is the event: 1 (default) or 0. With
#'   `poslabel = 0`, a prediction below the threshold is a positive test.
#' @return Depending on the `detail` parameter, returns a single numeric value of AUC or
#'         a data frame with the AUC. Confidence intervals are not calculated.
#' @details
#' Precision and recall are calculated at every distinct prediction. Between two
#' neighboring thresholds, true and false positives increase together, so
#' precision follows a curve rather than a straight line. Joining the points
#' with straight lines (the trapezoidal rule) overstates the area, especially
#' when many predictions are tied (Davis and Goadrich, 2006).
#'
#' Instead, between neighboring thresholds false positives are assumed to
#' increase in proportion to true positives, which is the expected path when
#' tied predictions are ordered at random. Precision is integrated exactly along
#' that path. The curve starts with no predictions positive and ends with all
#' predictions positive (recall of 1 and precision equal to the prevalence).
#'
#' The AUC-PR is `NA` unless both outcome classes are present. A classifier that
#' guesses at random has an expected AUC-PR equal to the prevalence, so compare
#' values with the prevalence rather than with 0.5.
#'
#' The same curve is drawn by [dx_plot_pr()].
#' @references
#' Davis J, Goadrich M. The relationship between precision-recall and ROC
#' curves. Proceedings of the 23rd International Conference on Machine
#' Learning. 2006:233-240.
#' @examples
#' dx_auc_pr(dx_heart_failure$truth, dx_heart_failure$predicted)
#' dx_auc_pr(dx_heart_failure$truth, dx_heart_failure$predicted, detail = "simple")
#' @export
#' @concept metrics
dx_auc_pr <- function(truth, predprob, detail = "full", poslabel = 1) {
  validate_detail(detail)
  check_truth_predprob(truth, predprob)
  if (length(poslabel) != 1 || !poslabel %in% c(0, 1)) {
    stop("poslabel should be 0 or 1", call. = FALSE)
  }

  # AUC-PR is undefined unless both outcome classes are present
  if (length(unique(truth)) < 2) {
    if (detail == "simple") {
      return(NA_real_)
    }
    return(measure_df(
      measure = "AUC PR",
      estimate = "-",
      notes = "Requires both outcome classes",
      estimate_raw = NA_real_
    ))
  }

  counts <- threshold_counts(predprob, truth, poslabel)
  curve <- pr_curve_counts(
    counts$tp, counts$fp,
    events = sum(truth == poslabel), n = length(truth)
  )
  auc <- pr_area(curve$tp, curve$fp)

  if (detail == "simple") {
    return(auc)
  }
  measure_df(
    measure = "AUC PR",
    estimate = format_number(auc, accuracy = .001),
    estimate_raw = auc,
    notes = "Davis & Goadrich (2006) interpolation"
  )
}

# True and false positive counts along the PR curve, ordered from no
# predictions positive to all predictions positive. `events` and `n` are the
# number of events and cases.
pr_curve_counts <- function(tp, fp, events, n) {
  ord <- order(tp + fp)
  tp <- c(0, tp[ord], events)
  fp <- c(0, fp[ord], n - events)
  # Drop repeated points, e.g. when a threshold already has none or all positive
  keep <- !duplicated(paste(tp, fp))
  list(tp = tp[keep], fp = fp[keep])
}

# Area under the PR curve with Davis & Goadrich (2006) interpolation. Between
# neighboring points, false positives increase by `slope` per true positive, so
# precision is (tp0 + x) / (tp0 + fp0 + (1 + slope) * x) for x from 0 to the
# increase in true positives. Integrating gives
#   x / k + (tp0 - c / k) / k * log(1 + k * x / c),  k = 1 + slope, c = tp0 + fp0
# and when c = 0 (nothing positive yet) precision is the constant 1 / k.
pr_area <- function(tp, fp) {
  events <- tp[length(tp)]
  i <- seq_len(length(tp) - 1)
  d_tp <- tp[i + 1] - tp[i]
  d_fp <- fp[i + 1] - fp[i]

  # Steps that only add false positives don't change recall, so add no area
  step <- d_tp > 0
  tp0 <- tp[i][step]
  c0 <- tp0 + fp[i][step]
  k <- 1 + d_fp[step] / d_tp[step]
  x <- d_tp[step]
  # ifelse() evaluates both branches, so avoid dividing by c0 = 0
  safe_c0 <- ifelse(c0 > 0, c0, 1)
  area <- x / k + ifelse(c0 > 0, (tp0 - c0 / k) / k * log1p(k * x / safe_c0), 0)

  sum(area) / events
}

# Points to draw the PR curve with the same interpolation as pr_area(). Steps
# that add both true and false positives (e.g. tied predictions) are drawn
# with intermediate points.
# Check outcome and prediction vectors passed directly to a metric. The
# signature of dx_auc_pr() changed from (precision, recall), so old calls
# should fail here rather than return a wrong result.
check_truth_predprob <- function(truth, predprob) {
  if (length(truth) != length(predprob)) {
    stop("truth and predprob should be the same length", call. = FALSE)
  }
  if (anyNA(truth) || !all(truth %in% c(0, 1))) {
    stop(paste(
      "truth should contain only 0's and 1's, with no missing values.",
      "dx_auc_pr() takes truth and predprob, not precision and recall."
    ), call. = FALSE)
  }
  if (!is.numeric(predprob) || anyNA(predprob) || any(predprob < 0 | predprob > 1)) {
    stop("predprob should contain probabilities between 0 and 1, with no missing values", call. = FALSE)
  }
}

pr_curve_points <- function(tp, fp, n_between = 20) {
  events <- tp[length(tp)]
  k <- length(tp) - 1
  d_tp <- diff(tp)
  d_fp <- diff(fp)

  # Steps that add both true and false positives are curved, so draw
  # n_between points inside them; other steps are straight lines
  m <- ifelse(d_tp > 0 & d_fp > 0, n_between + 2, 2)
  seg <- rep(seq_len(k), m)
  frac <- (sequence(m) - 1) / (m[seg] - 1)

  # Along a step, true and false positives grow in proportion
  seg_tp <- tp[seg] + frac * d_tp[seg]
  seg_fp <- fp[seg] + frac * d_fp[seg]
  precision <- seg_tp / (seg_tp + seg_fp)

  # With nothing positive yet, precision is the limit along the step
  start <- which(frac == 0 & tp[seg] + fp[seg] == 0)
  precision[start] <- ifelse(
    d_tp[seg[start]] > 0,
    d_tp[seg[start]] / (d_tp[seg[start]] + d_fp[seg[start]]),
    NA_real_
  )

  points <- data.frame(recall = seg_tp / events, precision = precision)
  points <- points[!is.na(points$precision), ]
  points <- points[!duplicated(points), ]
  rownames(points) <- NULL
  points
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
#' @param direction Direction of the comparison, passed to [pROC::roc()]. The default,
#'        "<", treats higher predicted values as more likely to be positive. Use ">" if
#'        lower values indicate the positive class. "auto" lets pROC choose the direction
#'        that gives an AUC of at least 0.5, which hides models that perform worse than
#'        chance and biases the AUC upward in small samples.
#' @return Depending on the `detail` parameter, returns a single numeric value of AUC or
#'         a data frame with the AUC and its confidence intervals.
#' @examples
#' simple_auc <- dx_auc(dx_heart_failure$truth, dx_heart_failure$predicted, detail = "simple")
#' detailed_auc <- dx_auc(dx_heart_failure$truth, dx_heart_failure$predicted)
#' print(simple_auc)
#' print(detailed_auc)
#' @export
#' @concept metrics
dx_auc <- function(truth, predprob, detail = "full", direction = "<") {
  validate_detail(detail)

  # AUC is undefined unless both outcome classes are present
  if (length(unique(truth)) < 2) {
    if (detail == "simple") {
      return(NA_real_)
    }
    return(measure_df(
      measure = "AUC ROC",
      estimate = conf_int(NA, NA, NA),
      notes = "Requires both outcome classes",
      estimate_raw = NA_real_
    ))
  }

  rocest <- pROC::roc(truth, predprob, ci = TRUE, direction = direction, quiet = TRUE)
  aucest <- pROC::auc(rocest)
  auc_raw <- as.numeric(aucest)

  if (detail == "simple") {
    return(auc_raw)
  } else if (detail == "full") {
    auc_ci <- as.numeric(pROC::ci(aucest))
    auc_lci <- auc_ci[1]
    auc_uci <- auc_ci[3]
    auc <- conf_int(auc_raw, auc_lci, auc_uci, percent = FALSE, accuracy = .001)

    return(measure_df(
      measure = "AUC ROC",
      estimate = auc,
      ci_type = "DeLong",
      estimate_raw = auc_raw,
      lci_raw = auc_lci,
      uci_raw = auc_uci
    ))
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

  # Precision and recall are 0 when undefined. Vectorized so it also works
  # on the threshold table.
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))

  # F-beta is 0 when precision and recall are both 0
  beta_sq <- beta^2
  ifelse(
    precision + recall == 0,
    0,
    (1 + beta_sq) * (precision * recall) / (beta_sq * precision + recall)
  )
}

#' Calculate F-beta Score with Confidence Intervals
#'
#' Calculates the F-beta score from a confusion matrix object with an option
#' to include bootstrapped confidence intervals. The F-beta score is a
#' generalization of the F1 score, allowing different importance to precision
#' and recall via the beta parameter.
#'
#' @inherit metrics-params
#' @param beta The beta value determining the weight of precision in the F-score.
#' @return Depending on the `detail` parameter, returns a single numeric value of
#'         F-beta or a data frame with the F-beta and its confidence intervals.
#' @seealso [dx_f1()], [dx_f2()] for specific F-beta scores.
#' @export
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_f <- dx_fbeta(cm, beta = .5, detail = "simple")
#' detailed_f <- dx_fbeta(cm, beta = .5)
#' print(simple_f)
#' print(detailed_f)
#' @concept metrics
dx_fbeta <- function(cm, beta = 1, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @seealso [dx_fbeta()], [dx_f2()] for other F-beta scores.
#' @export
#' @concept metrics
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_f <- dx_f1(cm, detail = "simple")
#' detailed_f <- dx_f1(cm)
#' print(simple_f)
#' print(detailed_f)
dx_f1 <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @seealso [dx_fbeta()], [dx_f1()] for other F-beta scores.
#' @export
#' @concept metrics
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold =
#'     0.5, poslabel = 1
#' )
#' simple_f <- dx_f2(cm, detail = "simple")
#' detailed_f <- dx_f2(cm)
#' print(simple_f)
#' print(detailed_f)
dx_f2 <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
  dx_fbeta(cm, beta = 2, detail, boot, bootreps)
}

#' Calculate Informedness
#'
#' Calculates Informedness for the provided confusion matrix. Informedness is a combined measure
#' of Sensitivity (True Positive Rate) and Specificity (True Negative Rate). It reflects the
#' probability that a classifier is informed about the true class, ranging from -1 to 1.
#'
#' @inherit metrics-params
#'
#' @details
#' Informedness is defined as `Informedness = Sensitivity + Specificity - 1`. It is the sum of the true positive rate
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
#' @seealso [dx_cm()] to understand how to create and interact with a 'dx_cm' object.
#' @seealso [dx_sensitivity()], [dx_specificity()] for components of informedness.
#' @export
#' @concept metrics
dx_informedness <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @export
dx_youden_j <- dx_informedness


#' Calculate Markedness
#'
#' Calculates Markedness for the provided confusion matrix. Markedness is a combined measure
#' of PPV (Positive Predictive Value) and NPV (Negative Predictive Value). It reflects the
#' effectiveness of a classifier in marking class labels correctly, ranging from -1 to 1.
#'
#' @inherit metrics-params
#'
#' @details
#' Markedness is defined as `Markedness = PPV + NPV - 1`. It is the sum of the proportions
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
#' @seealso [dx_cm()] to understand how to create and interact with a 'dx_cm' object.
#' @seealso [dx_ppv()], [dx_npv()] for components of markedness.
#' @export
#' @concept metrics
dx_markedness <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @inherit metrics-params
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
#' @seealso [dx_cm()] to understand how to create and interact with a 'dx_cm' object.
#' @seealso [dx_sensitivity()], [dx_specificity()] for components of G-mean.
#' @export
#' @concept metrics
dx_g_mean <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
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
#' @inherit metrics-params
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
#' @seealso [dx_cm()] to understand how to create and interact with a 'dx_cm' object.
#' @seealso [dx_ppv()], [dx_sensitivity()] for components of FM Index.
#' @export
#' @concept metrics
dx_fowlkes_mallows <- function(cm, detail = "full", boot = FALSE, bootreps = 2000) {
  evaluate_metric(cm, calculate_fowlkes_mallows, "Fowlkes-Mallows Index", detail, boot, bootreps)
}



calculate_fowlkes_mallows <- function(cm) {
  # Extract PPV (precision) and sensitivity (recall) from the confusion matrix
  ppv <- dx_ppv(cm, "simple") # Positive Predictive Value or Precision
  recall <- dx_sensitivity(cm, "simple") # Sensitivity or Recall

  # Calculate Fowlkes-Mallows Index
  sqrt(ppv * recall)
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
#'   \item `tp`: True Positives
#'   \item `fn`: False Negatives
#'   \item `tn`: True Negatives
#'   \item `fp`: False Positives
#'   \item `dispos`: Number of Actual Positives
#'   \item `disneg`: Number of Actual Negatives
#'   \item `n`: Total Number of Observations
#'   \item `correct`: Number of Correct Predictions
#'   \item `testpos`: Number of Predicted Positives
#'   \item `testneg`: Number of Predicted Negatives
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

  cm_from_counts(tp, fn, tn, fp)
}

# Build a dx_cm object from cell counts. The counts can be vectors, e.g. one
# element per threshold, so the metric functions work on every row at once.
cm_from_counts <- function(tp, fn, tn, fp) {
  cm <- data.frame(
    tp = tp,
    fn = fn,
    tn = tn,
    fp = fp,
    dispos = tp + fn, # Actual positives
    disneg = tn + fp, # Actual negatives
    n = tp + fn + tn + fp,
    correct = tp + tn, # Correct predictions
    testpos = tp + fp, # Predicted positives
    testneg = tn + fn # Predicted negatives
  )

  class(cm) <- c("dx_cm", class(cm))
  cm
}

#' Return an pROC::roc object for a dx object
#' @param true_varname Column name containing the reference standard (string)
#' @param pred_varname Column name containing the predictions (string)
#' @param data A tbl.
#' @param direction Direction for roc comparison.  See ?pROC::roc
#' @noRd
get_roc <- function(true_varname, pred_varname, data, direction) {
  # truth ~ predicted
  # Backticks allow column names that aren't syntactic, e.g. with spaces
  f <- stats::as.formula(paste0(backtick(true_varname), "~", backtick(pred_varname)))
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
#' @inherit metrics-params
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
  validate_detail(detail)
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
      estimate = format_number(brier, accuracy = .001),
      estimate_raw = brier,
      notes = "CIs not yet implemented"
    ))
  }
}



calculate_brier <- function(truth, predprob) {
  mean((predprob - truth)^2)
}

#' Calculate No Information Rate (NIR)
#'
#' @description
#' The No Information Rate is the proportion of the largest class in the actual outcomes.
#' It represents the accuracy that a naive model would achieve by always predicting
#' the most frequent class. It's a baseline measure for classification performance.
#'
#' @inherit metrics-params
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = 0.5, poslabel = 1)
#' nir <- dx_nir(cm)
#' print(nir)
#' @export
#' @concept metrics
dx_nir <- function(cm, detail = "full") {
  validate_detail(detail)
  # Calculate the total number of actual positives and negatives
  dispos <- cm$dispos # Number of actual positives
  disneg <- cm$disneg # Number of actual negatives

  # The NIR is the proportion of the largest class
  num <- max(dispos, disneg)
  den <- dispos + disneg
  nir <- num / den

  if (detail == "simple") {
    return(nir)
  } else if (detail == "full") {
    return(measure_df(
      measure = "No Information Rate",
      estimate = format_number(nir, accuracy = .1, percent = TRUE),
      estimate_raw = nir,
      fraction = paste0(comma(num), "/", comma(den))
    ))
  }
}
