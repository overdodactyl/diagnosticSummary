#' Breslow-Day Test for Homogeneity of Odds Ratios
#'
#' Performs the Breslow-Day test for homogeneity of the odds ratios across different
#' strata of data. Optionally includes Tarone's correction. This test is useful for
#' investigating whether the effect of an explanatory variable on the outcome is consistent
#' across different strata.
#'
#' @param contingency_table A 2x2xK contingency table where K is the number of strata.
#' @param odds_ratio Optionally, the overall odds ratio to use in the test.
#' If not provided, the Mantel-Haenszel odds ratio is calculated.
#' @param correct Logical indicating whether to apply Tarone's correction.
#' @return A list with the Breslow-Day test statistic, p-value, method used, and data name.
#' @export
#' @references
#' Breslow NE, Day NE. Statistical methods in cancer research. Volume I -
#' The analysis of case-control studies. IARC Sci Publ. 1980;(32):5-338.
breslow_day_test <- function(contingency_table, odds_ratio = NA, correct = FALSE) {
  if (!is.array(contingency_table) || length(dim(contingency_table)) != 3) {
    stop("The input 'contingency_table' must be a 2x2xK array.")
  }

  if (is.na(odds_ratio)) {
    # Calculate the common OR based on Mantel-Haenszel
    odds_ratio <- stats::mantelhaen.test(contingency_table)$estimate
  }

  num_strata <- dim(contingency_table)[3]
  breslow_day_statistic <- 0
  a_values <- tilde_a_values <- var_a_values <- numeric(num_strata)

  for (stratum_index in 1:num_strata) {
    stratum_data <- contingency_table[, , stratum_index]
    marginals <- margin_sums(stratum_data)
    a_value <- stratum_data[1, 1]
    a_values[stratum_index] <- a_value

    tilde_a_value <- expected_cell_count(a_value, marginals, odds_ratio)
    tilde_a_values[stratum_index] <- tilde_a_value

    var_a_value <- variance_a(tilde_a_value, marginals)
    var_a_values[stratum_index] <- var_a_value

    breslow_day_statistic <- breslow_day_statistic + (a_value - tilde_a_value)^2 / var_a_value
  }

  if (correct) {
    correction_term <- (sum(a_values) - sum(tilde_a_values))^2 / sum(var_a_values)
    breslow_day_statistic <- breslow_day_statistic - correction_term
  }

  p_value <- 1 - stats::pchisq(breslow_day_statistic, num_strata - 1)

  list(
    statistic = breslow_day_statistic,
    p_value = p_value,
    method = if (correct) "Tarone's corrected Breslow-Day" else "Breslow-Day",
    data_name = deparse(substitute(contingency_table))
  )
}

#' Calculate Margin Sums for a Contingency Table
#'
#' Calculates the row and column sums for each 2x2 stratum of the contingency table.
#'
#' @param stratum_data A 2x2 matrix representing a single stratum of the contingency table.
#' @return A list with 'row' and 'col' elements containing the sums of rows and columns, respectively.
#' @keywords internal
margin_sums <- function(stratum_data) {
  row_sums <- apply(stratum_data, 1, sum)
  col_sums <- apply(stratum_data, 2, sum)
  list(row = row_sums, col = col_sums)
}


#' Calculate Expected Cell Count
#'
#' Calculates the expected count of the top left cell (a) under the null hypothesis
#' of homogeneity of odds ratios across strata.
#'
#' @param a_value The observed count for cell 'a' in a single stratum.
#' @param marginals A list containing the 'row' and 'col' marginal sums for the stratum.
#' @param odds_ratio The overall odds ratio to use in the calculation.
#' @return The expected count for cell 'a'.
#' @keywords internal
expected_cell_count <- function(a_value, marginals, odds_ratio) {
  mj <- marginals$row
  nj <- marginals$col
  coef <- c(-mj[1] * nj[1] * odds_ratio, nj[2] - mj[1] + odds_ratio * (nj[1] + mj[1]), 1 - odds_ratio)
  sols <- Re(polyroot(coef))
  min(sols[(0 < sols) & (sols <= min(nj[1], mj[1]))])
}

#' Calculate Variance of Cell Count
#'
#' Calculates the variance of the top left cell (a) count under the null hypothesis
#' of homogeneity of odds ratios across strata.
#'
#' @param tilde_a_value The expected count for cell 'a' in a single stratum.
#' @param marginals A list containing the 'row' and 'col' marginal sums for the stratum.
#' @return The variance of the count for cell 'a'.
#' @keywords internal
variance_a <- function(tilde_a_value, marginals) {
  mj <- marginals$row
  nj <- marginals$col
  (1 / tilde_a_value + 1 / (mj[1] - tilde_a_value) + 1 / (nj[1] - tilde_a_value) + 1 / (mj[2] - nj[1] + tilde_a_value))^-1
}




#' Chi-Square Test for Independence in a 2x2 Table
#'
#' Conducts a Chi-square test of independence on a 2x2 confusion matrix derived
#' from binary classification results, assessing whether the observed frequency
#' distribution differs from the expected distribution.
#'
#' @inheritParams metrics-params
#' @return Depending on the `detail` parameter:
#'         - if "simple": a single numeric value representing the p-value of
#'           the Chi-square test.
#'         - if "full": a data frame with the Chi-square test result, including
#'           the p-value and method note.
#' @details The Chi-square test is used to determine whether there is a
#' significant association between the predicted and actual binary classifications.
#' It compares the observed frequencies in each cell of the table to the frequencies
#' expected if the rows and columns are independent. A low p-value indicates that
#' the distributions of actual and predicted classifications are not independent,
#' suggesting a significant association between them. The function uses Pearson's
#' Chi-squared test with Yates' continuity correction by default, which is more
#' accurate for small sample sizes. The test is most appropriate when each cell
#' in the 2x2 table has an expected frequency of 5 or more.
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold = 0.3, poslabel = 1
#' )
#' simple <- dx_chi_square(cm, detail = "simple")
#' detailed <- dx_chi_square(cm)
#' print(simple)
#' print(detailed)
#' @seealso [dx_cm()] for creating a 'dx_cm' object.
#' @concept tests
#' @export
dx_chi_square <- function(cm, detail = "full") {
  conf_matrix <- matrix(c(cm$tp, cm$fn, cm$fp, cm$tn), nrow = 2)
  chi_square_result <- suppressWarnings({
    stats::chisq.test(conf_matrix)
  })

  if (detail == "simple") {
    return(chi_square_result$p.value)
  } else if (detail == "full") {
    return(measure_df(
      measure = "Pearson's Chi-squared",
      estimate = format_pvalue(chi_square_result$p.value, accuracy = 0.01),
      fraction = "",
      ci_type = "",
      notes = chi_square_result$method,
      estimate_raw = chi_square_result$p.value,
      lci_raw = NA,
      uci_raw = NA
    ))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}

#' Fisher's Exact Test for Independence in a 2x2 Table
#'
#' Conducts Fisher's Exact test of independence on a 2x2 confusion matrix derived
#' from binary classification results, assessing the significance of the association
#' between the observed and expected frequencies.
#'
#' @inheritParams metrics-params
#' @return Depending on the `detail` parameter:
#'         - if "simple": a single numeric value representing the p-value of
#'           Fisher's Exact test.
#'         - if "full": a data frame with the Fisher's Exact test result, including
#'           the p-value and method note.
#' @details Fisher's Exact Test is used to examine the significance of the association
#' between the variables in a 2x2 contingency table, particularly useful when sample
#' sizes are small. Unlike the chi-square test, it does not rely on large sample
#' distribution approximations and is hence exact. It's especially preferred when the
#' data has small expected frequencies in one or more cells of the table. A low p-value
#' indicates a significant association between the predicted and actual binary classifications.
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold = 0.3, poslabel = 1
#' )
#' simple <- dx_fishers_exact(cm, detail = "simple")
#' detailed <- dx_fishers_exact(cm)
#' print(simple)
#' print(detailed)
#' @seealso [dx_cm()] for creating a 'dx_cm' object.
#' @concept tests
#' @export
dx_fishers_exact <- function(cm, detail = "full") {
  conf_matrix <- matrix(c(cm$tp, cm$fn, cm$fp, cm$tn), nrow = 2)
  fisher_result <- stats::fisher.test(conf_matrix)

  if (detail == "simple") {
    return(fisher_result$p.value)
  } else if (detail == "full") {
    return(measure_df(
      measure = "Fisher's Exact",
      estimate = format_pvalue(fisher_result$p.value, accuracy = 0.01),
      fraction = "",
      ci_type = "",
      notes = "Exact test",
      estimate_raw = fisher_result$p.value,
      lci_raw = NA,
      uci_raw = NA
    ))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}


#' G-Test (Log-Likelihood Ratio Test) for Independence in 2x2 Table
#'
#' Conducts a G-test of independence to assess the goodness of fit or the association
#' between two categorical variables in a 2x2 contingency table. It is an alternative to
#' the chi-squared test and is particularly useful when dealing with small expected frequencies.
#'
#' @inheritParams metrics-params
#' @details The test compares the observed frequencies to the expected frequencies based on
#' the marginal totals and calculates a G statistic, which follows a chi-squared distribution.
#' The test is especially useful when the data contains small expected frequencies, which might
#' make the chi-squared test less accurate. A low p-value indicates a significant
#' association between the variables or a significant difference from the expected
#' distribution. Caution is needed with zero counts or very small samples.
#' @examples
#' cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
#'   threshold = 0.3, poslabel = 1
#' )
#' simple <- dx_g_test(cm, detail = "simple")
#' detailed <- dx_g_test(cm)
#' print(simple)
#' print(detailed)
#' @concept tests
#' @export
dx_g_test <- function(cm, detail = "full") {
  # Convert confusion matrix to table and calculate expected frequencies
  conf_matrix <- matrix(c(cm$tp, cm$fn, cm$fp, cm$tn), nrow = 2)
  row_totals <- apply(conf_matrix, 1, sum)
  col_totals <- apply(conf_matrix, 2, sum)
  expected <- outer(row_totals, col_totals) / sum(conf_matrix)

  # Calculate observed frequencies and ensure no zero counts for log calculation
  observed <- as.vector(conf_matrix)
  observed[observed == 0] <- 1e-5 # small constant to avoid log(0)
  expected[expected == 0] <- 1e-5 # small constant to avoid log(0)

  # Calculate G statistic
  G <- 2 * sum(observed * log(observed / expected))

  # Degrees of freedom for 2x2 table
  df <- 1

  # Calculate p-value
  p_value <- stats::pchisq(G, df, lower.tail = FALSE)

  if (detail == "simple") {
    return(p_value)
  } else if (detail == "full") {
    return(measure_df(
      measure = "G-Test",
      estimate = format_pvalue(p_value, accuracy = 0.01),
      fraction = "",
      ci_type = "",
      estimate_raw = p_value,
      lci_raw = NA,
      uci_raw = NA
    ))
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}


# Two Models --------------------------------------------------------------

#' DeLong's Test for Comparing Two ROC Curves
#'
#' This function applies DeLong's test to compare the areas under two correlated ROC curves,
#' providing a statistical approach to assess if there is a significant difference between
#' them. It's particularly useful for comparing the performance of two diagnostic models.
#'
#' @param dx1 A `dx` object containing the first ROC curve.
#' @param dx2 A `dx` object containing the second ROC curve.
#' @param detail Character specifying the level of detail in the output:
#'               "simple" for p-value only, "full" for detailed test results.
#' @param paired Logical indicating if data between `dx1` and `dx2` is paired.
#' @return Depending on the `detail` parameter, returns a p-value or a comprehensive
#'         summary of the test including the test statistic, p-value, and confidence interval
#'         for the difference in AUC.
#' @details The function utilizes the `roc.test` function from the `pROC` package to perform
#' DeLong's test, which is suitable for comparing two correlated ROC curves. This correlation
#' typically arises from using the same set of samples to generate both curves. A significant
#' p-value indicates a statistically significant difference between the AUCs of the two models.
#' When `detail` is "full", additional information about the comparison, including the estimated
#' difference in AUC and its confidence interval, is provided. The confidence interval gives a
#' range of plausible values for the difference in AUC between the two models.
#' @examples
#' dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
#' dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
#' simple <- dx_delong(dx_glm, dx_rf, detail = "simple")
#' detailed <- dx_delong(dx_glm, dx_rf)
#' print(simple)
#' print(detailed)
#' @seealso [pROC::roc.test()] for the underlying test implementation.
#' @seealso [dx_cm()] to understand how to create a `dx` object.
#' @concept tests
#' @export
dx_delong <- function(dx1, dx2, detail = "full", paired = TRUE) {
  test <- pROC::roc.test(
    roc1 = dx1$roc,
    roc2 = dx2$roc,
    paired = paired,
    method = "delong"
  )

  if (detail == "simple") {
    return(test$p.value)
  } else if (detail == "full") {
    roc1 <- as.numeric(test$roc1$auc)
    roc2 <- as.numeric(test$roc2$auc)

    estimate <- abs(roc1 - roc2)
    low <- test$conf.int[1]
    high <- test$conf.int[2]
    low <- ifelse(is.null(low), NA, low)
    high <- ifelse(is.null(high), NA, high)

    summary <- conf_int(estimate, low, high, accuracy = 0.01)

    compare_df(
      test = "DeLong's test for ROC curves",
      summary = summary,
      p_value = test$p.value,
      estimate = abs(roc1 - roc2),
      conf_low = low,
      conf_high = high,
      statistic = as.numeric(test$statistic)
    )
  }
}

#' Z-test for Comparing Two Proportions
#'
#' Conducts a two-sided Z-test (using prop.test for two proportions) to assess whether the success rates (proportions)
#' of two groups are different from each other based on a specified metric. It can compare accuracy,
#' PPV, NPV, FNR, FPR, FDR, sensitivity, or specificity between two dx objects.
#'
#' @param dx1 A `dx` object for the first group.
#' @param dx2 A `dx` object for the second group.
#' @param metric A character string specifying the metric to compare between the two groups.
#'        Options include "accuracy", "ppv", "npv", "fnr", "fpr", "fdr", "sensitivity", "specificity".
#' @param detail Character specifying the level of detail in the output:
#'        "simple" for raw estimate (p-value only), "full" for detailed estimate including
#'        confidence intervals and test statistic.
#' @return Depending on the `detail` parameter, returns the p-value of the test or a
#'         more detailed list including the test statistic, confidence interval, and p-value.
#' @details The function uses the prop.test function to perform the hypothesis test,
#' assuming the null hypothesis that the two proportions based on the specified metric are the same.
#' A low p-value indicates a significant difference in the proportions, suggesting that the success rates
#' of the two groups are statistically significantly different. The function automatically adjusts
#' for continuity and provides confidence intervals for the difference in proportions.
#'
#' The Z-test for two proportions is appropriate when comparing the success rates (proportions) of two independent samples.
#' Here are some considerations for using this test:
#' \itemize{
#'   \item Independence: The samples should be independent. This test is not appropriate for paired or matched data.
#'   \item Sample Size**: Both groups should have a sufficiently large number of trials. As a rule of thumb, the test
#'    assumes that the number of successes and failures in each group should be at least 5. However, for more accurate
#'    results, especially in cases with extreme proportions (close to 0 or 1), larger sample sizes may be necessary.
#'   \item Binary Outcome: This test is specific to binary outcomes (success/failure, presence/absence, etc.) represented
#'    as proportions. It's not suitable for continuous data or counts that haven't been converted to proportions.
#'   \item Normal Approximation: The Z-test is based on the normal approximation of the distribution of the sample proportion.
#'    This approximation is more accurate when the sample size is large and the proportion is not extremely close to 0 or 1.
#' }
#' It's also important to note that while `prop.test` adjusts for continuity, this adjustment may not be sufficient for very
#' small sample sizes or very unbalanced designs. Always consider the context and assumptions of your data when interpreting
#' the results of the test.
#' @examples
#' dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
#' dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
#' dx_z_test(dx_glm, dx_rf, metric = "accuracy")
#' @concept tests
#' @export
dx_z_test <- function(dx1, dx2, metric = c("accuracy", "ppv", "npv", "fnr", "fpr", "fdr", "sensitivity", "specificity"), detail = "full") {
  metric <- match.arg(metric)

  metric_mapping <- list(
    accuracy = c("correct", "n"),
    ppv = c("tp", "testpos"),
    npv = c("tn", "testneg"),
    fnr = c("fn", "dispos"),
    fpr = c("fp", "disneg"),
    fdr = c("fp", "testpos"),
    sensitivity = c("tp", "dispos"),
    specificity = c("tn", "disneg")
  )


  vals <- metric_mapping[[metric]]

  p <- c(dx1$cm[[vals[1]]], dx2$cm[[vals[1]]])
  n <- c(dx1$cm[[vals[2]]], dx2$cm[[vals[2]]])

  test <- stats::prop.test(p, n, alternative = "two.sided")

  if (detail == "simple") {
    return(test$p.value)
  } else if (detail == "full") {
    val1 <- as.numeric(test$estimate[1])
    val2 <- as.numeric(test$estimate[2])

    estimate <- abs(val1 - val2)
    low <- test$conf.int[1]
    high <- test$conf.int[2]

    summary <- conf_int(estimate, low, high, accuracy = 0.01)

    compare_df(
      test = paste0("Z-test: ", metric),
      summary = summary,
      p_value = test$p.value,
      estimate = estimate,
      conf_low = low,
      conf_high = high,
      statistic = as.numeric(test$statistic)
    )
  }
}

#' McNemar's Chi-squared Test for Paired Proportions
#'
#' Performs McNemar's test to evaluate the difference between two paired proportions.
#' This is typically used in the context of binary classification to test whether the
#' proportion of correct classifications significantly differs between two classifiers
#' on the same set of instances.
#'
#' @param dx1 The first `dx` object containing predictions and truth values.
#' @param dx2 The second `dx` object containing predictions and truth values.
#' @param detail A string indicating the level of detail for the output: "simple" for
#'        just the p-value, "full" for a comprehensive result including test statistics.
#' @return Depending on the `detail` parameter, returns either the p-value (simple)
#'         or a more comprehensive list including the test statistic and p-value (full).
#' @details
#' McNemar's test is appropriate when comparing the classification results of two
#' algorithms on the same data set (paired design). It specifically tests the null
#' hypothesis that the marginal probabilities of row and column variable are the same.
#'
#' This test is suitable for binary classification tasks where you are comparing the
#' performance of two classifiers over the same instances. It's not appropriate for
#' independent samples or more than two classifiers.
#'
#' The function expects the input as two `dx` objects, each containing the predictions
#' and truth values from the classifiers being compared. It calculates the contingency
#' table based on the agreements and disagreements between the classifiers and applies
#' McNemar's test to this table.
#'
#' @examples
#' dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
#' dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
#' dx_mcnemars(dx_glm, dx_rf)
#' @seealso [dx_cm()], [mcnemar.test()]
#' @concept tests
#' @export
dx_mcnemars <- function(dx1, dx2, detail = "full") {
  truth <- pluck_truths(dx1)
  predictions1 <- pluck_predicted(dx1)
  predictions2 <- pluck_predicted(dx2)

  # Construct the contingency table for paired samples
  both_correct <- sum(predictions1 == truth & predictions2 == truth)
  first_correct_second_incorrect <- sum(predictions1 == truth & predictions2 != truth)
  first_incorrect_second_correct <- sum(predictions1 != truth & predictions2 == truth)
  both_incorrect <- sum(predictions1 != truth & predictions2 != truth)

  cont_table <- matrix(c(
    both_correct, first_correct_second_incorrect,
    first_incorrect_second_correct, both_incorrect
  ),
  nrow = 2
  )

  # Perform McNemar's test
  test <- stats::mcnemar.test(cont_table)

  if (detail == "simple") {
    return(test$p.value)
  } else if (detail == "full") {
    compare_df(
      test = "McNemar's Chi-squared Test",
      summary = format_pvalue(test$p.value),
      p_value = test$p.value,
      statistic = as.numeric(test$statistic)
    )
  }
}
