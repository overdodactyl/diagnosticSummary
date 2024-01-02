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

    breslow_day_statistic <- breslow_day_statistic + (a_value - tilde_a_value) ^ 2 / var_a_value
  }

  if (correct) {
    correction_term <- (sum(a_values) - sum(tilde_a_values)) ^ 2 / sum(var_a_values)
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
  (1 / tilde_a_value + 1 / (mj[1] - tilde_a_value) + 1 / (nj[1] - tilde_a_value) + 1 / (mj[2] - nj[1] + tilde_a_value)) ^ -1
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
#'             threshold = 0.3, poslabel = 1)
#' simple <- dx_chi_square(cm, detail = "simple")
#' detailed <- dx_chi_square(cm)
#' print(simple)
#' print(detailed)
#' @seealso \code{\link{dx_cm}} for creating a 'dx_cm' object.
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
#'             threshold = 0.3, poslabel = 1)
#' simple <- dx_fishers_exact(cm, detail = "simple")
#' detailed <- dx_fishers_exact(cm)
#' print(simple)
#' print(detailed)
#' @seealso \code{\link{dx_cm}} for creating a 'dx_cm' object.
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
#'             threshold = 0.3, poslabel = 1)
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
  observed[observed == 0] <- 1e-5  # small constant to avoid log(0)
  expected[expected == 0] <- 1e-5  # small constant to avoid log(0)

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


