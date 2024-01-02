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
