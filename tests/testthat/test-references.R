# Checks against independent references: base R, published examples, and
# independent implementations written here from the published methods.

hf <- dx_heart_failure

dx_obj <- dx(
  data = hf,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = c("AgeGroup", "Sex", "AgeSex")
)

cm <- dx_cm(hf$predicted, hf$truth, threshold = .3, poslabel = 1)

overall <- function(dx_obj, measure) {
  m <- dx_obj$measures
  m[m$variable == "Overall" & m$measure == measure, ]
}


# Binomial confidence intervals -------------------------------------------

test_that("Exact binomial CIs match binom.test (Clopper-Pearson)", {
  counts <- list(
    "Accuracy" = c(cm$correct, cm$n),
    "Sensitivity" = c(cm$tp, cm$dispos),
    "Specificity" = c(cm$tn, cm$disneg),
    "Positive Predictive Value" = c(cm$tp, cm$testpos),
    "Negative Predictive Value" = c(cm$tn, cm$testneg),
    "Prevalence" = c(cm$dispos, cm$n),
    "False Negative Rate" = c(cm$fn, cm$dispos),
    "False Positive Rate" = c(cm$fp, cm$disneg),
    "False Discovery Rate" = c(cm$fp, cm$testpos)
  )

  for (measure in names(counts)) {
    x <- counts[[measure]]
    ci <- stats::binom.test(x[1], x[2])$conf.int
    res <- overall(dx_obj, measure)

    expect_equal(res$conf_low, ci[1], info = measure)
    expect_equal(res$conf_high, ci[2], info = measure)
  }
})

test_that("Wilson CIs match prop.test without continuity correction", {
  dx_wilson <- dx(
    data = hf,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    citype = "wilson"
  )
  ci <- stats::prop.test(cm$tp, cm$dispos, correct = FALSE)$conf.int
  res <- overall(dx_wilson, "Sensitivity")

  expect_equal(res$conf_low, ci[1])
  expect_equal(res$conf_high, ci[2])
})


# Ratio confidence intervals ----------------------------------------------

test_that("Odds ratio and likelihood ratio CIs match published formulas", {
  # Woolf (1955) log odds ratio CI and Simel et al. (1991) log likelihood
  # ratio CIs, with 0.5 added to every cell when any cell is zero
  check_ratios <- function(tp, fn, fp, tn) {
    cm <- make_cm(tp, fn, fp, tn)
    if (any(c(tp, fn, fp, tn) == 0)) {
      tp <- tp + .5
      fn <- fn + .5
      fp <- fp + .5
      tn <- tn + .5
    }
    z <- stats::qnorm(.975)

    or <- (tp * tn) / (fp * fn)
    se <- sqrt(1 / tp + 1 / fn + 1 / fp + 1 / tn)
    res <- dx_odds_ratio(cm)
    expect_equal(res$estimate, or)
    expect_equal(c(res$conf_low, res$conf_high), exp(log(or) + c(-1, 1) * z * se))

    lrp <- (tp / (tp + fn)) / (fp / (fp + tn))
    se <- sqrt(1 / tp - 1 / (tp + fn) + 1 / fp - 1 / (fp + tn))
    res <- dx_lrt_pos(cm)
    expect_equal(res$estimate, lrp)
    expect_equal(c(res$conf_low, res$conf_high), exp(log(lrp) + c(-1, 1) * z * se))

    lrn <- (fn / (tp + fn)) / (tn / (fp + tn))
    se <- sqrt(1 / fn - 1 / (tp + fn) + 1 / tn - 1 / (fp + tn))
    res <- dx_lrt_neg(cm)
    expect_equal(res$estimate, lrn)
    expect_equal(c(res$conf_low, res$conf_high), exp(log(lrn) + c(-1, 1) * z * se))
  }

  check_ratios(40, 10, 20, 30)
  check_ratios(83, 15, 39, 124)
  # Zero cell, continuity correction applied
  check_ratios(20, 5, 0, 30)
})


# Predictive values at a target prevalence ---------------------------------

test_that("Predictive values at a prevalence match Bayes' theorem and Mercaldo (2007)", {
  sens <- cm$tp / cm$dispos
  spec <- cm$tn / cm$disneg
  z <- stats::qnorm(.975)
  expit <- function(x) exp(x) / (1 + exp(x))

  for (prev in c(.03, .2, .5)) {
    ppv <- sens * prev / (sens * prev + (1 - spec) * (1 - prev))
    npv <- spec * (1 - prev) / ((1 - sens) * prev + spec * (1 - prev))

    # Logit CIs from Mercaldo, Lau & Zhou (2007)
    var_ppv <- ((1 - sens) / sens) / cm$dispos + (spec / (1 - spec)) / cm$disneg
    var_npv <- (sens / (1 - sens)) / cm$dispos + ((1 - spec) / spec) / cm$disneg
    ci_ppv <- expit(log(ppv / (1 - ppv)) + c(-1, 1) * z * sqrt(var_ppv))
    ci_npv <- expit(log(npv / (1 - npv)) + c(-1, 1) * z * sqrt(var_npv))

    res_ppv <- dx_ppv_prevalence(cm, prevalence = prev)
    res_npv <- dx_npv_prevalence(cm, prevalence = prev)

    expect_equal(res_ppv$estimate, ppv, info = prev)
    expect_equal(res_npv$estimate, npv, info = prev)
    expect_equal(c(res_ppv$conf_low, res_ppv$conf_high), ci_ppv, tolerance = 1e-6, info = prev)
    expect_equal(c(res_npv$conf_low, res_npv$conf_high), ci_npv, tolerance = 1e-6, info = prev)
  }
})


# Tests of association ----------------------------------------------------

test_that("Association tests match base R and textbook formulas", {
  tbl <- matrix(c(cm$tp, cm$fn, cm$fp, cm$tn), nrow = 2)
  expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl)

  # Pearson chi-square with Yates' continuity correction
  yates <- sum((abs(tbl - expected) - .5)^2 / expected)
  expect_equal(dx_chi_square(cm, "simple"), stats::pchisq(yates, 1, lower.tail = FALSE))

  # Likelihood ratio (G) test
  g <- 2 * sum(tbl * log(tbl / expected))
  expect_equal(dx_g_test(cm, "simple"), stats::pchisq(g, 1, lower.tail = FALSE))

  expect_equal(dx_fishers_exact(cm, "simple"), stats::fisher.test(tbl)$p.value)
})

test_that("McNemar's test matches the textbook formula", {
  dx_glm <- dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = hf, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  correct1 <- (hf$predicted >= .3) == hf$truth
  correct2 <- (hf$predicted_rf >= .3) == hf$truth
  b <- sum(correct1 & !correct2)
  c <- sum(!correct1 & correct2)

  # McNemar with continuity correction: (|b - c| - 1)^2 / (b + c)
  stat <- (abs(b - c) - 1)^2 / (b + c)
  res <- dx_mcnemars(dx_glm, dx_rf)

  expect_equal(res$statistic, stat)
  expect_equal(res$p_value, stats::pchisq(stat, 1, lower.tail = FALSE))
})


# AUC and DeLong ----------------------------------------------------------

# DeLong, DeLong & Clarke-Pearson (1988) structural components
delong_parts <- function(score, truth) {
  pos <- score[truth == 1]
  neg <- score[truth == 0]
  psi <- outer(pos, neg, ">") + .5 * outer(pos, neg, "==")
  list(auc = mean(psi), v10 = rowMeans(psi), v01 = colMeans(psi), m = length(pos), n = length(neg))
}

test_that("AUC and its CI match the Mann-Whitney statistic and DeLong variance", {
  p <- delong_parts(hf$predicted, hf$truth)
  se <- sqrt(stats::var(p$v10) / p$m + stats::var(p$v01) / p$n)
  ci <- p$auc + c(-1, 1) * stats::qnorm(.975) * se

  res <- dx_auc(hf$truth, hf$predicted)
  expect_equal(res$estimate, p$auc)
  expect_equal(c(res$conf_low, res$conf_high), ci)

  res <- overall(dx_obj, "AUC ROC")
  expect_equal(res$estimate, p$auc)
  expect_equal(c(res$conf_low, res$conf_high), ci)
})

test_that("Paired DeLong test matches the DeLong (1988) covariance", {
  dx_glm <- dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = hf, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  p1 <- delong_parts(hf$predicted, hf$truth)
  p2 <- delong_parts(hf$predicted_rf, hf$truth)
  diff <- p1$auc - p2$auc
  se <- sqrt(stats::var(p1$v10 - p2$v10) / p1$m + stats::var(p1$v01 - p2$v01) / p1$n)
  z <- diff / se

  res <- dx_delong(dx_glm, dx_rf, paired = TRUE)
  expect_equal(res$estimate, diff)
  expect_equal(abs(res$statistic), abs(z))
  expect_equal(res$p_value, 2 * stats::pnorm(-abs(z)))
  expect_equal(c(res$conf_low, res$conf_high), diff + c(-1, 1) * stats::qnorm(.975) * se)
})

test_that("Unpaired DeLong test matches independent variances", {
  dx_glm <- dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = hf, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  p1 <- delong_parts(hf$predicted, hf$truth)
  p2 <- delong_parts(hf$predicted_rf, hf$truth)
  var1 <- stats::var(p1$v10) / p1$m + stats::var(p1$v01) / p1$n
  var2 <- stats::var(p2$v10) / p2$m + stats::var(p2$v01) / p2$n
  stat <- (p1$auc - p2$auc) / sqrt(var1 + var2)

  # pROC compares the statistic to a t distribution with Welch-Satterthwaite
  # degrees of freedom rather than a normal distribution
  n1 <- p1$m + p1$n
  n2 <- p2$m + p2$n
  df <- (var1 + var2)^2 / (var1^2 / (n1 - 1) + var2^2 / (n2 - 1))

  res <- suppressWarnings(dx_delong(dx_glm, dx_rf, paired = FALSE))
  expect_equal(res$estimate, p1$auc - p2$auc)
  expect_equal(res$statistic, stat)
  expect_equal(res$p_value, 2 * stats::pt(-abs(stat), df = df))
})


# Breslow-Day -------------------------------------------------------------

# Independent implementation: the expected count in each stratum makes the
# stratum odds ratio equal the Mantel-Haenszel common odds ratio, given fixed
# margins (Breslow & Day, 1980). Solved with uniroot rather than the quadratic.
bd_reference <- function(x) {
  or_mh <- stats::mantelhaen.test(x)$estimate
  stat <- 0
  for (j in seq_len(dim(x)[3])) {
    t <- x[, , j]
    m1 <- sum(t[1, ])
    m2 <- sum(t[2, ])
    n1 <- sum(t[, 1])
    f <- function(a) a * (m2 - n1 + a) - or_mh * (m1 - a) * (n1 - a)
    a_t <- stats::uniroot(f, c(max(0, n1 - m2), min(n1, m1)), tol = 1e-12)$root
    v <- 1 / (1 / a_t + 1 / (m1 - a_t) + 1 / (n1 - a_t) + 1 / (m2 - n1 + a_t))
    stat <- stat + (t[1, 1] - a_t)^2 / v
  }
  unname(stat)
}

test_that("Breslow-Day matches the published UCB admissions example", {
  # Agresti, Categorical Data Analysis: X-squared = 18.83, df = 5, p = 0.002
  res <- breslow_day_test(UCBAdmissions)
  stat <- unname(res$statistic)

  expect_equal(stat, 18.83, tolerance = 0.005 / 18.83)
  expect_equal(unname(res$p_value), 0.002, tolerance = 0.0005 / 0.002)
  expect_equal(stat, bd_reference(UCBAdmissions), tolerance = 1e-8)
})

test_that("Breslow-Day in dx() matches an independent implementation", {
  pred <- as.numeric(hf$predicted >= .3)
  bd <- dx_obj$measures[dx_obj$measures$measure == "Breslow-Day", ]

  for (var in c("AgeGroup", "Sex", "AgeSex")) {
    stat <- bd_reference(table(pred, hf$truth, hf[[var]]))
    k <- nlevels(hf[[var]])

    expect_equal(
      bd$estimate[bd$variable == var],
      stats::pchisq(stat, k - 1, lower.tail = FALSE),
      tolerance = 1e-8,
      info = var
    )
  }
})
