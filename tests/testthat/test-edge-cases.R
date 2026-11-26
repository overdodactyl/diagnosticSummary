# Edge cases where the current behavior is intended.

test_that("Undefined proportions are NaN and display as missing", {
  # Nothing predicted positive, so PPV and FDR are 0/0
  cm <- make_cm(tp = 0, fn = 10, fp = 0, tn = 40)

  for (f in list(dx_ppv, dx_fdr)) {
    res <- f(cm)
    expect_true(is.nan(res$estimate), info = res$measure)
    expect_match(res$summary, "^- ", info = res$measure)
  }

  # Nothing predicted negative, so NPV is 0/0
  expect_true(is.nan(dx_npv(make_cm(tp = 10, fn = 0, fp = 40, tn = 0), "simple")))
})

test_that("F1, MCC, and kappa are 0 when undefined", {
  # Common convention, e.g. Chicco & Jurman (2020) for MCC
  cm <- make_cm(tp = 0, fn = 10, fp = 0, tn = 40)

  expect_equal(dx_f1(cm, "simple"), 0)
  expect_equal(dx_mcc(cm, "simple"), 0)
  expect_equal(dx_cohens_kappa(cm, "simple"), 0)
})

test_that("Ratios use a 0.5 continuity correction for zero cells", {
  # Perfect classifier: fn = fp = 0, so 0.5 is added to every cell
  res <- dx_odds_ratio(make_cm(tp = 30, fn = 0, fp = 0, tn = 70))

  expect_equal(res$estimate, (30.5 * 70.5) / (.5 * .5))
  expect_equal(res$conf_type, "Adjusted for zero cells")
})

test_that("Perfect agreement and disagreement give kappa of 1 and -1", {
  expect_equal(dx_cohens_kappa(make_cm(tp = 30, fn = 0, fp = 0, tn = 70), "simple"), 1)
  expect_equal(dx_cohens_kappa(make_cm(tp = 0, fn = 5, fp = 5, tn = 0), "simple"), -1)
})

test_that("Predictions equal to the threshold are positive", {
  cm <- dx_cm(c(.2, .5, .5, .8), c(0, 0, 1, 1), threshold = .5, poslabel = 1)

  expect_equal(cm$tp, 2)
  expect_equal(cm$fp, 1)
  expect_equal(cm$tn, 1)
})

test_that("poslabel = 0 swaps the positive class", {
  fit <- function(poslabel) {
    d <- dx(
      data = dx_heart_failure,
      true_varname = "truth",
      pred_varname = "predicted",
      setthreshold = .3,
      poslabel = poslabel
    )
    m <- d$measures
    setNames(m$estimate, m$measure)
  }
  pos1 <- fit(1)
  pos0 <- fit(0)

  expect_equal(pos0[["Sensitivity"]], pos1[["Specificity"]])
  expect_equal(pos0[["Specificity"]], pos1[["Sensitivity"]])
  expect_equal(pos0[["Prevalence"]], 1 - pos1[["Prevalence"]])
  expect_equal(pos0[["Accuracy"]], pos1[["Accuracy"]])
})

test_that("Tibble input gives the same results as a data frame", {
  skip_if_not_installed("tibble")

  fit <- function(data) {
    dx(data = data, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)$measures
  }

  expect_equal(fit(tibble::as_tibble(dx_heart_failure)), fit(dx_heart_failure))
})

test_that("setthreshold is reported even when not in threshold_range", {
  d <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    threshold_range = c(.1, .2)
  )

  expect_setequal(unique(d$measures$threshold), c(.1, .2, .3))
  expect_equal(sum(d$measures$measure == "Sensitivity"), 3)
})

test_that("Breslow-Day handles a zero cell in one subgroup", {
  hf <- dx_heart_failure
  # Remove false positives aged (20,50], leaving a zero cell in that subgroup
  hf <- hf[!(hf$predicted >= .3 & hf$truth == 0 & hf$AgeGroup == "(20,50]"), ]

  d <- dx(
    data = hf,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "AgeGroup"
  )
  bd <- d$measures[d$measures$measure == "Breslow-Day", ]

  # Agreed to 10 decimals across this package, DescTools::BreslowDayTest,
  # and an independent uniroot implementation (X-squared = 11.2497)
  expect_equal(bd$estimate, 0.0007963668, tolerance = 1e-6)
})

test_that("dx_compare names unnamed models and checks its inputs", {
  dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  expect_equal(unique(dx_compare(list(dx_glm, dx_rf))$tests$models), "Model 1 vs. Model 2")
  expect_error(dx_compare(list(a = dx_glm)), "two or more")

  dx_sub <- dx(data = dx_heart_failure[1:200, ], true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  expect_error(dx_compare(list(a = dx_glm, b = dx_sub), paired = TRUE), "paired")
})

test_that("Ratios are NA when every prediction is on one side of the threshold", {
  all_negative <- make_cm(tp = 0, fn = 20, fp = 0, tn = 30)
  all_positive <- make_cm(tp = 20, fn = 0, fp = 30, tn = 0)

  for (cm in list(all_negative, all_positive)) {
    for (ratio in list(dx_odds_ratio, dx_lrt_pos, dx_lrt_neg)) {
      expect_true(is.na(ratio(cm, detail = "simple")))
      res <- ratio(cm)
      expect_true(is.na(res$estimate))
      expect_equal(res$summary, "- (-, -)")
      expect_equal(res$notes, "Requires both positive and negative predictions")
    }
  }

  # A zero cell with predictions on both sides still uses the correction
  res <- dx_lrt_pos(make_cm(tp = 5, fn = 0, fp = 3, tn = 7))
  expect_equal(res$estimate, (5.5 / 6) / (3.5 / 11))
  expect_equal(res$conf_type, "Adjusted for zero cells")
})

test_that("dx() reports NA ratios at thresholds with no positive or no negative predictions", {
  fit <- dx(
    data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted",
    setthreshold = .5, threshold_range = c(0, 1)
  )
  expect_equal(dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted",
                  setthreshold = 1)$cm$testpos, 0)

  ratios <- fit$measures[fit$measures$measure %in% c("Odds Ratio", "LRT+", "LRT-"), ]
  expect_true(all(is.na(ratios$estimate[ratios$threshold %in% c(0, 1)])))
  expect_false(anyNA(ratios$estimate[ratios$threshold == .5]))
})
