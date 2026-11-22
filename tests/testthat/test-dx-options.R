# Options of dx() and the methods that summarize its output.

hf <- dx_heart_failure

fit_dx <- function(...) {
  dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3, ...)
}


test_that("prevalence adds predictive values at the target prevalence", {
  d <- fit_dx(prevalence = .1, grouping_variables = "Sex")
  m <- d$measures

  for (label in c("Overall", "Female", "Male")) {
    rows <- if (label == "Overall") rep(TRUE, nrow(hf)) else hf$Sex == label
    cm <- dx_cm(hf$predicted[rows], hf$truth[rows], threshold = .3, poslabel = 1)
    sub <- m[m$label == label, ]

    expect_equal(
      sub$estimate[sub$measure == "Positive Predictive Value (Target Prevalence)"],
      dx_ppv_prevalence(cm, prevalence = .1, detail = "simple"),
      info = label
    )
    expect_equal(
      sub$estimate[sub$measure == "Negative Predictive Value (Target Prevalence)"],
      dx_npv_prevalence(cm, prevalence = .1, detail = "simple"),
      info = label
    )
  }

  # Not reported unless requested
  expect_false(any(grepl("Target Prevalence", fit_dx()$measures$measure)))
})

test_that("Bootstrapped CIs are reported for the bootstrapped measures", {
  d <- fit_dx(doboot = TRUE, bootreps = 200)
  m <- d$measures

  boot_measures <- c(
    "F1 Score", "F2 Score", "Matthews Correlation Coefficient", "Balanced Accuracy",
    "Informedness", "Markedness", "G-mean", "Fowlkes-Mallows Index"
  )
  boot <- m[m$measure %in% boot_measures, ]

  expect_equal(nrow(boot), length(boot_measures))
  expect_true(all(boot$conf_type == "Bootstrapped"))
  expect_true(all(boot$conf_low <= boot$estimate & boot$estimate <= boot$conf_high))

  # Without doboot, these measures have no CI
  m <- fit_dx()$measures
  expect_true(all(is.na(m$conf_low[m$measure %in% boot_measures])))
})

test_that("threshold_range adds overall results at each threshold", {
  d <- fit_dx(threshold_range = c(.1, .5), grouping_variables = "Sex")
  m <- d$measures

  for (t in c(.1, .3, .5)) {
    cm <- dx_cm(hf$predicted, hf$truth, threshold = t, poslabel = 1)
    sens <- m$estimate[m$variable == "Overall" & m$measure == "Sensitivity" & m$threshold == t]
    expect_equal(sens, cm$tp / cm$dispos, info = t)
  }

  # Subgroups are only reported at setthreshold
  expect_true(all(m$threshold[m$variable == "Sex"] == .3))
})

test_that("citype is used for binomial CIs", {
  for (method in c("ac", "asymptotic", "logit", "cloglog", "probit")) {
    m <- fit_dx(citype = method)$measures
    sens <- m[m$variable == "Overall" & m$measure == "Sensitivity", ]
    ref <- binom::binom.confint(sum(hf$predicted >= .3 & hf$truth == 1), sum(hf$truth == 1), methods = method)

    expect_equal(sens$conf_low, ref$lower, info = method)
    expect_equal(sens$conf_high, ref$upper, info = method)
    expect_equal(sens$conf_type, paste0("Binomial: ", method), info = method)
  }
})

test_that("summary shows the requested rows and columns", {
  skip_if_not_installed("knitr")
  d <- fit_dx(threshold_range = c(.1, .5), grouping_variables = "Sex")

  res <- summary(d, variable = "Sex", measure = c("Sensitivity", "Specificity"))
  text <- paste(res, collapse = "\n")
  expect_match(text, "Threshold= 0.3")
  expect_match(text, "variable")
  expect_match(text, "label")
  # Two subgroups x two measures, plus the header and separator lines
  expect_equal(sum(grepl("^\\|", res)), 2 + 4)

  res <- summary(d, thresh = .5, show_var = FALSE, show_label = FALSE)
  text <- paste(res, collapse = "\n")
  expect_match(text, "Threshold= 0.5")
  expect_false(grepl("variable", text))
  expect_false(grepl("\\| *label", text))

  expect_error(summary(d, thresh = c(.1, .5)), "numeric value to thresh")
})
