# Data with known 3x2 cell counts: positive, negative, and ungradable (NA)
# predictions among cases with outcome 1, then outcome 0
make_itd_data <- function(tp, fn, up, fp, tn, un) {
  data.frame(
    truth = rep(c(1, 1, 1, 0, 0, 0), times = c(tp, fn, up, fp, tn, un)),
    predicted = rep(c(.9, .1, NA, .9, .1, NA), times = c(tp, fn, up, fp, tn, un))
  )
}

itd_estimate <- function(fit, name) {
  m <- fit$measures
  m$estimate[m$measure == name & m$variable == "Overall"]
}

itd_row <- function(fit, name) {
  m <- fit$measures
  m[m$measure == name & m$variable == "Overall", ]
}

test_that("dx_itd counts and estimates match hand calculations", {
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")

  expect_s3_class(fit, "dx_itd")
  cm <- fit$cm
  expect_equal(
    unlist(cm[c("tp", "fn", "ug_pos", "fp", "tn", "ug_neg")]),
    c(tp = 40, fn = 10, ug_pos = 5, fp = 15, tn = 60, ug_neg = 10)
  )
  expect_equal(unlist(cm[c("dispos", "disneg", "n", "testug")]),
               c(dispos = 55, disneg = 85, n = 140, testug = 15))

  expected <- c(
    "Diagnostic Yield (Disease Positive)" = 50 / 55,
    "Diagnostic Yield (Disease Negative)" = 75 / 85,
    "Diagnostic Yield (Overall)" = 125 / 140,
    "Pre-Test Disease Risk" = 55 / 140,
    "Positive Predictive Value" = 40 / 55,
    "Negative Predictive Value" = 60 / 70,
    "Post-Test Disease Risk (Positive)" = 40 / 55,
    "Post-Test Disease Risk (Negative)" = 10 / 70,
    "Post-Test Disease Risk (Ungradable)" = 5 / 15,
    "Likelihood Ratio (Positive)" = (40 / 55) / (15 / 85),
    "Likelihood Ratio (Negative)" = (10 / 55) / (60 / 85),
    "Likelihood Ratio (Ungradable)" = (5 / 55) / (10 / 85),
    "Worst-Case Sensitivity" = 40 / 55,
    "Worst-Case Specificity" = 60 / 85,
    "Worst-Case Youden's J" = 40 / 55 + 60 / 85 - 1
  )
  expect_equal(fit$measures$measure, names(expected))
  for (name in names(expected)) {
    expect_equal(itd_estimate(fit, name), expected[[name]], label = name)
  }
})

test_that("dx_itd confidence intervals match their formulas", {
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")

  # Exact binomial
  ci <- binom::binom.confint(5, 15, methods = "exact")
  row <- itd_row(fit, "Post-Test Disease Risk (Ungradable)")
  expect_equal(c(row$conf_low, row$conf_high), c(ci$lower, ci$upper))
  expect_equal(row$fraction, "5/15")

  # Log scale likelihood ratio
  row <- itd_row(fit, "Likelihood Ratio (Ungradable)")
  se <- sqrt(1 / 5 - 1 / 55 + 1 / 10 - 1 / 85)
  ratio <- (5 / 55) / (10 / 85)
  expect_equal(row$conf_low, exp(log(ratio) - qnorm(.975) * se))
  expect_equal(row$conf_high, exp(log(ratio) + qnorm(.975) * se))
  expect_equal(row$conf_type, "Large sample")

  # Wald interval for Youden's J
  sens <- 40 / 55
  spec <- 60 / 85
  se <- sqrt(sens * (1 - sens) / 55 + spec * (1 - spec) / 85)
  row <- itd_row(fit, "Worst-Case Youden's J")
  expect_equal(row$conf_low, sens + spec - 1 - qnorm(.975) * se)
  expect_equal(row$conf_high, sens + spec - 1 + qnorm(.975) * se)

  # citype is passed to every proportion
  wilson <- dx_itd(data, pred_varname = "predicted", true_varname = "truth", citype = "wilson")
  row <- itd_row(wilson, "Diagnostic Yield (Overall)")
  ci <- binom::binom.confint(125, 140, methods = "wilson")
  expect_equal(c(row$conf_low, row$conf_high), c(ci$lower, ci$upper))
  expect_equal(itd_row(wilson, "Positive Predictive Value")$conf_type, "Binomial: wilson")
})

test_that("dx_itd agrees with dx() when no predictions are missing", {
  fit <- dx_itd(dx_heart_failure, pred_varname = "predicted", true_varname = "truth")
  full <- dx(dx_heart_failure, pred_varname = "predicted", true_varname = "truth")
  dx_estimate <- function(name) {
    full$measures$estimate[full$measures$measure == name]
  }

  expect_equal(itd_estimate(fit, "Diagnostic Yield (Overall)"), 1)
  expect_equal(itd_estimate(fit, "Worst-Case Sensitivity"), dx_estimate("Sensitivity"))
  expect_equal(itd_estimate(fit, "Worst-Case Specificity"), dx_estimate("Specificity"))
  expect_equal(itd_estimate(fit, "Positive Predictive Value"), dx_estimate("Positive Predictive Value"))
  expect_equal(itd_estimate(fit, "Negative Predictive Value"), dx_estimate("Negative Predictive Value"))
  expect_equal(itd_estimate(fit, "Likelihood Ratio (Positive)"), dx_estimate("LRT+"))
  expect_equal(itd_estimate(fit, "Likelihood Ratio (Negative)"), dx_estimate("LRT-"))
  expect_true(is.nan(itd_estimate(fit, "Post-Test Disease Risk (Ungradable)")))
  expect_true(is.na(itd_estimate(fit, "Likelihood Ratio (Ungradable)")))
})

test_that("dx_itd identities hold with missing predictions", {
  data <- dx_heart_failure
  data$predicted[seq(1, nrow(data), by = 7)] <- NA
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth", setthreshold = .3)

  expect_equal(
    itd_estimate(fit, "Post-Test Disease Risk (Positive)"),
    itd_estimate(fit, "Positive Predictive Value")
  )
  expect_equal(
    itd_estimate(fit, "Post-Test Disease Risk (Negative)"),
    1 - itd_estimate(fit, "Negative Predictive Value")
  )
  expect_equal(itd_estimate(fit, "Diagnostic Yield (Overall)"), 1 - fit$cm$testug / fit$cm$n)
  expect_equal(fit$cm$testug, sum(is.na(data$predicted)))

  # Gradable cases match dx() on the complete cases
  complete <- dx(data[!is.na(data$predicted), ], pred_varname = "predicted",
                 true_varname = "truth", setthreshold = .3)
  expect_equal(unlist(fit$cm[c("tp", "fn", "fp", "tn")]),
               unlist(complete$cm[c("tp", "fn", "fp", "tn")]))
})

test_that("dx_itd uses poslabel and setthreshold like dx_cm()", {
  data <- data.frame(
    truth = c(1, 1, 1, 1, 0, 0, 0, 0, 0),
    predicted = c(.8, .6, .4, NA, .7, .3, .2, NA, NA)
  )

  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth", setthreshold = .6)
  expect_equal(unlist(fit$cm[c("tp", "fn", "ug_pos", "fp", "tn", "ug_neg")]),
               c(tp = 2, fn = 1, ug_pos = 1, fp = 1, tn = 2, ug_neg = 2))

  # With poslabel = 0, outcome 0 is the event and low predictions are positive
  fit0 <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                 setthreshold = .6, poslabel = 0)
  expect_equal(unlist(fit0$cm[c("tp", "fn", "ug_pos", "fp", "tn", "ug_neg")]),
               c(tp = 2, fn = 1, ug_pos = 2, fp = 1, tn = 2, ug_neg = 1))
  expect_equal(itd_estimate(fit0, "Pre-Test Disease Risk"), 5 / 9)
})

test_that("dx_itd accepts 0/1 class predictions", {
  data <- make_itd_data(tp = 4, fn = 1, up = 1, fp = 1, tn = 4, un = 1)
  data$predicted <- round(data$predicted)
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
  expect_equal(fit$cm$tp, 4)

  # Only one distinct gradable prediction is allowed
  data$predicted[!is.na(data$predicted)] <- 1
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
  expect_equal(fit$cm$testpos, 10)
})

test_that("dx_itd likelihood ratios add 0.5 to zero cells", {
  data <- make_itd_data(tp = 40, fn = 10, up = 0, fp = 15, tn = 60, un = 10)
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")

  row <- itd_row(fit, "Likelihood Ratio (Ungradable)")
  expect_equal(row$estimate, (0.5 / 51) / (10.5 / 86))
  expect_equal(row$conf_type, "Adjusted for zero cells")
  expect_true(is.finite(row$conf_low) && is.finite(row$conf_high))

  # Other rows have no zero cells and are not adjusted
  expect_equal(itd_row(fit, "Likelihood Ratio (Positive)")$conf_type, "Large sample")
  expect_equal(itd_estimate(fit, "Post-Test Disease Risk (Ungradable)"), 0)
})

test_that("dx_itd summarizes subgroups", {
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  data$site <- rep(c("a", "b"), length.out = nrow(data))
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                grouping_variables = "site")

  expect_equal(unique(fit$measures$label), c("a", "b", "Overall"))
  expect_equal(unique(fit$measures$variable), c("site", "Overall"))

  # Each subgroup matches dx_itd() run on that subgroup alone
  for (g in c("a", "b")) {
    sub <- fit$measures[fit$measures$label == g, ]
    alone <- dx_itd(data[data$site == g, ], pred_varname = "predicted",
                    true_varname = "truth")$measures
    cols <- c("measure", "summary", "estimate", "conf_low", "conf_high", "fraction", "notes", "n")
    expect_equal(plain_df(sub[cols]), plain_df(alone[cols]), info = g)
    expect_equal(sub$n[1], sum(data$site == g))
  }

  # Rows with a missing group are left out of the subgroups but not Overall
  data$site[1:3] <- NA
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                grouping_variables = "site")
  n <- unique(fit$measures[c("label", "n")])
  expect_equal(n$n[n$label != "Overall"], c(sum(data$site %in% "a"), sum(data$site %in% "b")))
  expect_equal(n$n[n$label == "Overall"], nrow(data))

  # Subgroups with one outcome class give a warning and NA likelihood ratios
  data$site <- ifelse(data$truth == 1 & seq_len(nrow(data)) <= 5, "c", "d")
  expect_warning(
    fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                  grouping_variables = "site"),
    "only one outcome class: c\\. Measures that need both outcome classes are undefined"
  )
  sub <- fit$measures[fit$measures$label == "c", ]
  expect_true(is.na(sub$estimate[sub$measure == "Likelihood Ratio (Positive)"]))
})

test_that("dx_itd validates its inputs", {
  data <- make_itd_data(tp = 4, fn = 1, up = 1, fp = 1, tn = 4, un = 1)

  na_truth <- data
  na_truth$truth[1] <- NA
  expect_error(dx_itd(na_truth, "predicted", "truth"),
               "truth contains 1 missing value.*dx_itd\\(\\)")

  all_na <- data
  all_na$predicted <- NA_real_
  expect_error(dx_itd(all_na, "predicted", "truth"), "All values of predicted are missing")

  logits <- data
  logits$predicted[1] <- 3
  expect_error(dx_itd(logits, "predicted", "truth"), "between 0 and 1")

  one_class <- data[data$truth == 1, ]
  expect_error(dx_itd(one_class, "predicted", "truth"), "should contain both 0's and 1's")

  expect_error(dx_itd(data, "predicted", "truth", poslabel = 2), "poslabel should be 0 or 1")

  for (bad in list(NA, c(.5, .6), "0.5", -0.1, 1.1)) {
    expect_error(dx_itd(data, "predicted", "truth", setthreshold = bad),
                 "setthreshold should be a single number between 0 and 1")
    expect_error(dx(dx_heart_failure, pred_varname = "predicted", true_varname = "truth",
                    setthreshold = bad),
                 "setthreshold should be a single number between 0 and 1")
  }
  expect_s3_class(dx_itd(data, "predicted", "truth", setthreshold = 1), "dx_itd")
})

test_that("dx() points to dx_itd() for missing predictions", {
  data <- dx_heart_failure
  data$predicted[1:3] <- NA
  expect_error(
    dx(data, pred_varname = "predicted", true_varname = "truth"),
    "predicted contains 3 missing value.*use dx_itd\\(\\)"
  )
})

test_that("dx_itd_table lays out the 3x2 table", {
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  data$site <- rep(c("a", "b"), length.out = nrow(data))
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                grouping_variables = "site")
  tab <- dx_itd_table(fit)

  expect_equal(names(tab), c(
    "row", "disease_positive", "disease_negative", "total",
    "predictive_value", "post_test_risk", "likelihood_ratio"
  ))
  expect_equal(tab$row, c(
    "Positive", "Negative", "Ungradable", "Total",
    "Diagnostic yield", "Worst-case performance"
  ))
  expect_equal(tab$disease_positive[1:4], c("40", "10", "5", "55"))
  expect_equal(tab$disease_negative[1:4], c("15", "60", "10", "85"))
  expect_equal(tab$total[1:4], c("55", "70", "15", "140"))

  m <- fit$measures[fit$measures$variable == "Overall", ]
  summary_of <- function(name) m$summary[m$measure == name]
  expect_equal(tab$disease_positive[5], summary_of("Diagnostic Yield (Disease Positive)"))
  expect_equal(tab$post_test_risk[3], summary_of("Post-Test Disease Risk (Ungradable)"))
  expect_equal(tab$likelihood_ratio[3], summary_of("Likelihood Ratio (Ungradable)"))
  expect_equal(tab$predictive_value[4],
               paste("Pre-test disease risk =", summary_of("Pre-Test Disease Risk")))
  expect_equal(tab$total[6], paste("Youden's J =", summary_of("Worst-Case Youden's J")))

  no_ci <- dx_itd_table(fit, show_ci = FALSE)
  expect_equal(no_ci$post_test_risk[3], "33.3%")
  expect_equal(no_ci$likelihood_ratio[3], "0.77")

  # Subgroup tables use the subgroup's counts
  sub <- dx_itd_table(fit, variable = "site", label = "a")
  expect_equal(sub$total[4], as.character(sum(data$site == "a")))

  expect_error(dx_itd_table(fit, variable = "site", label = "z"), "No results")
  expect_error(dx_itd_table(list()), "dx_itd object")
})

test_that("summary and as.data.frame methods filter dx_itd results", {
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")

  df <- as.data.frame(fit, measure = c("Worst-Case Sensitivity", "Worst-Case Specificity"))
  expect_equal(nrow(df), 2)

  s <- summary(fit, measure = "Diagnostic Yield (Overall)", show_var = FALSE)
  expect_true(any(grepl("Diagnostic Yield \\(Overall\\)", s)))
})

test_that("dx_itd likelihood ratios are NA for results no case has", {
  # No ungradable results
  fit <- dx_itd(make_itd_data(tp = 40, fn = 10, up = 0, fp = 15, tn = 60, un = 0),
                pred_varname = "predicted", true_varname = "truth")
  row <- itd_row(fit, "Likelihood Ratio (Ungradable)")
  expect_true(is.na(row$estimate))
  expect_equal(row$notes, "No cases with this result")
  expect_true(is.finite(itd_estimate(fit, "Likelihood Ratio (Positive)")))

  # No negative results
  fit <- dx_itd(make_itd_data(tp = 5, fn = 0, up = 2, fp = 5, tn = 0, un = 2),
                pred_varname = "predicted", true_varname = "truth")
  expect_true(is.na(itd_estimate(fit, "Likelihood Ratio (Negative)")))
  expect_equal(itd_estimate(fit, "Likelihood Ratio (Positive)"), 1)

  # A subgroup where every prediction is missing
  data <- make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10)
  data$site <- ifelse(is.na(data$predicted), "missing", "gradable")
  expect_warning(
    fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth",
                  grouping_variables = "site"),
    "with all predictions missing: missing\\."
  )
  # dx_itd() on that subgroup alone is an error
  expect_error(dx_itd(data[data$site == "missing", ], "predicted", "truth"),
               "All values of predicted are missing")
  sub <- fit$measures[fit$measures$label == "missing", ]
  lr <- sub[sub$measure %in% c("Likelihood Ratio (Positive)", "Likelihood Ratio (Negative)"), ]
  expect_true(all(is.na(lr$estimate)))
  expect_equal(sub$estimate[sub$measure == "Diagnostic Yield (Overall)"], 0)
  expect_equal(sub$estimate[sub$measure == "Worst-Case Youden's J"], -1)
})

test_that("dx_itd notes when the Youden's J interval is too narrow", {
  # Perfect worst-case sensitivity and specificity: no width
  fit <- dx_itd(make_itd_data(tp = 50, fn = 0, up = 0, fp = 0, tn = 80, un = 0),
                pred_varname = "predicted", true_varname = "truth")
  row <- itd_row(fit, "Worst-Case Youden's J")
  expect_equal(c(row$estimate, row$conf_low, row$conf_high), c(1, 1, 1))
  expect_match(row$notes, "too narrow")

  # Only sensitivity at 100%
  fit <- dx_itd(make_itd_data(tp = 50, fn = 0, up = 0, fp = 10, tn = 70, un = 0),
                pred_varname = "predicted", true_varname = "truth")
  row <- itd_row(fit, "Worst-Case Youden's J")
  expect_true(row$conf_low < row$conf_high)
  expect_match(row$notes, "too narrow")

  # No note otherwise
  fit <- dx_itd(make_itd_data(tp = 40, fn = 10, up = 5, fp = 15, tn = 60, un = 10),
                pred_varname = "predicted", true_varname = "truth")
  expect_equal(itd_row(fit, "Worst-Case Youden's J")$notes, "")
})
