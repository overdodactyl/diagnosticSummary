dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  setthreshold = .3
)

# ggplot2 only warns about layer data when a plot is built, so build each plot
# instead of only creating it
expect_builds <- function(p) {
  expect_no_warning(b <- ggplot2::ggplot_build(p))
  invisible(b)
}


# Every plot builds without errors or warnings ----------------------------

test_that("All plots build without warnings", {
  dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  expect_builds(dx_plot_gain(dx_obj))
  expect_builds(dx_plot_lift(dx_obj))
  expect_builds(dx_plot_ks(dx_obj))
  expect_builds(dx_plot_pr(dx_obj))
  expect_builds(dx_plot_predictive_value(dx_obj))
  expect_builds(dx_plot_calibration(dx_obj))
  expect_builds(dx_plot_decision_curve(dx_obj))
  expect_builds(dx_plot_youden_j(dx_obj))
  expect_builds(dx_plot_cap(dx_obj))
  expect_builds(dx_plot_cost(dx_obj, cfp = 1000, cfn = 8000))
  expect_builds(dx_plot_cm(dx_obj))
  expect_builds(dx_plot_roc(dx_obj))
  expect_builds(dx_plot_probabilities(dx_obj, bins = 20))
  expect_builds(dx_plot_probabilities(dx_obj, plot_type = "density"))
  expect_builds(dx_plot_thresholds(dx_obj))
  expect_builds(dx_plot_rocs(dx_compare(list(glm = dx_obj, rf = dx_rf))))
})

test_that("Forest plot builds without warnings", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  dx_grp <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    outcome_label = "Heart Attack",
    setthreshold = .3,
    grouping_variables = c("AgeGroup", "Sex")
  )

  # Surface soft deprecations (e.g. aes_()) that would otherwise stay hidden
  op <- options(lifecycle_verbosity = "warning")
  on.exit(options(op))

  expect_no_warning(dx_plot_forest(dx_grp))
  expect_no_warning(dx_plot_forest(dx_grp, trans = "log10", fraction = TRUE))
})


# Data behind the plots ---------------------------------------------------

test_that("ROC plot shades the AUC and marks the chosen threshold", {
  b <- expect_builds(dx_plot_roc(dx_obj))
  cm <- dx_obj$cm

  # The shaded rectangles under the curve add up to the AUC
  rects <- b$data[[1]]
  area <- sum(abs(rects$xmax - rects$xmin) * rects$ymax, na.rm = TRUE)
  expect_equal(area, as.numeric(dx_obj$roc$auc))

  # The reference circle sits at the sensitivity and specificity at setthreshold.
  # The x axis is reversed, so built x values are negated.
  circle <- b$data[[length(b$data) - 1]]
  expect_equal(-circle$x, cm$tn / cm$disneg)
  expect_equal(circle$y, cm$tp / cm$dispos)
})

test_that("Confusion matrix plot shows counts in the right cells", {
  b <- expect_builds(dx_plot_cm(dx_obj))
  counts <- b$data[[2]]
  cm <- dx_obj$cm

  # x is truth (1 = negative, 2 = positive), y is the prediction
  cell <- function(x, y) as.numeric(counts$label[counts$x == x & counts$y == y])
  expect_equal(cell(1, 1), cm$tn)
  expect_equal(cell(2, 1), cm$fn)
  expect_equal(cell(1, 2), cm$fp)
  expect_equal(cell(2, 2), cm$tp)
})

test_that("Calibration plot uses mean prediction and observed rate per bin", {
  pred <- dx_heart_failure$predicted
  truth <- dx_heart_failure$truth

  for (bins in c(5, 10)) {
    b <- expect_builds(dx_plot_calibration(dx_obj, bins = bins))
    line <- b$data[[1]]

    bin <- cut(pred, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
    expected_x <- as.numeric(tapply(pred, bin, mean))
    expected_y <- as.numeric(tapply(truth, bin, mean))
    keep <- !is.na(expected_x)

    expect_equal(line$x, expected_x[keep], info = bins)
    expect_equal(line$y, expected_y[keep], info = bins)
  }
})

test_that("Decision curve compares the model with treat all and treat none", {
  b <- expect_builds(dx_plot_decision_curve(dx_obj, plot_range = c(.1, .6)))
  prev <- mean(dx_heart_failure$truth)
  n <- nrow(dx_heart_failure)

  model <- b$data[[1]]
  treat_all <- b$data[[2]]
  treat_none <- b$data[[3]]

  expect_true(all(model$x >= .1 & model$x <= .6))

  thr <- dx_obj$thresholds
  thr <- thr[match(model$x, thr$threshold), ]
  weight <- model$x / (1 - model$x)
  expect_equal(model$y, thr$tp / n - thr$fp / n * weight)
  expect_equal(treat_all$y, prev - (1 - prev) * weight)
  expect_true(all(treat_none$y == 0))
})

test_that("Cost curve totals costs at the extremes", {
  cm <- dx_obj$cm
  b <- expect_builds(dx_plot_cost(dx_obj, cfp = 2, cfn = 7, ctp = 1, ctn = 3))
  cost <- b$data[[1]]

  # Threshold 0: everyone is called positive
  expect_equal(cost$y[cost$x == 0], 2 * cm$disneg - 1 * cm$dispos)
  # Threshold 1: no one is called positive
  expect_equal(cost$y[cost$x == 1], 7 * cm$dispos - 3 * cm$disneg)
})

test_that("Predictive value plot matches Bayes' theorem", {
  b <- expect_builds(dx_plot_predictive_value(dx_obj))
  lines <- b$data[[1]]
  cm <- dx_obj$cm

  prev <- dx_obj$prevalence$prevalence
  values <- split(lines$y, lines$group)
  ppv <- dx_obj$prevalence$ppv
  npv <- dx_obj$prevalence$npv

  for (p in c(.05, .5, .9)) {
    i <- which.min(abs(prev - p))
    expect_equal(ppv[i], dx_ppv_prevalence(cm, prevalence = prev[i], detail = "simple"), info = p)
    expect_equal(npv[i], dx_npv_prevalence(cm, prevalence = prev[i], detail = "simple"), info = p)
  }
  expect_setequal(unlist(values), c(ppv, npv))
})

test_that("Gain, lift, and CAP curves reach their end points", {
  prev <- mean(dx_heart_failure$truth)

  gain <- expect_builds(dx_plot_gain(dx_obj))$data[[1]]
  expect_equal(gain$y[which.max(gain$x)], 1)
  expect_true(all(diff(gain$y[order(gain$x)]) >= 0))

  lift <- expect_builds(dx_plot_lift(dx_obj))$data
  lift <- lift[[which(vapply(lift, nrow, numeric(1)) > 1)[1]]]
  expect_equal(lift$y[which.max(lift$x)], 1)

  # The perfect model captures every positive once the top prev share is selected
  cap <- dx_plot_cap(dx_obj)$data
  perfect <- cap[cap$Model == "Perfect Model", ]
  expect_equal(min(perfect$x[perfect$y == 1]), min(perfect$x[perfect$x >= prev]))
})

test_that("Multiple ROC plot draws each model and reports DeLong p-values", {
  dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)
  cmp <- dx_compare(list(glm = dx_obj, rf = dx_rf))
  b <- expect_builds(dx_plot_rocs(cmp))

  expect_equal(length(unique(b$data[[1]]$group)), 2)

  text <- b$data[[length(b$data)]]$label
  delong <- cmp$tests[cmp$tests$test == "DeLong's test for ROC curves", ]
  expect_equal(text, paste0(delong$models, ": ", format_pvalue(delong$p_value)))
})
