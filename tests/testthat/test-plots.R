dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  setthreshold = .3
)

# Area of a polygon from built layer data (shoelace formula)
polygon_area <- function(d) {
  x <- d$x
  y <- d$y
  abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
}

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

  # The shaded area under the curve equals the AUC
  expect_equal(polygon_area(b$data[[1]]), as.numeric(dx_obj$roc$auc))

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

  # The perfect model captures every positive once the top prev share is
  # selected. Compare with a tolerance, since exact equality differs by platform.
  cap <- dx_plot_cap(dx_obj)$data
  perfect <- cap[cap$Model == "Perfect Model", ]
  at_prev <- which.min(abs(perfect$x - prev))
  expect_equal(perfect$y[at_prev], 1)
  expect_equal(perfect$y[perfect$x >= perfect$x[at_prev]], rep(1, sum(perfect$x >= perfect$x[at_prev])))
  expect_true(all(perfect$y[perfect$x < perfect$x[at_prev]] < 1))
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

test_that("Forest plot shows Breslow-Day and custom axes", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  dx_grp <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = c("AgeGroup", "Sex")
  )
  grob_labels <- function(g) unlist(lapply(g$grobs, function(x) if (inherits(x, "text")) x$label))

  # Breslow-Day p-values appear in each variable's header row
  g <- dx_plot_forest(
    dx_grp,
    measures = c("AUC ROC", "Sensitivity", "Odds Ratio", "Breslow-Day"),
    return = "grid"
  )
  bd <- dx_grp$measures[dx_grp$measures$measure == "Breslow-Day", ]
  expect_true(all(bd$summary %in% grob_labels(g)))

  # User supplied breaks become the axis ticks
  g <- dx_plot_forest(dx_grp, limits = c(0, 60), breaks = c(0, 20, 40, 60), return = "grid")
  ticks <- sub("^tick_label_", "", grep("^tick_label_", g$layout$name, value = TRUE))
  expect_setequal(ticks, c("0", "20", "40", "60"))
})

test_that("ROC and forest plots can be saved to a file", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  dx_grp <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )

  roc_file <- tempfile(fileext = ".pdf")
  forest_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(roc_file, forest_file)))

  dx_plot_roc(dx_grp, filename = roc_file)
  dx_plot_forest(dx_grp, filename = forest_file)

  expect_true(file.exists(roc_file))
  expect_true(file.exists(forest_file))
})

test_that("ROC plot matches the AUC with tied predictions and any direction", {
  ties <- data.frame(truth = c(1, 1, 0, 0, 1, 0, 1, 0), p = c(.5, .5, .5, .5, .2, .2, .9, .1))
  d <- dx(data = ties, true_varname = "truth", pred_varname = "p", setthreshold = .5)
  expect_equal(polygon_area(expect_builds(dx_plot_roc(d))$data[[1]]), as.numeric(d$roc$auc))

  inverted <- dx_heart_failure
  inverted$predicted <- 1 - inverted$predicted
  d <- suppressWarnings(dx(data = inverted, true_varname = "truth", pred_varname = "predicted", setthreshold = .7, direction = ">"))
  expect_equal(polygon_area(expect_builds(dx_plot_roc(d))$data[[1]]), as.numeric(d$roc$auc))
})

test_that("PR curve reaches full recall", {
  d <- dx(
    data = data.frame(truth = c(1, 0, 1, 0, 0, 1), p = c(.9, .8, .7, .4, .3, .1)),
    true_varname = "truth", pred_varname = "p", setthreshold = .5
  )
  pr <- expect_builds(dx_plot_pr(d))$data[[1]]
  expect_equal(max(pr$x), 1)
})

test_that("ROC summary statistics follow the requested order", {
  b <- expect_builds(dx_plot_roc(dx_obj, summary_stats = c(3, 1, 3)))
  text <- b$data[[length(b$data)]]$label
  expect_length(text, 2)
  expect_match(text[1], "^Sensitivity")
  expect_match(text[2], "^AUC ROC")
})

test_that("Rank-based plots and the decision curve use poslabel = 0", {
  hf <- dx_heart_failure
  d0 <- dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3, poslabel = 0)

  # poslabel = 0 matches poslabel = 1 with the outcome and predictions flipped
  flipped <- hf
  flipped$truth <- 1 - flipped$truth
  flipped$predicted <- 1 - flipped$predicted
  d1 <- dx(data = flipped, true_varname = "truth", pred_varname = "predicted", setthreshold = .7)

  cols <- c("cumulativeTruePositives", "gain", "percentile", "lift", "cumulativeTPR", "cumulativeFPR")
  expect_equal(as.data.frame(d0$rank)[cols], as.data.frame(d1$rank)[cols], check.attributes = FALSE)

  for (f in list(dx_plot_gain, dx_plot_lift, dx_plot_ks, dx_plot_cap, dx_plot_decision_curve)) {
    expect_no_warning(ggplot2::ggplot_build(f(d0)))
  }

  # Gain and CAP reach 1 when every event is captured
  expect_equal(utils::tail(d0$rank$gain, 1), 1)

  # The decision curve is on the event's threshold probability, and treat all
  # uses the prevalence of the event
  b <- ggplot2::ggplot_build(dx_plot_decision_curve(d0))
  model <- b$data[[1]]
  treat_all <- b$data[[2]]
  prev0 <- mean(hf$truth == 0)
  expect_equal(treat_all$y, prev0 - (1 - prev0) * treat_all$x / (1 - treat_all$x))
  thr <- d0$thresholds
  expected <- thr$net_benefit[match(round(1 - model$x, 10), round(thr$threshold, 10))]
  expect_equal(model$y, expected)
})

test_that("KS axis label matches the axis direction", {
  p <- dx_plot_ks(dx_obj)
  expect_equal(p$labels$x, "Prediction Score")
})

test_that("ROC reference circle is on the curve for either poslabel", {
  for (poslabel in c(0, 1)) {
    d <- dx(
      data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted",
      setthreshold = .3, poslabel = poslabel
    )
    b <- ggplot2::ggplot_build(dx_plot_roc(d, add_text = FALSE))
    curve <- b$data[[2]]
    circle <- b$data[[length(b$data)]]
    dist <- min(sqrt((curve$x - circle$x)^2 + (curve$y - circle$y)^2))
    expect_equal(dist, 0, info = paste("poslabel", poslabel))
  }
})

test_that("poslabel = 0 matches poslabel = 1 with the outcome and predictions flipped", {
  # setthreshold is not equal to any prediction, so ties at the threshold
  # (where pred < t and 1 - pred >= 1 - t differ) don't matter
  hf <- dx_heart_failure
  expect_false(any(hf$predicted == .3))
  flipped <- hf
  flipped$truth <- 1 - flipped$truth
  flipped$predicted <- 1 - flipped$predicted

  d0 <- dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3, poslabel = 0)
  d1 <- dx(data = flipped, true_varname = "truth", pred_varname = "predicted", setthreshold = .7)

  expect_equal(unclass(d0$cm), unclass(d1$cm))
  cols <- c("measure", "estimate", "conf_low", "conf_high")
  # AUC PR is a trapezoid over the threshold table, which treats tied
  # predictions differently in the two directions
  keep <- d0$measures$measure != "AUC PR"
  expect_equal(d0$measures[keep, cols], d1$measures[keep, cols])
  expect_equal(d0$prevalence, d1$prevalence)

  # Plots built from the cm, rank, or ROC curve draw the same layers
  layers <- function(d, f) {
    lapply(ggplot2::ggplot_build(f(d))$data, function(l) {
      l <- l[intersect(c("x", "y", "xend", "yend"), names(l))]
      l[do.call(order, l), , drop = FALSE]
    })
  }
  plots <- list(
    roc = function(d) dx_plot_roc(d, add_text = FALSE),
    cm = dx_plot_cm, gain = dx_plot_gain, lift = dx_plot_lift, cap = dx_plot_cap
  )
  for (p in names(plots)) {
    expect_equal(layers(d0, plots[[p]]), layers(d1, plots[[p]]), check.attributes = FALSE, info = p)
  }

  # The KS plot's x axis is the prediction, which is mirrored in the flipped data
  ks0 <- ggplot2::ggplot_build(dx_plot_ks(d0))$data
  ks1 <- ggplot2::ggplot_build(dx_plot_ks(d1))$data
  key <- function(l, mirror = FALSE) {
    l <- data.frame(group = l$group, x = if (mirror) 1 - l$x else l$x, y = l$y)
    l[do.call(order, l), ]
  }
  expect_equal(key(ks0[[1]]), key(ks1[[1]], mirror = TRUE), check.attributes = FALSE)
  expect_equal(ks0[[2]]$xintercept, 1 - ks1[[2]]$xintercept)
  expect_equal(ks0[[3]]$label, ks1[[3]]$label)
})
