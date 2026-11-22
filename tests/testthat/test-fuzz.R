# Randomized ("fuzz") tests. Each seed builds a random dataset and set of
# options, runs dx(), and checks the results against invariants and against
# expected values calculated here from the raw vectors, not from package
# functions.
#
# A fixed set of seeds runs by default so results are reproducible. To run
# more, set the DX_FUZZ_N environment variable, e.g. DX_FUZZ_N=500. Failures
# report the seed; rebuild the case with fuzz_case(seed).

fuzz_seeds <- function() {
  n <- suppressWarnings(as.integer(Sys.getenv("DX_FUZZ_N", "40")))
  if (is.na(n) || n < 1) n <- 40
  seq_len(n)
}

# Run code with a given seed, then restore the random number state
with_seed <- function(seed, code) {
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (is.null(old_seed)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  code
}

fuzz_case <- function(seed) {
  with_seed(seed, {
    repeat {
      # Half small samples, where edge cases are common, half moderate
      n <- if (stats::runif(1) < .5) sample(2:10, 1) else sample(11:300, 1)
      prevalence <- stats::runif(1, .05, .95)
      truth <- stats::rbinom(n, 1, prevalence)

      # Predictions that are related to truth by a random amount
      signal <- stats::runif(1, -1, 3)
      pred <- stats::plogis(stats::rnorm(n, signal * (truth - .5), 1))

      # Heavy ties
      if (stats::runif(1) < .4) pred <- round(pred, sample(1:2, 1))
      # Exact 0 and 1
      if (stats::runif(1) < .2) {
        pred[sample(n, min(n, 2))] <- c(0, 1)[seq_len(min(n, 2))]
      }

      # Both classes and at least two distinct predictions are required
      if (length(unique(truth)) == 2 && length(unique(pred)) >= 2) break
    }

    threshold <- switch(sample(3, 1),
      .5,
      round(stats::runif(1, .05, .95), 2),
      sample(pred, 1) # equal to an observed prediction
    )
    # Sometimes put every prediction on one side of the threshold
    if (stats::runif(1) < .1) {
      threshold <- if (stats::runif(1) < .5) min(pred) else max(pred) + 1e-6
      threshold <- min(max(threshold, 0), 1)
    }

    # Truth in different types
    truth_value <- switch(sample(4, 1),
      truth,
      as.logical(truth),
      factor(truth),
      as.character(truth)
    )

    data <- data.frame(pred = pred, pred2 = stats::runif(n))
    data$truth <- truth_value

    grouping <- NA
    if (n >= 6 && stats::runif(1) < .5) {
      levels <- sample(1:5, 1)
      group <- sample(letters[seq_len(levels)], n, replace = TRUE)
      if (stats::runif(1) < .2) group[sample(n, 1)] <- NA
      data$group <- group
      grouping <- "group"
    }

    threshold_range <- if (stats::runif(1) < .3) round(stats::runif(sample(1:3, 1)), 2) else NA
    target_prevalence <- if (stats::runif(1) < .3) round(stats::runif(1, .01, .99), 2) else NA

    list(
      seed = seed,
      data = data,
      truth = truth,
      args = list(
        data = data,
        pred_varname = "pred",
        true_varname = "truth",
        setthreshold = threshold,
        poslabel = sample(c(1, 1, 0), 1),
        grouping_variables = grouping,
        threshold_range = threshold_range,
        prevalence = target_prevalence,
        citype = sample(c("exact", "wilson", "agresti-coull"), 1)
      )
    )
  })
}

# Warnings that are expected for some random inputs
allowed_warnings <- c(
  "with only one outcome class",
  "excludes subgroup\\(s\\) where all predictions fall on one side",
  "Chi-squared approximation may be incorrect",
  # pROC, when the classes are perfectly separated
  "AUC == 1"
)

# Run code, collecting warnings rather than signaling them
collect_warnings <- function(code) {
  warnings <- character()
  value <- withCallingHandlers(code, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = warnings)
}

expect_allowed_warnings <- function(warnings, info) {
  ok <- vapply(warnings, function(w) {
    any(vapply(allowed_warnings, grepl, logical(1), x = w))
  }, logical(1))
  expect_true(all(ok), info = paste(info, "-", paste(warnings[!ok], collapse = "; ")))
}

# Confusion matrix counts calculated directly. Predictions at or above the
# threshold are test positive; poslabel = 0 swaps the positive class.
direct_counts <- function(pred, truth, threshold, poslabel) {
  test_pos <- (pred >= threshold) == (poslabel == 1)
  true_pos <- truth == poslabel
  c(
    tp = sum(test_pos & true_pos),
    fn = sum(!test_pos & true_pos),
    fp = sum(test_pos & !true_pos),
    tn = sum(!test_pos & !true_pos)
  )
}

# AUC as the Mann-Whitney statistic, with ties counted as 1/2
direct_auc <- function(pred, truth) {
  n1 <- sum(truth == 1)
  n0 <- sum(truth == 0)
  (sum(rank(pred)[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

estimates <- function(measures) {
  stats::setNames(measures$estimate, measures$measure)
}

test_that("dx() runs on random inputs without unexpected errors or warnings", {
  skip_on_cran()
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    info <- paste("seed", seed)
    res <- tryCatch(
      collect_warnings(do.call(dx, case$args)),
      error = function(e) e
    )
    expect_false(inherits(res, "error"), info = paste(info, "-", conditionMessage(res)))
    if (inherits(res, "error")) next
    expect_allowed_warnings(res$warnings, info)
  }
})

test_that("dx() confusion matrix and overall measures match direct calculations", {
  skip_on_cran()
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    info <- paste("seed", seed)
    a <- case$args
    d <- suppressWarnings(do.call(dx, a))
    pred <- case$data$pred
    truth <- case$truth

    k <- as.list(direct_counts(pred, truth, a$setthreshold, a$poslabel))
    expect_equal(d$cm$tp, k$tp, info = info)
    expect_equal(d$cm$fn, k$fn, info = info)
    expect_equal(d$cm$fp, k$fp, info = info)
    expect_equal(d$cm$tn, k$tn, info = info)
    expect_equal(d$cm$n, length(truth), info = info)

    # dx() rounds thresholds, so match with a tolerance
    overall <- d$measures[d$measures$variable == "Overall" &
      abs(d$measures$threshold - a$setthreshold) < 1e-8, ]
    est <- estimates(overall)
    n <- length(truth)

    expect_equal(nrow(overall), length(unique(overall$measure)), info = info)
    expect_equal(est[["Sensitivity"]], k$tp / (k$tp + k$fn), info = info)
    expect_equal(est[["Specificity"]], k$tn / (k$tn + k$fp), info = info)
    expect_equal(est[["Positive Predictive Value"]], k$tp / (k$tp + k$fp), info = info)
    expect_equal(est[["Negative Predictive Value"]], k$tn / (k$tn + k$fn), info = info)
    expect_equal(est[["Accuracy"]], (k$tp + k$tn) / n, info = info)
    expect_equal(est[["Prevalence"]], (k$tp + k$fn) / n, info = info)
    expect_equal(est[["False Negative Rate"]], k$fn / (k$tp + k$fn), info = info)
    expect_equal(est[["False Positive Rate"]], k$fp / (k$fp + k$tn), info = info)
    expect_equal(est[["False Discovery Rate"]], k$fp / (k$tp + k$fp), info = info)

    # AUC and Brier don't depend on the threshold or poslabel
    expect_equal(est[["AUC ROC"]], direct_auc(pred, truth), info = info)
    expect_equal(est[["Brier Score"]], mean((pred - truth)^2), info = info)
    expect_equal(as.numeric(d$roc$auc), direct_auc(pred, truth), info = info)

    # Estimates lie inside their confidence intervals
    has_ci <- is.finite(overall$estimate) & is.finite(overall$conf_low) &
      is.finite(overall$conf_high)
    tol <- 1e-8
    expect_true(
      all(overall$conf_low[has_ci] <= overall$estimate[has_ci] + tol &
        overall$estimate[has_ci] <= overall$conf_high[has_ci] + tol),
      info = paste(info, "-", paste(overall$measure[has_ci][
        !(overall$conf_low[has_ci] <= overall$estimate[has_ci] + tol &
          overall$estimate[has_ci] <= overall$conf_high[has_ci] + tol)
      ], collapse = ", "))
    )

    # Proportions and their intervals lie in [0, 1]
    binomial <- grepl("^Binomial", overall$conf_type)
    vals <- c(overall$estimate[binomial], overall$conf_low[binomial], overall$conf_high[binomial])
    vals <- vals[!is.na(vals)]
    expect_true(all(vals >= -tol & vals <= 1 + tol), info = info)

    # Measures at every threshold use the same rows
    expect_true(all(d$measures$n[d$measures$variable == "Overall"] == n), info = info)
  }
})

test_that("Threshold table matches direct calculations at every threshold", {
  skip_on_cran()
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    info <- paste("seed", seed)
    a <- case$args
    d <- suppressWarnings(do.call(dx, a))
    pred <- case$data$pred
    truth <- case$truth
    th <- d$thresholds

    expected_thresholds <- sort(unique(c(0, pred, 1)), decreasing = TRUE)
    expect_equal(th$threshold, expected_thresholds, info = info)

    k <- t(vapply(th$threshold, direct_counts, numeric(4),
      pred = pred, truth = truth, poslabel = a$poslabel
    ))
    tp <- k[, "tp"]
    fn <- k[, "fn"]
    fp <- k[, "fp"]
    tn <- k[, "tn"]
    n <- length(truth)

    expect_equal(th$tp, tp, info = info)
    expect_equal(th$fn, fn, info = info)
    expect_equal(th$fp, fp, info = info)
    expect_equal(th$tn, tn, info = info)

    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    ppv <- tp / (tp + fp)
    expect_equal(th$sensitivity, sens, info = info)
    expect_equal(th$specificity, spec, info = info)
    expect_equal(th$precision, ppv, info = info)
    expect_equal(th$ppv, ppv, info = info)
    expect_equal(th$npv, tn / (tn + fn), info = info)
    expect_equal(th$fpr, fp / (fp + tn), info = info)
    expect_equal(th$informedness, sens + spec - 1, info = info)
    expect_equal(th$f1, ifelse(tp == 0, 0, 2 * tp / (2 * tp + fp + fn)), info = info)
    weight <- th$threshold / (1 - th$threshold)
    expect_equal(th$net_benefit, tp / n - weight * fp / n, info = info)

    # Totals don't depend on the threshold, and sensitivity moves in one
    # direction as the threshold rises
    expect_true(all(tp + fn == sum(truth == a$poslabel)), info = info)
    if (a$poslabel == 1) {
      expect_true(all(diff(rev(th$sensitivity)) <= 0), info = info)
    } else {
      expect_true(all(diff(rev(th$sensitivity)) >= 0), info = info)
    }
  }
})

test_that("Rank and prevalence tables satisfy their definitions", {
  skip_on_cran()
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    info <- paste("seed", seed)
    a <- case$args
    d <- suppressWarnings(do.call(dx, a))

    r <- d$rank
    expect_true(all(diff(r$predprob) <= 0), info = info)
    for (col in c("gain", "cumulativeTPR", "cumulativeFPR", "percentile")) {
      expect_true(all(diff(r[[col]]) >= 0), info = paste(info, col))
      expect_equal(utils::tail(r[[col]], 1), 1, info = paste(info, col))
    }

    k <- as.list(direct_counts(case$data$pred, case$truth, a$setthreshold, a$poslabel))
    sens <- k$tp / (k$tp + k$fn)
    spec <- k$tn / (k$tn + k$fp)
    p <- d$prevalence$prevalence
    expect_equal(d$prevalence$ppv, sens * p / (sens * p + (1 - spec) * (1 - p)), info = info)
    expect_equal(d$prevalence$npv, spec * (1 - p) / ((1 - sens) * p + spec * (1 - p)), info = info)
  }
})

test_that("Subgroup measures match dx() run on each subgroup alone", {
  skip_on_cran()
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    a <- case$args
    if (identical(a$grouping_variables, NA)) next
    info <- paste("seed", seed)
    d <- suppressWarnings(do.call(dx, a))

    groups <- d$measures[d$measures$variable == "group" & d$measures$measure != "Breslow-Day", ]
    has_group <- !is.na(case$data$group)
    expect_equal(sum(unique(groups[c("label", "n")])$n), sum(has_group), info = info)

    for (g in unique(groups$label)) {
      rows <- has_group & case$data$group == g
      sub_truth <- case$truth[rows]
      sub_pred <- case$data$pred[rows]
      # dx() needs both classes and two distinct predictions
      if (length(unique(sub_truth)) < 2 || length(unique(sub_pred)) < 2) next

      sub_args <- a
      sub_args$data <- case$data[rows, ]
      sub_args$grouping_variables <- NA
      sub_args$threshold_range <- NA
      alone <- suppressWarnings(do.call(dx, sub_args))$measures

      in_group <- groups[groups$label == g, ]
      expect_equal(in_group$measure, alone$measure, info = paste(info, g))
      expect_equal(in_group$estimate, alone$estimate, info = paste(info, g))
      expect_equal(in_group$conf_low, alone$conf_low, info = paste(info, g))
      expect_equal(in_group$conf_high, alone$conf_high, info = paste(info, g))
    }
  }
})

test_that("dx_compare() runs on random pairs of models", {
  skip_on_cran()
  fit <- function(case, pred_varname = "pred") {
    a <- case$args
    a$pred_varname <- pred_varname
    a$grouping_variables <- NA
    a$threshold_range <- NA
    suppressWarnings(do.call(dx, a))
  }
  for (seed in fuzz_seeds()) {
    case <- fuzz_case(seed)
    info <- paste("seed", seed)
    d1 <- fit(case)

    # Paired: two models on the same rows. Unpaired: an independent sample.
    pairs <- list(
      paired = list(d1, fit(case, "pred2")),
      unpaired = list(d1, fit(fuzz_case(seed + 100000)))
    )
    for (type in names(pairs)) {
      res <- tryCatch(
        collect_warnings(dx_compare(pairs[[type]], paired = type == "paired")),
        error = function(e) e
      )
      label <- paste(info, type)
      expect_false(inherits(res, "error"), info = paste(label, "-", conditionMessage(res)))
      if (inherits(res, "error")) next
      expect_allowed_warnings(res$warnings, label)
      p <- res$value$tests$p_value
      expect_true(all(is.na(p) | (p >= 0 & p <= 1)), info = label)
    }
  }
})
