# Mathematical identities between metrics. Expected values are written from
# the confusion matrix counts, not from other package functions, so these
# tests are independent of the implementation.

test_that("Metrics match their definitions", {
  for (name in names(cm_grid)) {
    x <- as.list(cm_grid[[name]])
    cm <- grid_cm(cm_grid[[name]])
    n <- x$tp + x$fn + x$fp + x$tn
    info <- paste("cm:", name)

    expect_equal(dx_sensitivity(cm, "simple"), x$tp / (x$tp + x$fn), info = info)
    expect_equal(dx_specificity(cm, "simple"), x$tn / (x$tn + x$fp), info = info)
    expect_equal(dx_ppv(cm, "simple"), x$tp / (x$tp + x$fp), info = info)
    expect_equal(dx_npv(cm, "simple"), x$tn / (x$tn + x$fn), info = info)
    expect_equal(dx_accuracy(cm, "simple"), (x$tp + x$tn) / n, info = info)
    expect_equal(dx_prevalence(cm, "simple"), (x$tp + x$fn) / n, info = info)
    expect_equal(dx_detection_prevalence(cm, "simple"), (x$tp + x$fp) / n, info = info)
    expect_equal(dx_fnr(cm, "simple"), x$fn / (x$tp + x$fn), info = info)
    expect_equal(dx_fpr(cm, "simple"), x$fp / (x$fp + x$tn), info = info)
    expect_equal(dx_fdr(cm, "simple"), x$fp / (x$tp + x$fp), info = info)
    expect_equal(dx_odds_ratio(cm, "simple"), (x$tp * x$tn) / (x$fp * x$fn), info = info)
    expect_equal(dx_f1(cm, "simple"), 2 * x$tp / (2 * x$tp + x$fp + x$fn), info = info)
    expect_equal(dx_nir(cm, "simple"), max(x$tp + x$fn, x$fp + x$tn) / n, info = info)

    # Cohen's kappa for a 2x2 table in closed form
    kappa <- 2 * (x$tp * x$tn - x$fn * x$fp) /
      ((x$tp + x$fp) * (x$fp + x$tn) + (x$tp + x$fn) * (x$fn + x$tn))
    expect_equal(dx_cohens_kappa(cm, "simple"), kappa, info = info)

    # MCC from counts
    mcc <- (x$tp * x$tn - x$fp * x$fn) /
      sqrt((x$tp + x$fp) * (x$tp + x$fn) * (x$tn + x$fp) * (x$tn + x$fn))
    expect_equal(dx_mcc(cm, "simple"), mcc, info = info)
  }
})

test_that("Metrics satisfy relationships with each other", {
  for (name in names(cm_grid)) {
    cm <- grid_cm(cm_grid[[name]])
    info <- paste("cm:", name)

    sens <- dx_sensitivity(cm, "simple")
    spec <- dx_specificity(cm, "simple")
    ppv <- dx_ppv(cm, "simple")
    npv <- dx_npv(cm, "simple")
    prev <- dx_prevalence(cm, "simple")

    expect_equal(dx_fnr(cm, "simple"), 1 - sens, info = info)
    expect_equal(dx_fpr(cm, "simple"), 1 - spec, info = info)
    expect_equal(dx_fdr(cm, "simple"), 1 - ppv, info = info)
    expect_equal(dx_balanced_accuracy(cm, "simple"), (sens + spec) / 2, info = info)
    expect_equal(dx_informedness(cm, "simple"), sens + spec - 1, info = info)
    expect_equal(dx_markedness(cm, "simple"), ppv + npv - 1, info = info)
    expect_equal(dx_g_mean(cm, "simple"), sqrt(sens * spec), info = info)
    expect_equal(dx_fowlkes_mallows(cm, "simple"), sqrt(ppv * sens), info = info)
    expect_equal(dx_lrt_pos(cm, "simple"), sens / (1 - spec), info = info)
    expect_equal(dx_lrt_neg(cm, "simple"), (1 - sens) / spec, info = info)
    expect_equal(
      dx_odds_ratio(cm, "simple"),
      dx_lrt_pos(cm, "simple") / dx_lrt_neg(cm, "simple"),
      info = info
    )
    expect_equal(dx_f1(cm, "simple"), 2 * ppv * sens / (ppv + sens), info = info)
    expect_equal(dx_f2(cm, "simple"), 5 * ppv * sens / (4 * ppv + sens), info = info)
    expect_equal(dx_fbeta(cm, beta = 1, detail = "simple"), dx_f1(cm, "simple"), info = info)
    expect_equal(dx_fbeta(cm, beta = 2, detail = "simple"), dx_f2(cm, "simple"), info = info)
    expect_equal(dx_nir(cm, "simple"), max(prev, 1 - prev), info = info)

    # Accuracy is the prevalence-weighted average of sensitivity and specificity
    expect_equal(dx_accuracy(cm, "simple"), prev * sens + (1 - prev) * spec, info = info)

    # MCC^2 = informedness * markedness, with the sign of tp*tn - fp*fn
    mcc <- dx_mcc(cm, "simple")
    expect_equal(mcc^2, dx_informedness(cm, "simple") * dx_markedness(cm, "simple"), info = info)
    expect_equal(sign(mcc), sign(cm$tp * cm$tn - cm$fp * cm$fn), info = info)
  }
})

test_that("Aliases return the same values", {
  cm <- grid_cm(cm_grid$balanced)

  expect_identical(dx_recall(cm, "simple"), dx_sensitivity(cm, "simple"))
  expect_identical(dx_tpr(cm, "simple"), dx_sensitivity(cm, "simple"))
  expect_identical(dx_tnr(cm, "simple"), dx_specificity(cm, "simple"))
  expect_identical(dx_precision(cm, "simple"), dx_ppv(cm, "simple"))
  expect_identical(dx_miss_rate(cm, "simple"), dx_fnr(cm, "simple"))
  expect_identical(dx_fall_out(cm, "simple"), dx_fpr(cm, "simple"))
  expect_identical(dx_youden_j(cm, "simple"), dx_informedness(cm, "simple"))
})

test_that("Swapping the positive class swaps or preserves metrics", {
  for (name in names(cm_grid)) {
    x <- cm_grid[[name]]
    cm <- grid_cm(x)
    # Relabel: positives become negatives, so tp <-> tn and fn <-> fp
    swapped <- make_cm(tp = x[["tn"]], fn = x[["fp"]], fp = x[["fn"]], tn = x[["tp"]])
    info <- paste("cm:", name)

    expect_equal(dx_sensitivity(swapped, "simple"), dx_specificity(cm, "simple"), info = info)
    expect_equal(dx_specificity(swapped, "simple"), dx_sensitivity(cm, "simple"), info = info)
    expect_equal(dx_ppv(swapped, "simple"), dx_npv(cm, "simple"), info = info)
    expect_equal(dx_npv(swapped, "simple"), dx_ppv(cm, "simple"), info = info)
    expect_equal(dx_accuracy(swapped, "simple"), dx_accuracy(cm, "simple"), info = info)
    expect_equal(dx_balanced_accuracy(swapped, "simple"), dx_balanced_accuracy(cm, "simple"), info = info)
    expect_equal(dx_informedness(swapped, "simple"), dx_informedness(cm, "simple"), info = info)
    expect_equal(dx_markedness(swapped, "simple"), dx_markedness(cm, "simple"), info = info)
    expect_equal(dx_mcc(swapped, "simple"), dx_mcc(cm, "simple"), info = info)
    expect_equal(dx_cohens_kappa(swapped, "simple"), dx_cohens_kappa(cm, "simple"), info = info)
    expect_equal(dx_odds_ratio(swapped, "simple"), dx_odds_ratio(cm, "simple"), info = info)
    expect_equal(dx_prevalence(swapped, "simple"), 1 - dx_prevalence(cm, "simple"), info = info)
  }
})

test_that("Full results agree with simple results and have valid CIs", {
  proportions <- list(
    dx_accuracy, dx_sensitivity, dx_specificity, dx_ppv, dx_npv,
    dx_fnr, dx_fpr, dx_fdr, dx_prevalence, dx_detection_prevalence
  )
  ratios <- list(dx_odds_ratio, dx_lrt_pos, dx_lrt_neg)
  others <- list(
    dx_cohens_kappa, dx_mcc, dx_balanced_accuracy, dx_informedness,
    dx_markedness, dx_g_mean, dx_fowlkes_mallows, dx_f1, dx_f2, dx_nir
  )

  for (name in names(cm_grid)) {
    cm <- grid_cm(cm_grid[[name]])

    for (f in c(proportions, ratios, others)) {
      full <- f(cm, detail = "full")
      info <- paste("cm:", name, "measure:", full$measure)

      expect_equal(nrow(full), 1, info = info)
      expect_equal(full$estimate, f(cm, detail = "simple"), info = info)
      if (!is.na(full$conf_low)) {
        expect_lte(full$conf_low, full$estimate)
        expect_gte(full$conf_high, full$estimate)
      }
    }

    for (f in proportions) {
      full <- f(cm, detail = "full")
      expect_gte(full$conf_low, 0)
      expect_lte(full$conf_high, 1)
    }

    for (f in ratios) {
      full <- f(cm, detail = "full")
      expect_gt(full$conf_low, 0)
    }
  }
})

test_that("dx() measures match the standalone metric functions", {
  dx_obj <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )

  funs <- list(
    "Accuracy" = dx_accuracy,
    "Sensitivity" = dx_sensitivity,
    "Specificity" = dx_specificity,
    "Positive Predictive Value" = dx_ppv,
    "Negative Predictive Value" = dx_npv,
    "LRT+" = dx_lrt_pos,
    "LRT-" = dx_lrt_neg,
    "Odds Ratio" = dx_odds_ratio,
    "F1 Score" = dx_f1,
    "F2 Score" = dx_f2,
    "Prevalence" = dx_prevalence,
    "False Negative Rate" = dx_fnr,
    "False Positive Rate" = dx_fpr,
    "False Discovery Rate" = dx_fdr,
    "Cohen's Kappa" = dx_cohens_kappa,
    "Matthews Correlation Coefficient" = dx_mcc,
    "Balanced Accuracy" = dx_balanced_accuracy,
    "Informedness" = dx_informedness,
    "Markedness" = dx_markedness,
    "G-mean" = dx_g_mean,
    "Fowlkes-Mallows Index" = dx_fowlkes_mallows
  )

  groups <- list(
    Overall = rep(TRUE, nrow(dx_heart_failure)),
    Female = dx_heart_failure$Sex == "Female",
    Male = dx_heart_failure$Sex == "Male"
  )

  for (label in names(groups)) {
    rows <- groups[[label]]
    cm <- dx_cm(dx_heart_failure$predicted[rows], dx_heart_failure$truth[rows], threshold = .3, poslabel = 1)
    m <- dx_obj$measures[dx_obj$measures$label == label, ]

    for (measure in names(funs)) {
      expect_equal(
        m$estimate[m$measure == measure],
        funs[[measure]](cm, detail = "simple"),
        info = paste(label, measure)
      )
    }

    brier <- mean((dx_heart_failure$predicted[rows] - dx_heart_failure$truth[rows])^2)
    expect_equal(m$estimate[m$measure == "Brier Score"], brier, info = label)
  }
})

test_that("Thresholds table matches counts from the data", {
  dx_obj <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3
  )
  thr <- dx_obj$thresholds
  pred <- dx_heart_failure$predicted
  truth <- dx_heart_failure$truth

  # Counts at each threshold, using predicted >= threshold as positive
  tp <- vapply(thr$threshold, function(t) sum(pred >= t & truth == 1), numeric(1))
  fp <- vapply(thr$threshold, function(t) sum(pred >= t & truth == 0), numeric(1))
  expect_equal(thr$tp, tp)
  expect_equal(thr$fp, fp)
  expect_equal(thr$tp + thr$fn, rep(sum(truth == 1), nrow(thr)))
  expect_equal(thr$tn + thr$fp, rep(sum(truth == 0), nrow(thr)))

  sens <- thr$tp / (thr$tp + thr$fn)
  spec <- thr$tn / (thr$tn + thr$fp)
  expect_equal(thr$sensitivity, sens)
  expect_equal(thr$specificity, spec)
  expect_equal(thr$fpr, 1 - spec)
  expect_equal(thr$precision, thr$tp / (thr$tp + thr$fp))
  expect_equal(thr$informedness, sens + spec - 1)
  expect_equal(thr$f1, 2 * thr$tp / (2 * thr$tp + thr$fp + thr$fn))
})
