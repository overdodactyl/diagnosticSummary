# Regression tests for previously fixed bugs. Each test notes the commit
# that fixed the bug so it can't quietly come back.

dx_grp <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = c("AgeGroup", "Sex")
)


test_that("MCC does not overflow with large counts (9125b9a)", {
  # tp = tn = 60,000 and fp = fn = 20,000 as integers. The denominator
  # (80,000^4) overflows integer arithmetic.
  # MCC = (60000^2 - 20000^2) / 80000^2 = 0.5
  truth <- rep(c(1, 0, 1, 0), times = c(60000, 60000, 20000, 20000))
  predicted <- rep(c(1, 0, 0, 1), times = c(60000, 60000, 20000, 20000))
  cm <- dx_cm(predicted, truth, threshold = .5, poslabel = 1)

  expect_type(cm$tp, "integer")
  expect_equal(dx_mcc(cm, detail = "simple"), 0.5)
})

test_that("Cohen's kappa does not overflow with large counts", {
  # Same counts as above: po = 0.75, pe = 0.5, so kappa = 0.5
  truth <- rep(c(1, 0, 1, 0), times = c(60000, 60000, 20000, 20000))
  predicted <- rep(c(1, 0, 0, 1), times = c(60000, 60000, 20000, 20000))
  cm <- dx_cm(predicted, truth, threshold = .5, poslabel = 1)

  expect_equal(dx_cohens_kappa(cm, detail = "simple"), 0.5)
  expect_false(is.na(dx_cohens_kappa(cm)$conf_low))
})

test_that("Chi-square is only reported once (69b1582)", {
  chisq <- dx_grp$measures[dx_grp$measures$measure == "Pearson's Chi-squared", ]
  counts <- table(paste(chisq$variable, chisq$label))

  expect_true(all(counts == 1))
})

test_that("Chi-square does not warn with small counts (fd64010)", {
  cm <- dx_cm(c(1, 0, 0, 1), c(1, 0, 1, 1), threshold = .5, poslabel = 1)

  expect_no_warning(dx_chi_square(cm))
})

test_that("PPV and NPV in thresholds use the right formulas (f7417a7)", {
  thr <- dx_grp$thresholds

  expect_equal(thr$ppv, thr$tp / (thr$tp + thr$fp))
  expect_equal(thr$npv, thr$tn / (thr$tn + thr$fn))
})

test_that("Character grouping variables match factor results (4153bb0)", {
  hf_chr <- dx_heart_failure
  hf_chr$Sex <- as.character(hf_chr$Sex)

  dx_chr <- dx(
    data = hf_chr,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )
  dx_fct <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )

  sort_measures <- function(x) {
    x <- as.data.frame(x$measures)
    x <- x[order(x$variable, x$label, x$measure), ]
    rownames(x) <- NULL
    x
  }

  expect_equal(sort_measures(dx_chr), sort_measures(dx_fct))
})

test_that("Missing grouping values are dropped from subgroups (70b61e1)", {
  hf_na <- dx_heart_failure
  hf_na$Sex[c(1, 5, 9)] <- NA

  dx_na <- dx(
    data = hf_na,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )

  acc <- dx_na$measures[dx_na$measures$measure == "Accuracy", ]
  sex <- acc[acc$variable == "Sex", ]

  expect_false(any(is.na(sex$label)))
  expect_equal(sum(sex$n), sum(!is.na(hf_na$Sex)))
  expect_equal(sex$n[sex$label == "Female"], sum(hf_na$Sex == "Female", na.rm = TRUE))
  expect_equal(acc$n[acc$variable == "Overall"], nrow(hf_na))
})

test_that("Breslow-Day is reported for each grouping variable", {
  bd <- dx_grp$measures[dx_grp$measures$measure == "Breslow-Day", ]

  expect_setequal(bd$variable, c("AgeGroup", "Sex"))
  expect_equal(nrow(bd), 2)
})

test_that("as.data.frame filters on variable and label", {
  res <- as.data.frame(dx_grp, variable = "Sex", label = "Male")

  expect_gt(nrow(res), 0)
  expect_true(all(res$variable == "Sex"))
  expect_true(all(res$label == "Male"))
})

test_that("Forest table keeps group order with one row per group (ed5db54, 915a76e)", {
  dx_rev <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = c("Sex", "AgeGroup")
  )
  measures <- c("Specificity", "AUC ROC", "Sensitivity", "Odds Ratio", "Breslow-Day")

  res <- dx_prep_forest(dx_rev, fraction = FALSE, fraction_multiline = FALSE, measures = measures)

  # Variables in grouping_variables order, levels in factor order, Overall last
  expected <- c(
    "Sex", levels(dx_heart_failure$Sex),
    "AgeGroup", levels(dx_heart_failure$AgeGroup),
    "Overall"
  )
  expect_equal(res$group, expected)
  expect_true(all(c("Specificity", "AUC ROC", "Sensitivity", "Odds Ratio") %in% names(res)))
})

test_that("Forest plot does not warn (de7664b)", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  expect_no_warning(dx_plot_forest(dx_grp))
})

test_that("Log scale forest range includes odds ratios below 1 (78a8b42)", {
  res <- plot_range(trans = "log10", min_or = 0.5, max_or = 20)

  expect_equal(res, c(0.1, 100))
})

test_that("dx() does not change the user's random number state", {
  set.seed(1)
  expected <- stats::runif(3)

  set.seed(1)
  dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex",
    doboot = TRUE,
    bootreps = 50
  )
  expect_equal(stats::runif(3), expected)
})

test_that("Bootstrapped CIs depend only on bootseed", {
  fit_boot <- function(bootseed) {
    dx(
      data = dx_heart_failure,
      true_varname = "truth",
      pred_varname = "predicted",
      setthreshold = .3,
      grouping_variables = "Sex",
      doboot = TRUE,
      bootreps = 50,
      bootseed = bootseed
    )$measures
  }

  set.seed(1)
  a <- fit_boot(20191015)
  set.seed(999)
  b <- fit_boot(20191015)
  c <- fit_boot(42)

  expect_identical(a, b)
  f1 <- function(x) x$conf_low[x$measure == "F1 Score"]
  expect_false(identical(f1(a), f1(c)))
})

test_that("AUC is not flipped for worse than chance models", {
  inverted <- dx_heart_failure
  inverted$predicted <- 1 - inverted$predicted

  auc <- function(data, ...) {
    d <- dx(data = data, true_varname = "truth", pred_varname = "predicted", setthreshold = .3, ...)
    m <- d$measures
    list(
      measures = m$estimate[m$variable == "Overall" & m$measure == "AUC ROC"],
      roc = as.numeric(d$roc$auc)
    )
  }

  original <- auc(dx_heart_failure)
  flipped <- auc(inverted)

  expect_lt(flipped$measures, .5)
  expect_equal(flipped$measures, 1 - original$measures)
  expect_equal(flipped$measures, flipped$roc)

  # An explicit direction is used for both the measures and the ROC curve
  expect_warning(reversed <- auc(inverted, direction = ">"), "only affects the AUC")
  expect_equal(reversed$measures, original$measures)
  expect_equal(reversed$measures, reversed$roc)
})

test_that("as.data.frame and summary filter on several values", {
  res <- as.data.frame(dx_grp, variable = c("Sex", "AgeGroup"), measure = c("Accuracy", "Sensitivity"))

  expect_setequal(unique(res$variable), c("Sex", "AgeGroup"))
  expect_setequal(unique(res$measure), c("Accuracy", "Sensitivity"))
  expect_equal(nrow(res), 2 * 4)

  expect_no_error(summary(dx_grp, variable = c("Sex", "AgeGroup"), label = c("Male", "(20,50]")))
})

test_that("Bootstrapped metrics are not formatted as percentages", {
  cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth, threshold = .3, poslabel = 1)

  set.seed(1)
  res <- dx_mcc(cm, boot = TRUE, bootreps = 50)
  expected <- conf_int(res$estimate, res$conf_low, res$conf_high, accuracy = .01)

  expect_equal(res$summary, expected)
  expect_false(grepl("%", res$summary))
})

test_that("Cohen's kappa CI stays within -1 and 1", {
  # n = 10, kappa = 0.6; the normal approximation upper bound is about 1.10
  res <- dx_cohens_kappa(make_cm(tp = 4, fn = 1, fp = 1, tn = 4))

  expect_equal(res$estimate, .6)
  expect_equal(res$conf_high, 1)
  expect_gte(res$conf_low, -1)
})

test_that("Subgroups follow factor level order", {
  hf <- dx_heart_failure
  hf$num <- as.integer(hf$Sex) * 10
  hf$chr <- as.character(hf$AgeGroup)

  d <- dx(
    data = hf,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = c("Sex", "num", "chr")
  )
  labels <- function(var) unique(d$measures$label[d$measures$variable == var & d$measures$measure == "Accuracy"])

  expect_equal(labels("Sex"), levels(hf$Sex))
  expect_equal(labels("num"), c("10", "20"))
  expect_equal(labels("chr"), sort(unique(hf$chr)))
})

test_that("Unused factor levels are dropped from subgroups", {
  hf <- dx_heart_failure
  hf$Sex <- factor(hf$Sex, levels = c("Female", "Male", "Other"))

  d <- dx(
    data = hf,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = "Sex"
  )
  sex <- d$measures[d$measures$variable == "Sex", ]

  expect_false("Other" %in% sex$label)
  expect_equal(sum(sex$measure == "Breslow-Day"), 1)
})

test_that("Subgroups with one outcome class report NA instead of failing", {
  hf <- dx_heart_failure
  # Every fifth negative goes into its own subgroup, which has no positives
  hf$g <- factor(ifelse(hf$truth == 0 & seq_len(nrow(hf)) %% 5 == 0, "neg_only", "rest"))

  expect_warning(
    d <- dx(
      data = hf,
      true_varname = "truth",
      pred_varname = "predicted",
      setthreshold = .3,
      grouping_variables = "g"
    ),
    "with only one outcome class: neg_only"
  )

  neg <- d$measures[d$measures$label == "neg_only", ]
  for (measure in c("AUC ROC", "Odds Ratio", "LRT+", "LRT-", "Pearson's Chi-squared", "Fisher's Exact", "G-Test")) {
    expect_true(is.na(neg$estimate[neg$measure == measure]), info = measure)
  }
  expect_equal(neg$summary[neg$measure == "AUC ROC"], "- (-, -)")

  # Measures that are defined are still reported
  in_group <- hf$g == "neg_only"
  expect_equal(
    neg$estimate[neg$measure == "Specificity"],
    mean(hf$predicted[in_group] < .3)
  )

  # Only one informative subgroup remains, so Breslow-Day is skipped
  expect_false(any(d$measures$measure == "Breslow-Day" & d$measures$variable == "g"))
})

test_that("Subgroups with one row and single-level variables do not fail", {
  hf <- dx_heart_failure
  hf$solo <- factor(c("solo", rep("rest", nrow(hf) - 1)))
  hf$one <- factor("a")

  expect_warning(
    d <- dx(
      data = hf,
      true_varname = "truth",
      pred_varname = "predicted",
      setthreshold = .3,
      grouping_variables = c("solo", "one")
    ),
    "with only one outcome class: solo"
  )

  m <- d$measures
  expect_equal(m$n[m$label == "solo" & m$measure == "Accuracy"], 1)
  expect_equal(m$n[m$variable == "one" & m$measure == "Accuracy"], nrow(hf))
  expect_false(any(m$measure == "Breslow-Day" & m$variable %in% c("solo", "one")))
})

test_that("Breslow-Day excludes uninformative subgroups", {
  hf <- dx_heart_failure
  # Third level with both outcomes, where every prediction is above the threshold
  high <- hf[hf$predicted >= .3, ]
  extra <- rbind(high[high$truth == 1, ][1:10, ], high[high$truth == 0, ][1:10, ])
  extra$AgeGroup <- "all_positive"
  hf$AgeGroup <- as.character(hf$AgeGroup)
  hf2 <- rbind(hf, extra)

  expect_warning(
    d <- dx(
      data = hf2,
      true_varname = "truth",
      pred_varname = "predicted",
      setthreshold = .3,
      grouping_variables = "AgeGroup"
    ),
    "excludes subgroup\\(s\\) where all predictions fall on one side of the threshold: all_positive"
  )

  # Same result as testing only the two informative subgroups
  pred <- as.numeric(hf$predicted >= .3)
  expected <- breslow_day_test(table(pred, hf$truth, hf$AgeGroup))$p_value
  bd <- d$measures[d$measures$measure == "Breslow-Day", ]
  expect_equal(bd$estimate, unname(expected))
})

test_that("Standalone AUC and ratios are NA with one outcome class", {
  cm <- make_cm(tp = 0, fn = 0, fp = 5, tn = 15)

  expect_true(is.na(dx_auc(c(0, 0, 0), c(.1, .4, .8), detail = "simple")))
  expect_true(is.na(dx_odds_ratio(cm, detail = "simple")))
  expect_true(is.na(dx_lrt_pos(cm)$estimate))
  expect_equal(dx_chi_square(cm)$summary, "-")
})

test_that("AUC estimate is kept when its CI cannot be calculated", {
  # With one observation per class the DeLong variance is undefined
  res <- suppressWarnings(dx_auc(c(0, 1), c(.2, .7)))

  expect_equal(res$estimate, 1)
  expect_true(is.na(res$conf_low))
})

test_that("Net benefit uses the Vickers & Elkin (2006) formula", {
  thr <- dx_grp$thresholds
  n <- nrow(dx_heart_failure)
  prev <- mean(dx_heart_failure$truth)
  weight <- thr$threshold / (1 - thr$threshold)

  # Net benefit = TP/n - FP/n * pt / (1 - pt)
  ok <- thr$threshold < 1
  expect_equal(thr$net_benefit[ok], (thr$tp / n - thr$fp / n * weight)[ok])

  # Treating everyone (threshold 0) gives a net benefit equal to the prevalence,
  # the same as the treat all line in the decision curve
  expect_equal(thr$net_benefit[thr$threshold == 0], prev)
})

test_that("Forest plot indents subgroups with a missing odds ratio", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  hf <- dx_heart_failure
  hf$g <- factor(ifelse(hf$truth == 0 & seq_len(nrow(hf)) %% 5 == 0, "neg_only", "rest"))
  d <- suppressWarnings(dx(
    data = hf,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = c("g", "Sex")
  ))

  prep <- dx_prep_forest(d, fraction = FALSE, fraction_multiline = FALSE, measures = c("AUC ROC", "Odds Ratio"))
  expect_equal(prep$group[prep$header], c("g", "Sex"))

  g <- dx_plot_forest(d, return = "grid")
  labels <- unlist(lapply(g$grobs, function(x) if (inherits(x, "text")) x$label))
  expect_true(any(grepl("^\\s+neg_only$", labels)))
  expect_true("g" %in% labels)
})

test_that("Forest plot does not change the user's random number state", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  set.seed(1)
  expected <- stats::runif(3)

  set.seed(1)
  dx_plot_forest(dx_grp)
  expect_equal(stats::runif(3), expected)
})

test_that("KS statistic is not inflated by tied predictions", {
  # Four tied predictions with positives listed first. No threshold separates
  # the classes, so KS = 0, but stepping through the tie row by row gives 0.67.
  df <- data.frame(truth = c(1, 1, 0, 0, 1, 0), p = c(.5, .5, .5, .5, .2, .2))
  d <- dx(data = df, true_varname = "truth", pred_varname = "p", setthreshold = .3)

  p <- dx_plot_ks(d)
  labels <- unlist(lapply(seq_along(p$layers), function(i) ggplot2::layer_data(p, i)$label))
  expect_equal(labels, "KS = 0")
})

test_that("dx_compare names only the unnamed models in a partly named list", {
  dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  cmp <- dx_compare(list(glm = dx_glm, dx_rf))

  expect_equal(names(cmp$dx_list), c("glm", "Model 2"))
  expect_equal(unique(cmp$tests$models), "glm vs. Model 2")
})

test_that("Breslow-Day does not overflow with large integer counts", {
  x <- array(c(30000L, 20000L, 25000L, 40000L, 31000L, 19000L, 24000L, 41000L), c(2, 2, 2))
  expected <- breslow_day_test(array(as.numeric(x), c(2, 2, 2)))

  expect_no_warning(res <- breslow_day_test(x))
  expect_equal(res$statistic, expected$statistic)
})

test_that("Association tests are NA with one outcome class", {
  cm <- make_cm(tp = 0, fn = 0, fp = 5, tn = 15)

  expect_true(is.na(dx_chi_square(cm, "simple")))
  expect_true(is.na(dx_fishers_exact(cm, "simple")))
  expect_true(is.na(dx_g_test(cm, "simple")))
  expect_equal(dx_g_test(cm)$summary, "-")
})

test_that("Unpaired comparison works when a model has no predicted positives", {
  low <- dx_heart_failure
  low$predicted <- low$predicted * .25
  dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_low <- dx(data = low, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  expect_equal(dx_low$cm$testpos, 0)

  res <- suppressWarnings(dx_compare(list(a = dx_glm, b = dx_low), paired = FALSE))$tests
  expect_true(is.na(res$p_value[res$test == "Z-test: ppv"]))
  expect_false(is.na(res$p_value[res$test == "Z-test: sensitivity"]))
})

test_that("McNemar's test requires the same truth values", {
  dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  shuffled <- dx_heart_failure[rev(seq_len(nrow(dx_heart_failure))), ]
  dx_rev <- dx(data = shuffled, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  expect_error(dx_mcnemars(dx_glm, dx_rev), "same truth values")
})

test_that("Kappa of 1 is interpreted as almost perfect agreement", {
  expect_equal(dx_cohens_kappa(make_cm(tp = 30, fn = 0, fp = 0, tn = 70))$notes, "Almost Perfect Agreement")
})

test_that("Kappa is 0 when every case falls in one cell", {
  res <- dx_cohens_kappa(make_cm(tp = 0, fn = 0, fp = 0, tn = 10))

  expect_equal(res$estimate, 0)
  expect_true(is.na(res$conf_low))
})

test_that("Predictive value at a prevalence has an NA CI when undefined", {
  # Specificity of 1 makes the PPV logit infinite
  res <- dx_ppv_prevalence(make_cm(tp = 8, fn = 2, fp = 0, tn = 20), prevalence = .1)

  expect_equal(res$estimate, 1)
  # NA rather than NaN
  expect_identical(res$conf_low, NA_real_)
  expect_identical(res$conf_high, NA_real_)
})

test_that("Thresholds from seq() are not duplicated and can be filtered", {
  d <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    threshold_range = seq(.1, .9, .1)
  )
  m <- d$measures

  expect_equal(sum(m$measure == "Sensitivity"), 9)
  expect_equal(nrow(as.data.frame(d, thresh = .7, measure = "Sensitivity")), 1)
  expect_equal(nrow(as.data.frame(d, thresh = .3, measure = "Sensitivity")), 1)
})

test_that("Column names with spaces work", {
  renamed <- dx_heart_failure
  names(renamed)[names(renamed) == "predicted"] <- "pred score"

  d <- dx(data = renamed, true_varname = "truth", pred_varname = "pred score", setthreshold = .3)
  ref <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)

  expect_equal(d$measures, ref$measures)
  expect_equal(as.numeric(d$roc$auc), as.numeric(ref$roc$auc))
})

test_that("poslabel must be 0 or 1", {
  expect_error(
    dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", poslabel = 2),
    "poslabel should be 0 or 1"
  )
})

test_that("dx_compare requires unique model names", {
  dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)

  expect_error(dx_compare(list(a = dx_glm, a = dx_rf)), "unique name")
})

test_that("Forest plot ticks are placed on the odds ratio scale", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  tick_x <- function(g) {
    i <- grep("^tick_label_", g$layout$name)
    x <- vapply(g$grobs[i], function(t) as.numeric(t$children[[1]]$x), numeric(1))
    setNames(x, sub("^tick_label_", "", g$layout$name[i]))
  }
  forest <- function(...) dx_plot_forest(dx_grp, return = "grid", ...)

  # Breaks that don't span the limits
  x <- tick_x(forest(limits = c(0, 40), breaks = c(10, 20, 30)))
  expect_equal(unname(x), c(.25, .5, .75))

  # Default breaks outside the limits are dropped
  x <- tick_x(forest(limits = c(.5, 7)))
  expect_equal(names(x), as.character(1:7))
  expect_equal(unname(x), (1:7 - .5) / 6.5)

  # Log scale breaks are given as odds ratios
  x <- tick_x(forest(trans = "log10", breaks = c(1, 10, 100)))
  expect_equal(names(x), c("1", "10", "100"))
  expect_equal(unname(x), c(0, .5, 1))

  # Log scale default breaks are powers of 10 placed on the log scale
  x <- tick_x(forest(trans = "log10", limits = c(.2, 30)))
  expect_equal(names(x), c("1", "10"))
  expect_equal(unname(x), (log10(c(1, 10)) - log10(.2)) / (log10(30) - log10(.2)))
})

test_that("Forest plot clips the overall odds ratio line to its column", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  g <- dx_plot_forest(dx_grp, limits = c(.5, 7), return = "grid")
  expect_equal(g$layout$clip[g$layout$name == "overall_or"], "on")
})

test_that("Forest plot needs grouping variables", {
  skip_if_not_installed("gridExtra")
  skip_if_not_installed("gtable")

  d <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", setthreshold = .3)
  expect_error(dx_plot_forest(d), "needs a dx object created with grouping_variables")
})

test_that("Measures use the exact threshold when it equals a prediction", {
  # Rounding the threshold to 10 digits moved it above the prediction equal
  # to it, so measures disagreed with the confusion matrix
  df <- data.frame(p = c(.1, .2, .12345678906, .9), y = c(0, 0, 1, 1))
  d <- dx(df, pred_varname = "p", true_varname = "y", setthreshold = .12345678906)
  sens <- d$measures[d$measures$measure == "Sensitivity", ]

  expect_equal(d$cm$tp, 2)
  expect_equal(sens$estimate, 1)
  expect_equal(sens$threshold, .12345678906)
})

test_that("Thresholds are not duplicated when predictions include 0 and 1", {
  df <- data.frame(p = c(0, .2, .7, 1), y = c(0, 1, 0, 1))
  d <- dx(df, pred_varname = "p", true_varname = "y")

  expect_equal(d$thresholds$threshold, c(1, .7, .2, 0))
})

test_that("Breslow-Day is NA when every subgroup is perfectly separated", {
  # No false positives or negatives, so the Mantel-Haenszel OR is infinite
  df <- data.frame(
    p = c(.1, .8, .9, .2, .7, .6),
    y = c(0, 1, 1, 0, 1, 1),
    g = rep(c("a", "b"), each = 3)
  )
  d <- suppressWarnings(dx(df, pred_varname = "p", true_varname = "y", grouping_variables = "g"))
  bd <- d$measures[d$measures$measure == "Breslow-Day", ]

  expect_true(is.na(bd$estimate))
  expect_equal(bd$summary, "-")
  expect_match(bd$notes, "Undefined")

  x <- array(c(5, 0, 0, 5, 3, 0, 0, 4), c(2, 2, 2))
  expect_true(is.na(breslow_day_test(x)$p_value))
})

test_that("DeLong's test is NA when a class has one observation", {
  # AUCs are 1/3 and 2/3
  df <- data.frame(p1 = c(.1, .4, .35, .8), p2 = c(.2, .3, .6, .7), y = c(0, 0, 1, 0))
  d1 <- dx(df, pred_varname = "p1", true_varname = "y")
  d2 <- dx(df, pred_varname = "p2", true_varname = "y")

  for (paired in c(TRUE, FALSE)) {
    res <- dx_delong(d1, d2, paired = paired)
    expect_true(is.na(res$p_value), info = paired)
    expect_equal(res$estimate, -1 / 3, info = paired)
    expect_match(res$notes, "Undefined", info = paired)
    expect_true(is.na(dx_delong(d1, d2, detail = "simple", paired = paired)))
  }
  expect_no_error(dx_compare(list(d1, d2), paired = TRUE))
})

test_that("Binomial confidence intervals stay within 0 and 1", {
  cm <- make_cm(tp = 1, fn = 0, fp = 0, tn = 3)

  for (citype in c("agresti-coull", "asymptotic")) {
    sens <- dx_sensitivity(cm, citype = citype)
    fnr <- dx_fnr(cm, citype = citype)
    expect_equal(sens$conf_high, 1, info = citype)
    expect_equal(fnr$conf_low, 0, info = citype)
    expect_false(grepl("10[1-9]", sens$summary), info = citype)
  }
})

test_that("Net benefit uses the event's threshold probability when poslabel = 0", {
  # With poslabel = 0 the event is outcome 0, test positive means a
  # prediction below the threshold, and the event's threshold probability is
  # 1 - threshold. At threshold .75: tp = 2 (p = .1, .3), fp = 1 (p = .6), and
  # net benefit = 2/5 - 1/5 * (.25 / .75) = 1/3.
  df <- data.frame(p = c(.1, .3, .6, .75, .9), y = c(0, 0, 1, 0, 1))
  d <- dx(df, pred_varname = "p", true_varname = "y", poslabel = 0)
  thr <- d$thresholds

  expect_equal(thr$tp[thr$threshold == .75], 2)
  expect_equal(thr$fp[thr$threshold == .75], 1)
  expect_equal(thr$net_benefit[thr$threshold == .75], 1 / 3)
})

test_that("dx$thresholds stores the event's threshold probability", {
  for (poslabel in c(0, 1)) {
    d <- dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted", poslabel = poslabel)
    th <- d$thresholds
    expected <- if (poslabel == 1) th$threshold else 1 - th$threshold
    expect_equal(th$event_threshold, expected, info = paste("poslabel", poslabel))
  }
})

test_that("direction other than the default is deprecated", {
  expect_warning(
    dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted", direction = "auto"),
    "deprecated"
  )
  expect_no_warning(
    dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted", direction = "<")
  )
})

test_that("Estimates without a CI use a fixed number of decimals", {
  # Balanced accuracy is exactly 0.8, which format(digits = 2) showed as "0.8"
  cm <- make_cm(tp = 8, fn = 2, fp = 2, tn = 8)

  expect_equal(dx_balanced_accuracy(cm)$summary, "0.80")
  expect_equal(dx_g_mean(cm)$summary, "0.80")
  expect_equal(dx_brier(c(.9, .1), c(1, 0))$summary, "0.010")
  expect_equal(dx_auc_pr(c(1, 1, 0, 0), c(.9, .5, .5, .1))$summary, "0.887")
  expect_equal(dx_nir(cm)$summary, "50.0%")
})

test_that("dx_auc_pr() rejects inputs from the old precision/recall signature", {
  d <- dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
  th <- d$thresholds
  expect_error(dx_auc_pr(th$precision, th$sensitivity), "not precision and recall")
  expect_error(dx_auc_pr(c(0, 1, 1), c(.2, .8)), "same length")
  expect_error(dx_auc_pr(c(0, 1), c(.2, 1.5)), "between 0 and 1")
  expect_error(dx_auc_pr(c(0, 1), c(.2, .8), poslabel = 2), "poslabel")
  expect_no_error(dx_auc_pr(c(TRUE, FALSE), c(.8, .2)))
})

test_that("dx() does not overwrite user columns", {
  renamed <- dx_heart_failure
  names(renamed)[names(renamed) == "truth"] <- "test_binary"
  names(renamed)[names(renamed) == "Sex"] <- "true_binaryf"

  a <- dx(dx_heart_failure, true_varname = "truth", pred_varname = "predicted", grouping_variables = "Sex")
  b <- dx(renamed, true_varname = "test_binary", pred_varname = "predicted", grouping_variables = "true_binaryf")

  cols <- c("label", "measure", "estimate", "conf_low", "conf_high")
  expect_equal(b$measures[cols], a$measures[cols])
  expect_equal(b$cm, a$cm)
  expect_identical(names(b$data), names(renamed))
})
