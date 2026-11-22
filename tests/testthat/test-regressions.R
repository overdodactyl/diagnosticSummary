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
  reversed <- auc(inverted, direction = ">")
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
