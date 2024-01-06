dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  setthreshold = .3
)

test_that("Gain Plot", {
  expect_no_error(dx_plot_gain(dx_obj))
})

test_that("Lift Curve", {
  expect_no_error(dx_plot_lift(dx_obj))
})

test_that("Kolmogorov-Smirnov Curve", {
  expect_no_error(dx_plot_ks(dx_obj))
})

test_that("Precision-Recall Curve", {
  expect_no_error(dx_plot_pr(dx_obj))
})

test_that("Predictive Values Against Prevalence", {
  expect_no_error(dx_plot_predictive_value(dx_obj))
})

test_that("Calibration Curve", {
  expect_no_error(dx_plot_calibration(dx_obj))
})

test_that("Decision Curve", {
  expect_no_error(dx_plot_decision_curve(dx_obj))
})

test_that("Cumulative Accuracy Profile", {
  expect_no_error(dx_plot_cap(dx_obj))
})

test_that("Cost Curve", {
  expect_no_error(dx_plot_cost(dx_obj, cfp = 1000, cfn = 8000))
})

test_that("Confusion Matrix", {
  expect_no_error(dx_plot_cm(dx_obj))
})

test_that("ROC", {
  expect_no_error(dx_plot_roc(dx_obj))
})
