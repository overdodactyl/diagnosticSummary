library(dplyr)

test_that("Generate Confidence Intervals", {
  est <- .123234213
  high <- .89123423
  low <- .24252345
  c1 <- conf_int(est, low, high)

  # Default Settings
  expect_equal(
    conf_int(est, low, high),
    "0.1 (0.2, 0.9)"
  )

  # Accuracy
  expect_equal(
    conf_int(est, low, high, accuracy = .01),
    "0.12 (0.24, 0.89)"
  )

  # Percent
  expect_equal(
    conf_int(est, low, high, accuracy = .01, percent = TRUE),
    "12.32% (24.25%, 89.12%)"
  )
})


# Generat comparison to caret package -------------------------------------


# Caret results
pred <- ifelse(dx_heart_failure$predicted < .3, 0, 1)
truth <- dx_heart_failure$truth
pred <- as.factor(pred)
truth <- as.factor(truth)
xtab <- table(pred, truth)

caret_results <- caret::confusionMatrix(xtab, positive = "1")
caret_results <- broom::tidy(caret_results)

# dx results
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  threshold_range = c(.1, .2, .3),
  setthreshold = .3,
  grouping_variables = c("AgeGroup", "Sex", "AgeSex")
)

dx_res <- as.data.frame(dx_obj, variable = "Overall", thresh = .3)


test_that("Accuracy", {
  caret_accuracy <- caret_results %>% filter(term == "accuracy")
  caret_ac_raw <- caret_accuracy %>% pull(estimate)
  caret_ac_low <- caret_accuracy %>% pull(conf.low)
  caret_ac_high <- caret_accuracy %>% pull(conf.high)

  dx_acc <- dx_res %>% filter(measure == "Accuracy")
  dx_ac_raw <- dx_acc %>% pull(rawestime)
  dx_ac_low <- dx_acc %>% pull(rawlci)
  dx_ac_high <- dx_acc %>% pull(rawuci)

  expect_equal(dx_ac_raw, caret_ac_raw)
  expect_equal(dx_ac_low, caret_ac_low)
  expect_equal(dx_ac_high, caret_ac_high)
})

get_measures <- function(dx, caret) {
  caret_res <- caret_results %>%
    filter(term == caret) %>%
    pull(estimate)

  dx_res <- dx_res %>%
    filter(measure == dx) %>%
    pull(rawestime)

  testthat::expect_equal(dx_res, caret_res)
}

test_that("Sensitivity", {
  get_measures("Sensitivity", "sensitivity")
})

test_that("Specificty", {
  get_measures("Specificity", "specificity")
})

test_that("PPV", {
  get_measures("Positive Predictive Value", "pos_pred_value")
})

test_that("NPV", {
  get_measures("Negative Predictive Value", "neg_pred_value")
})

test_that("F1", {
  get_measures("F1 Score", "f1")
})
