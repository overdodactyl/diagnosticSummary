test_that("Assemble dx object", {

  # Truth variable not in data set
  expect_error(
    dx(
      data = dx_heart_failure,
      true_varname = "not_truth",
      pred_varname = "predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "not_truth was not found in"
  )

  # Predicted variable not in data set
  expect_error(
    dx(
      data = dx_heart_failure,
      true_varname = "truth",
      pred_varname = "not_predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "not_predicted was not found in"
  )


  # pred variable should be numeric
  not_numeric <- dx_heart_failure
  not_numeric$predicted <- as.factor(not_numeric$predicted)
  expect_error(
    dx(
      data = not_numeric,
      true_varname = "truth",
      pred_varname = "predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "should be numeric"
  )

  # true variable should consist of only 0 and 1
  not_binary <- dx_heart_failure
  not_binary$truth <- sample(0:2, nrow(not_binary), replace = TRUE)
  expect_error(
    dx(
      data = not_binary,
      true_varname = "truth",
      pred_varname = "predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "consisting of only 0's and 1's"
  )

  # Make sure we get dx class
  expect_s3_class(
    dx(
      data = dx_heart_failure,
      true_varname = "truth",
      pred_varname = "predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "dx"
  )

})

fit_dx <- function(data) {
  dx(
    data = data,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3
  )
}

test_that("Missing values are an error", {
  na_pred <- dx_heart_failure
  na_pred$predicted[1:3] <- NA
  expect_error(fit_dx(na_pred), "predicted contains 3 missing value")

  na_truth <- dx_heart_failure
  na_truth$truth[1:2] <- NA
  expect_error(fit_dx(na_truth), "truth contains 2 missing value")
})

test_that("Predictions outside 0 to 1 are an error", {
  logits <- dx_heart_failure
  logits$predicted <- stats::qlogis(logits$predicted)
  expect_error(fit_dx(logits), "predicted probabilities between 0 and 1.*plogis")

  scores <- dx_heart_failure
  scores$predicted <- scores$predicted * 100
  expect_error(fit_dx(scores), "between 0 and 1")
})

test_that("Truth coded as 0/1 in other types gives the same results", {
  ref <- fit_dx(dx_heart_failure)$measures

  types <- list(
    logical = dx_heart_failure$truth == 1,
    factor = factor(dx_heart_failure$truth),
    character = as.character(dx_heart_failure$truth)
  )

  for (type in names(types)) {
    data <- dx_heart_failure
    data$truth <- types[[type]]
    expect_equal(fit_dx(data)$measures, ref, info = type)
  }

  not_binary <- dx_heart_failure
  not_binary$truth <- factor(not_binary$truth, labels = c("No", "Yes"))
  expect_error(fit_dx(not_binary), "consisting of only 0's and 1's")
})

test_that("Truth with only one class is an error", {
  all_neg <- dx_heart_failure
  all_neg$truth <- 0
  expect_error(fit_dx(all_neg), "should contain both 0's and 1's, but only contains 0's")
})

test_that("Identical predictions are an error", {
  constant <- dx_heart_failure
  constant$predicted <- .5
  expect_error(fit_dx(constant), "All values of predicted are identical")
})
