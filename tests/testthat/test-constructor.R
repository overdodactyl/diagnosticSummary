test_that("Assemble dx object", {

  # Truth variable not in data set
  expect_error(
    dx(
      data = dx_heart_failure,
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
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
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
      true_varname = "truth",
      pred_varname = "not_predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "not_predicted was not found in"
  )

  # Grouping variable not a factor
  not_factor <- dx_heart_failure
  not_factor$AgeGroup <- as.character(not_factor$AgeGroup)
  expect_error(
    dx(
      data = not_factor,
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
      true_varname = "truth",
      pred_varname = "predicted",
      outcome_label = "Heart Attack",
      threshold_range = c(.1, .2, .3),
      setthreshold = .3,
      grouping_variables = c("AgeGroup", "Sex", "AgeSex")
    ),
    "should be a factor"
  )

  # pred variable should be numeric
  not_numeric <- dx_heart_failure
  not_numeric$predicted <- as.factor(not_numeric$predicted)
  expect_error(
    dx(
      data = not_numeric,
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
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
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
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
      study_name = "Heart Attack Prediction",
      data_description = "Validation Data",
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
