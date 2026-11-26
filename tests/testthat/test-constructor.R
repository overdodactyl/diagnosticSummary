test_that("Assemble dx object", {

  # Truth variable not in data set
  expect_error(
    dx(
      data = dx_heart_failure,
      true_varname = "not_truth",
      pred_varname = "predicted",
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

test_that("Options are checked before any results are calculated", {
  fit <- function(...) {
    dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted", ...)
  }

  # citype is one method of binom::binom.confint(), not "all" or several
  for (bad in list("all", "bogus", c("exact", "wilson"), NA, 1)) {
    expect_error(fit(citype = bad), "citype should be one of: exact, ac, asymptotic")
  }
  # Partial and alternative names that binom accepts still work
  expect_equal(fit(citype = "wil")$measures$estimate, fit(citype = "wilson")$measures$estimate)
  expect_equal(fit(citype = "agresti-coull")$measures$estimate, fit(citype = "ac")$measures$estimate)

  for (bad in list(c(-1, .5), c(.5, 2), "a", c(.2, NA))) {
    expect_error(fit(threshold_range = bad), "threshold_range should be a numeric vector of values between 0 and 1")
  }
  expect_equal(unique(fit(threshold_range = NULL)$measures$threshold), .5)
  expect_equal(unique(fit(threshold_range = c(0, 1))$measures$threshold), c(.5, 0, 1))

  for (bad in list(0, 1, 1.5, -.1, c(.1, .2), "a")) {
    expect_error(fit(prevalence = bad), "prevalence should be a single number greater than 0 and less than 1")
  }

  for (bad in list(NA, "yes", c(TRUE, FALSE), 1)) {
    expect_error(fit(doboot = bad), "doboot should be TRUE or FALSE")
  }
  for (bad in list(0, -5, 10.5, NA, c(10, 20), "100")) {
    expect_error(fit(doboot = TRUE, bootreps = bad), "bootreps should be a single whole number of at least 1")
  }
  # bootreps is only used when bootstrapping
  expect_s3_class(fit(bootreps = 0), "dx")
  expect_s3_class(fit(doboot = TRUE, bootreps = 1), "dx")

  for (bad in list(NA, "up", c("<", ">"), 1)) {
    expect_error(fit(direction = bad), "direction should be")
  }
})

test_that("Grouping variables are checked", {
  fit <- function(data = dx_heart_failure, ...) {
    dx(data = data, true_varname = "truth", pred_varname = "predicted", ...)
  }

  expect_error(
    fit(grouping_variables = c("Sex", "nope", "nada")),
    "grouping_variables not found in `data`: nope, nada"
  )
  expect_error(
    fit(grouping_variables = c("Sex", "AgeGroup", "Sex")),
    "grouping_variables should not contain duplicates: Sex"
  )
  expect_error(fit(grouping_variables = 1), "should be a character vector of column names")
  expect_error(fit(grouping_variables = c("Sex", NA)), "should be a character vector of column names")

  # "Overall" labels the results for all cases
  overall <- dx_heart_failure
  overall$Overall <- overall$Sex
  expect_error(fit(overall, grouping_variables = "Overall"), "should not include a column named \"Overall\"")
  # A column named Overall is fine if it isn't a grouping variable
  expect_s3_class(fit(overall, grouping_variables = "Sex"), "dx")
})
