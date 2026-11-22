# Compares full outputs against a saved baseline to catch unintended changes.
# See helper-baseline.R for how to regenerate the baseline after an intended
# change.

test_that("Outputs match the saved baseline", {
  expected <- readRDS(test_path("fixtures", "baseline.rds"))
  current <- baseline_outputs()

  expect_setequal(names(current), names(expected))
  for (name in names(expected)) {
    expect_equal(current[[name]], expected[[name]], tolerance = 1e-8, info = name)
  }
})

test_that("Printed summaries match the saved snapshot", {
  skip_if_not_installed("knitr")
  # Snapshots need testthat's 3rd edition; the rest of the suite uses the 2nd
  local_edition(3)

  grouped <- dx(
    data = dx_heart_failure,
    true_varname = "truth",
    pred_varname = "predicted",
    setthreshold = .3,
    grouping_variables = c("AgeGroup", "Sex")
  )

  expect_snapshot(print(summary(grouped)))
  expect_snapshot(print(summary(grouped, variable = "Sex", show_var = FALSE)))
})
