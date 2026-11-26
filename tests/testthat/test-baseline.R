# Compares full outputs against a saved baseline to catch unintended changes.
# See helper-baseline.R for how to regenerate the baseline after an intended
# change.

test_that("Outputs match the saved baseline", {
  # Exact outputs can change with new versions of pROC or binom, or on other
  # platforms. The other tests check that results are correct.
  skip_on_cran()
  expected <- readRDS(test_path("fixtures", "baseline.rds"))
  current <- baseline_outputs()

  expect_setequal(names(current), names(expected))

  # Bootstrap draws from rmultinom() differ across platforms and R versions,
  # so bootstrapped CIs only need to agree up to Monte Carlo error (seeds 1 to
  # 100 moved them by at most 0.048 with 2000 replicates). Their
  # accuracy is checked against case resampling in test-references.R.
  boot <- expected$boot_measures$conf_type == "Bootstrapped"
  expect_true(any(boot))
  expect_equal(current$boot_measures$conf_type, expected$boot_measures$conf_type)
  for (bound in c("conf_low", "conf_high")) {
    diff <- abs(current$boot_measures[[bound]][boot] - expected$boot_measures[[bound]][boot])
    expect_true(all(diff < 0.075), info = paste(bound, "max difference", max(diff)))
  }
  boot_cols <- c("summary", "conf_low", "conf_high")
  current$boot_measures[boot, boot_cols] <- NA
  expected$boot_measures[boot, boot_cols] <- NA

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
