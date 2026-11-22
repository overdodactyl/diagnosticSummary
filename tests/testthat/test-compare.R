dx_glm <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  doboot = FALSE
)

dx_rf <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted_rf",
  setthreshold = .3,
  doboot = FALSE
)

set.seed(20)
dx_heart_failure$predicted_noise <- stats::runif(nrow(dx_heart_failure))

dx_noise <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted_noise",
  setthreshold = .3,
  doboot = FALSE
)


test_that("Z-test estimate is signed and within its CI", {
  z1 <- dx_z_test(dx_glm, dx_rf, metric = "sensitivity")
  z2 <- dx_z_test(dx_rf, dx_glm, metric = "sensitivity")

  expect_equal(z1$estimate, -z2$estimate)
  expect_true(z1$estimate >= z1$conf_low & z1$estimate <= z1$conf_high)
  expect_true(z2$estimate >= z2$conf_low & z2$estimate <= z2$conf_high)
})

test_that("DeLong estimate is signed and within its CI", {
  d1 <- dx_delong(dx_glm, dx_rf)
  d2 <- dx_delong(dx_rf, dx_glm)

  expect_equal(d1$estimate, -d2$estimate)
  expect_true(d1$estimate >= d1$conf_low & d1$estimate <= d1$conf_high)
  expect_true(d2$estimate >= d2$conf_low & d2$estimate <= d2$conf_high)
})

test_that("Compare all model pairs", {
  dx_list <- list(glm = dx_glm, rf = dx_rf, noise = dx_noise)
  pairs <- c("glm vs. rf", "glm vs. noise", "rf vs. noise")

  paired <- suppressWarnings(dx_compare(dx_list, paired = TRUE))
  expect_equal(unique(paired$tests$models), pairs)
  expect_equal(nrow(paired$tests), 2 * length(pairs))

  unpaired <- suppressWarnings(dx_compare(dx_list, paired = FALSE))
  expect_equal(unique(unpaired$tests$models), pairs)
  expect_equal(nrow(unpaired$tests), 9 * length(pairs))
})
