# Baseline of full outputs, used to detect unintended changes (e.g. during a
# refactor). Results were checked by the other tests before the baseline was
# saved; this only checks that they don't change.
#
# If a change is intended, review the differences and regenerate with:
#   devtools::load_all()
#   source("tests/testthat/helper-baseline.R")
#   saveRDS(baseline_outputs(), "tests/testthat/fixtures/baseline.rds")

baseline_outputs <- function() {
  hf <- dx_heart_failure

  fit <- function(...) {
    dx(data = hf, true_varname = "truth", pred_varname = "predicted", setthreshold = .3, ...)
  }
  as_df <- function(x) {
    x <- as.data.frame(x)
    rownames(x) <- NULL
    x
  }

  overall <- fit()
  grouped <- fit(grouping_variables = c("AgeGroup", "Sex", "AgeSex"))
  options <- fit(citype = "wilson", threshold_range = c(.1, .2, .5), prevalence = .1)
  boot <- fit(grouping_variables = "Sex", doboot = TRUE)
  pos0 <- fit(poslabel = 0)

  rf <- dx(data = hf, true_varname = "truth", pred_varname = "predicted_rf", setthreshold = .3)
  models <- list(glm = overall, rf = rf)

  list(
    overall_measures = as_df(overall$measures),
    overall_thresholds = as_df(overall$thresholds),
    overall_prevalence = as_df(overall$prevalence),
    overall_rank = as_df(overall$rank),
    overall_cm = as_df(overall$cm),
    grouped_measures = as_df(grouped$measures),
    options_measures = as_df(options$measures),
    boot_measures = as_df(boot$measures),
    poslabel0_measures = as_df(pos0$measures),
    compare_paired = as_df(dx_compare(models, paired = TRUE)$tests),
    compare_unpaired = as_df(suppressWarnings(dx_compare(models, paired = FALSE))$tests),
    forest_table = as_df(dx_prep_forest(
      grouped,
      fraction = TRUE,
      fraction_multiline = FALSE,
      measures = c("AUC ROC", "Sensitivity", "Specificity", "Odds Ratio", "Breslow-Day")
    ))
  )
}
