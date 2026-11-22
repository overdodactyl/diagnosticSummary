# Changelog

## diagnosticSummary 0.0.2.15

**Breaking changes**

- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  now stops with an error if the prediction or truth variable has
  missing values, or if predictions are outside 0 to 1. Previously these
  gave unclear errors or, for predictions outside 0 to 1, results such
  as a meaningless Brier score.
- `direction` in
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  and
  [`dx_auc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc.md)
  now defaults to `"<"` (higher predictions indicate the positive class)
  instead of `"auto"`. Models that perform worse than chance now report
  an AUC below 0.5 instead of a flipped AUC. The AUC in
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  measures now also respects `direction`.
- Bootstrapped confidence intervals now use `bootseed`, which was
  previously ignored, so their values will change. Results are now
  reproducible regardless of the session’s random seed.
- `poslabel` must be 0 or 1, and
  [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  requires each model to have a unique name. Previously other values ran
  silently with wrong results.
- `direction` only affects the AUC and ROC curve, so
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  now warns when it isn’t `"<"`, and plots that always treat 1 as the
  positive class (gain, lift, KS, CAP, and decision curves) warn when
  `poslabel = 0`.

**Bug fixes that change results**

- The net benefit in
  [`dx_plot_decision_curve()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_decision_curve.md)
  (and the `net_benefit` column of `thresholds`) used the wrong formula,
  which overstated the model’s benefit relative to treating all or none.
  It now uses the standard formula from Vickers & Elkin (2006).
- The KS statistic in
  [`dx_plot_ks()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_ks.md)
  was inflated when predictions were tied
- Cohen’s kappa was `NA` for data sets with more than about 46,000 rows
  due to integer overflow, and its confidence interval could extend
  beyond -1 to 1
- Subgroups with only one outcome class or very few rows no longer cause
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  to fail. Measures that are undefined for these subgroups (AUC, odds
  ratio, likelihood ratios, chi-square, Fisher’s exact, and G-tests) are
  `NA`, with a warning, and the Breslow-Day test only uses subgroups
  with both outcome classes and both predicted classes.
- [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  placed axis ticks on a different scale from the odds ratios when
  breaks did not span the axis limits, and gave incorrect labels for
  `trans = "log10"` with `breaks`. Breaks are now always given as odds
  ratios.
- [`dx_plot_roc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_roc.md)
  shaded less than the AUC when predictions were tied, and its curve did
  not match the AUC when `direction` was not `"<"`. The curve now comes
  from the same ROC object as the AUC.
- Thresholds created with [`seq()`](https://rdrr.io/r/base/seq.html)
  were duplicated in the results and could not be selected with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or
  [`summary()`](https://rdrr.io/r/base/summary.html)

**Other bug fixes**

- Subgroups are ordered by factor level, unused factor levels are
  dropped, and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) can filter on
  several values. Bootstrapped measures are displayed as decimals
  instead of percentages.
  [`dx_plot_roc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_roc.md)
  and
  [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  no longer give ggplot2 warnings, forest plots style subgroups with a
  missing odds ratio correctly, and
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  and
  [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  no longer change the session’s random seed.
  [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  no longer mislabels models when only some are named, and
  [`summary()`](https://rdrr.io/r/base/summary.html) gives a clear error
  when given several thresholds.
- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  no longer fails with column names that contain spaces or with large
  subgroups (integer overflow in the Breslow-Day test), and unpaired
  [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  no longer fails when a model has no predicted positives or negatives.
  [`dx_plot_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_pr.md)
  keeps its end point,
  [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  gives a clear error without grouping variables, and kappa of 1 is
  interpreted as almost perfect agreement.

## diagnosticSummary 0.0.2.14

**Bug fixes**

- [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  now compares all pairs of models instead of only the first pair, and
  labels each row with its own pair
- [`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md)
  and
  [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md)
  now report the signed difference (`dx1` minus `dx2`) so the estimate
  matches its confidence interval. Previously the absolute difference
  was reported, so estimates may now be negative.
- [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  results now keep `estimate`, `p_value`, and `statistic` as numeric
  columns
- Unpaired
  [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md)
  summaries no longer show an empty confidence interval

## diagnosticSummary 0.0.2.13

**Bug fixes**

- Subgroup analyses no longer include rows where the grouping variable
  is missing

## diagnosticSummary 0.0.2.12

**New metrics**

- Added `dx_npv_prevalence` and `dx_ppv_prevalence` to calculate NPV and
  PPV at a specified prevalence
- New `prevalence` argument in `dx` adds prevalence-adjusted NPV and PPV
  to the output

**Bug fixes**

- `dx_plot_forest` keeps measures in the requested order

## diagnosticSummary 0.0.2.11

**Bug fixes**

- Fixed a row bind warning in `dx_plot_forest` with newer versions of R

## diagnosticSummary 0.0.2.10

**Changes**

- Variables in `grouping_variables` are converted to factors instead of
  throwing an error

**Bug fixes**

- PPV in `thresholds` output was calculated using NPV

## diagnosticSummary 0.0.2.00

**New metrics**

- Prevalence
- False Negative Rate
- False Positive Rate
- False Discovery Rate
- PR AUC
- Cohen’s Kappa
- Matthews Correlation Coefficient
- Balanced Accuracy
- F-beta
- F2
- Informedness
- Markedness
- G-mean
- Fowlkes-Mallows Index
- Brier Score
- Detection Prevalence
- No Information Rate (NIR)

*Also added aliases for metrics with the same name (e.g., recall and
true positive rate for sensitivity)*

**New/modified tests**

- Breslow and Day test implemented within package
- Added Chi-Square Test
- Added Fisher’s Exact Test
- Added G-Test (Log-Likelihood Ratio Test)
- Added McNemar’s Chi-squared Test for Paired Proportions
- Added Z-test for Comparing Two Proportions
- Added DeLong’s Test for Comparing Two ROC Curves

**New output**

- Added `thresholds` to the output of `dx` containing metrics across a
  full range of thresholds
- Added `prevalence` to the output of `dx` containing metrics across a
  full range of prevalencies
- Added `rank` to the output of `dx` containing rank-based summaries
  from predicted probabilities

**Changes to `dx_obj$measures` output**

- `rawestime` column renamed to `estimate`
- `rawuci` and `rawlci` column renamed to `conf_low` and `conf_high`

**New plots**

- Added lift curve: `dx_plot_lift`
- Added Kolmogorov-Smirnov plot: `dx_plot_ks`
- Added cumulative gains chart: `dx_plot_gain`
- Added Precision-Recall plot: `dx_plot_pr`
- Added NPV and PPV across prevalencies: `dx_plot_predictive_value`
- Added calibration curve: `dx_plot_calibration`
- Added decision curve: `dx_plot_decision_curve`
- Added Youden’s J Index Curve: `dx_plot_youden_j`
- Added Cumulative Accuracy Profile (CAP) curve: `dx_plot_cap`
- Added cost curve: `dx_plot_cost`
- Added plot showing metrics across thresholds: `dx_plot_thresholds`
- Add ability to plot multiple ROC curves: `dx_plot_rocs`

**Renamed Functions**

- The confusion matrix plot was renamed from `dx_cm` to `dx_plot_cm`
  (`dx_cm` now constructs the confusion matrix)
- `dx_roc` was renamed `dx_plot_roc`
- `dx_forest` was renamed `dx_plot_forest`

**Bug Fixes**

- Breslow-Day test no longer silently left out if DescTools not
  installed
- `levels` argument in `as.data.frame` and `summary` didn’t filter to
  `label` column
- Matthews Correlation Coefficient no longer overflows with large
  confusion matrix counts
- Chi-square test was included twice in `dx` measures

**Dependencies**

- Removed `dplyr`, `scales`, `boot`, and `tidyr` from Imports
- Moved `gtable`, `grid`, and `gridExtra` to Suggests
- Removed `DescTools` and `e1071` from Suggests
- Added `tibble` to Suggests

**Comparison of dx objects**

- New `dx_compare` function to run pairwise tests on a list of
  `dx_objects`

**Documentation**

- Added a `NEWS.md` file to track changes to the package.
- Added documentation/descriptions/details to all functions
- Migrated pkgdown website to bootstrap 5
