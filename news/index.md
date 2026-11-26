# Changelog

## diagnosticSummary 0.1.0

- Initial CRAN submission

## diagnosticSummary 0.0.2.20

**Changes that affect results**

- The estimate of a proportion is the observed proportion for every
  `citype`. With `citype = "bayes"` it was the posterior mean, which
  disagreed with the fraction shown beside it. Intervals are unchanged.

**Bug fixes**

- `prevalence`, `threshold_range`, and `grouping_variables` accept
  `NULL` or a missing value of any type, such as `NA_real_`, as not
  supplied, which gave an error in 0.0.2.19. Proportions with a
  denominator of 0 gave an error with `citype = "profile"` or
  `"prop.test"`, so
  [`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
  failed when no prediction was missing, and were reported as 50% with
  `"bayes"`.
  [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  gives a clear error when no odds ratio can be calculated, and
  [`dx_plot_decision_curve()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_decision_curve.md)
  with `poslabel = 0` could miss the first point of `plot_range`. Errors
  that name a function no longer print its code when it is called with
  [`do.call()`](https://rdrr.io/r/base/do.call.html).

**Dependencies**

- The plot functions give an error when ggplot2 is older than 3.4.0

## diagnosticSummary 0.0.2.19

**Breaking changes**

- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  and
  [`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
  now give an error for invalid options, which previously failed later
  with an unclear error or were silently ignored. `citype` must be a
  method of
  [`binom::binom.confint()`](https://rdrr.io/pkg/binom/man/binom.confint.html),
  `threshold_range` must hold numbers between 0 and 1, `prevalence` must
  be a single number greater than 0 and less than 1, `doboot` must be
  `TRUE` or `FALSE`, `bootreps` must be a whole number of at least 1,
  and `grouping_variables` must be unique column names in `data` other
  than “Overall”.
- [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md),
  [`dx_auc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc.md),
  and
  [`dx_brier()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_brier.md)
  now give an error when `truth` and `predprob` differ in length, have
  missing values, or `truth` isn’t 0/1.
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
  and
  [`dx_brier()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_brier.md)
  also require predictions between 0 and 1.
  [`dx_fbeta()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fbeta.md)
  requires `beta` to be a single positive number, and
  [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md),
  [`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md),
  and
  [`dx_mcnemars()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_mcnemars.md)
  require `detail` to be “simple” or “full”.
- [`breslow_day_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/breslow_day_test.md)
  requires a 2x2xK array of counts with at least two strata, and gives
  an error when a stratum has an empty row or column
- The `dx_plot_*` functions give an error when they aren’t given a `dx`
  object (a `dx_compare` object for
  [`dx_plot_rocs()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_rocs.md)),
  or when `plot_range`, `plot_type`, `summary_stats`, `bins`, `levels`,
  or log-scale `limits` are invalid

**Changes that affect results**

- The odds ratio and likelihood ratios are `NA` when every prediction is
  on one side of the threshold. Previously the continuity correction
  gave values that only reflected the number of cases in each class. A
  single empty cell is still corrected by adding 0.5 to each cell.
- [`dx_plot_decision_curve()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_decision_curve.md)
  calculates net benefit on an even grid of thresholds across
  `plot_range` and at each distinct prediction, instead of only at the
  predictions. The curve is now also drawn for models with few distinct
  predictions, such as 0/1 classes.
- The G-test is `NA` when every prediction is on one side of the
  threshold, matching the chi-squared test, and empty cells contribute
  exactly 0
- The kappa confidence interval uses the exact normal quantile instead
  of 1.96, and an undefined kappa is noted as undefined instead of
  “Slight Agreement”
- Proportions with a denominator of 0 have no confidence interval for
  any `citype`. Previously `"exact"` gave 0% to 100%.
- [`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md)
  returns an estimate of 0 and a p-value of `NA`, with a note, when both
  proportions are 0 or both are 1
- Numbers that round to zero are no longer shown with a minus sign

**Bug fixes**

- [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  gave an error with `trans = NA`, its documented default, when passed
  explicitly, and replaced a subgroup’s odds ratio with the Breslow-Day
  p-value when the subgroup had the same name as its variable.
  [`breslow_day_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/breslow_day_test.md)
  keeps precision for very small p-values and no longer names its
  results after a level of the table. The message for a missing
  suggested package no longer repeats “must be”.

**New features**

- [`print()`](https://rdrr.io/r/base/print.html) methods for `dx`,
  `dx_itd`, and `dx_compare` objects show the inputs and a few headline
  measures (or the table of tests) instead of the whole list

**Dependencies**

- The plots need ggplot2 \>= 3.4.0

## diagnosticSummary 0.0.2.18

**Dependencies**

- Requires R \>= 3.5.0

**Bug fixes**

- [`summary()`](https://rdrr.io/r/base/summary.html) for `dx` and
  `dx_itd` objects returns a data frame when knitr isn’t installed,
  instead of printing the table and returning it invisibly. The
  documentation for `dx_heart_failure` now cites its source, the
  Hungarian records in the UCI Heart Disease data set, in place of a
  dead link.

## diagnosticSummary 0.0.2.17

**Breaking changes**

- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  now gives an error unless `setthreshold` is a single number between 0
  and 1. Previously other values failed later with an unclear error or
  gave results with every case on one side of the threshold.

**New features**

- [`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)
  summarizes a diagnostic test when some predictions are missing, for
  example because a case failed quality control. Missing predictions are
  kept as ungradable results in a 3x2 intention-to-diagnose analysis
  instead of being excluded. It reports the diagnostic yield, pre-test
  disease risk, predictive values, post-test disease risk and likelihood
  ratio for each result, and worst-case sensitivity, specificity, and
  Youden’s J, which count ungradable results as incorrect.
  [`dx_itd_table()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd_table.md)
  lays the results out as a 3x2 table, and
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  are included.
- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  still gives an error for missing predictions, and the message now
  points to
  [`dx_itd()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd.md)

## diagnosticSummary 0.0.2.16

**Breaking changes**

- [`dx_auc_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc_pr.md)
  now takes `truth` and `predprob` (plus `detail` and `poslabel`)
  instead of `precision` and `recall`, and checks its inputs, so calls
  with the old arguments give an error
- The chi-squared measure is renamed from “Pearson’s Chi-squared” to
  “Pearson’s Chi-squared (Yates’ correction)”. Code that filters results
  on the old name needs updating.
- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  no longer adds `test_binary`, `test_binaryf`, and `true_binaryf`
  columns to `data`. Previously it overwrote user columns with these
  names, so a truth column named `test_binary` gave a perfect model.

**Deprecated**

- `classlabels` and `outcome_label` in
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  have no effect and now give a warning. They are no longer stored in
  `options`.
- `direction` values other than `"<"` are deprecated. Predictions are
  probabilities that the outcome is 1; use `poslabel` to choose the
  event.

**Changes that affect results**

- `poslabel = 0` is now used by the rank table and the ROC, gain, lift,
  KS, CAP, and decision curves, which previously always treated 1 as the
  event and gave a warning. Net benefit for `poslabel = 0` now weights
  by the event’s threshold probability, 1 minus the threshold.
- AUC-PR and the PR curve use Davis & Goadrich (2006) interpolation
  instead of straight lines between points. Values change slightly.
- The kappa confidence interval uses the Fleiss, Cohen & Everitt (1969)
  variance
- Bootstrapped confidence intervals resample confusion matrix cell
  counts, which is equivalent to resampling cases and much faster.
  Intervals differ by Monte Carlo error at the same `bootseed`, can
  differ across platforms and R versions, and the default `bootreps` in
  the metric functions is now 2000, matching
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md).
- With tied predictions, gain, lift, and CAP curves only pass through
  points a threshold can reach, and gain and CAP curves start at the
  origin. `rank` has a new `reachable` column.
- Agresti-Coull confidence intervals are truncated to 0 to 1
- Estimates without a confidence interval are shown with a fixed number
  of decimals (3 for the Brier score and AUC-PR), and the no information
  rate is shown as a percent

**Bug fixes**

- Measures in
  [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  could disagree with `cm` when `setthreshold` equaled a prediction, and
  `thresholds` had duplicate rows when predictions included exactly 0
  or 1. The Breslow-Day test and paired
  [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md)
  now return `NA` with a note instead of an error when they are
  undefined (every subgroup separates perfectly, or a class has one
  observation).

**New features**

- [`dx_recall()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md),
  [`dx_tpr()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md),
  [`dx_tnr()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md),
  [`dx_precision()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md),
  [`dx_miss_rate()`](https://overdodactyl.github.io/diagnosticSummary/reference/fnr.md),
  [`dx_fall_out()`](https://overdodactyl.github.io/diagnosticSummary/reference/fpr.md),
  and
  [`dx_youden_j()`](https://overdodactyl.github.io/diagnosticSummary/reference/informedness.md)
  are now exported, as documented
- `thresholds` has a new `event_threshold` column, the threshold as a
  probability of the event
- [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md)
  notes say whether a Z or t statistic was used
- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  is much faster for large data sets, especially with `doboot = TRUE`.
  caret and broom are no longer suggested packages.

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
