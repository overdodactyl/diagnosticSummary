# diagnosticSummary 0.0.2.17

**Breaking changes**

* `dx()` now gives an error unless `setthreshold` is a single number between 0 and 1. Previously other values failed later with an unclear error or gave results with every case on one side of the threshold.

**New features**

* `dx_itd()` summarizes a diagnostic test when some predictions are missing, for example because a case failed quality control. Missing predictions are kept as ungradable results in a 3x2 intention-to-diagnose analysis instead of being excluded. It reports the diagnostic yield, pre-test disease risk, predictive values, post-test disease risk and likelihood ratio for each result, and worst-case sensitivity, specificity, and Youden's J, which count ungradable results as incorrect. `dx_itd_table()` lays the results out as a 3x2 table, and `summary()` and `as.data.frame()` methods are included.
* `dx()` still gives an error for missing predictions, and the message now points to `dx_itd()`

# diagnosticSummary 0.0.2.16

**Breaking changes**

* `dx_auc_pr()` now takes `truth` and `predprob` (plus `detail` and `poslabel`) instead of `precision` and `recall`, and checks its inputs, so calls with the old arguments give an error
* The chi-squared measure is renamed from "Pearson's Chi-squared" to "Pearson's Chi-squared (Yates' correction)". Code that filters results on the old name needs updating.
* `dx()` no longer adds `test_binary`, `test_binaryf`, and `true_binaryf` columns to `data`. Previously it overwrote user columns with these names, so a truth column named `test_binary` gave a perfect model.

**Deprecated**

* `classlabels` and `outcome_label` in `dx()` have no effect and now give a warning. They are no longer stored in `options`.
* `direction` values other than `"<"` are deprecated. Predictions are probabilities that the outcome is 1; use `poslabel` to choose the event.

**Changes that affect results**

* `poslabel = 0` is now used by the rank table and the ROC, gain, lift, KS, CAP, and decision curves, which previously always treated 1 as the event and gave a warning. Net benefit for `poslabel = 0` now weights by the event's threshold probability, 1 minus the threshold.
* AUC-PR and the PR curve use Davis & Goadrich (2006) interpolation instead of straight lines between points. Values change slightly.
* The kappa confidence interval uses the Fleiss, Cohen & Everitt (1969) variance
* Bootstrapped confidence intervals resample confusion matrix cell counts, which is equivalent to resampling cases and much faster. Intervals differ by Monte Carlo error at the same `bootseed`, can differ across platforms and R versions, and the default `bootreps` in the metric functions is now 2000, matching `dx()`.
* With tied predictions, gain, lift, and CAP curves only pass through points a threshold can reach, and gain and CAP curves start at the origin. `rank` has a new `reachable` column.
* Agresti-Coull confidence intervals are truncated to 0 to 1
* Estimates without a confidence interval are shown with a fixed number of decimals (3 for the Brier score and AUC-PR), and the no information rate is shown as a percent

**Bug fixes**

* Measures in `dx()` could disagree with `cm` when `setthreshold` equaled a prediction, and `thresholds` had duplicate rows when predictions included exactly 0 or 1. The Breslow-Day test and paired `dx_delong()` now return `NA` with a note instead of an error when they are undefined (every subgroup separates perfectly, or a class has one observation).

**New features**

* `dx_recall()`, `dx_tpr()`, `dx_tnr()`, `dx_precision()`, `dx_miss_rate()`, `dx_fall_out()`, and `dx_youden_j()` are now exported, as documented
* `thresholds` has a new `event_threshold` column, the threshold as a probability of the event
* `dx_delong()` notes say whether a Z or t statistic was used
* `dx()` is much faster for large data sets, especially with `doboot = TRUE`. caret and broom are no longer suggested packages.


# diagnosticSummary 0.0.2.15

**Breaking changes**

* `dx()` now stops with an error if the prediction or truth variable has missing values, or if predictions are outside 0 to 1. Previously these gave unclear errors or, for predictions outside 0 to 1, results such as a meaningless Brier score.
* `direction` in `dx()` and `dx_auc()` now defaults to `"<"` (higher predictions indicate the positive class) instead of `"auto"`. Models that perform worse than chance now report an AUC below 0.5 instead of a flipped AUC. The AUC in `dx()` measures now also respects `direction`.
* Bootstrapped confidence intervals now use `bootseed`, which was previously ignored, so their values will change. Results are now reproducible regardless of the session's random seed.
* `poslabel` must be 0 or 1, and `dx_compare()` requires each model to have a unique name. Previously other values ran silently with wrong results.
* `direction` only affects the AUC and ROC curve, so `dx()` now warns when it isn't `"<"`, and plots that always treat 1 as the positive class (gain, lift, KS, CAP, and decision curves) warn when `poslabel = 0`.

**Bug fixes that change results**

* The net benefit in `dx_plot_decision_curve()` (and the `net_benefit` column of `thresholds`) used the wrong formula, which overstated the model's benefit relative to treating all or none. It now uses the standard formula from Vickers & Elkin (2006).
* The KS statistic in `dx_plot_ks()` was inflated when predictions were tied
* Cohen's kappa was `NA` for data sets with more than about 46,000 rows due to integer overflow, and its confidence interval could extend beyond -1 to 1
* Subgroups with only one outcome class or very few rows no longer cause `dx()` to fail. Measures that are undefined for these subgroups (AUC, odds ratio, likelihood ratios, chi-square, Fisher's exact, and G-tests) are `NA`, with a warning, and the Breslow-Day test only uses subgroups with both outcome classes and both predicted classes.
* `dx_plot_forest()` placed axis ticks on a different scale from the odds ratios when breaks did not span the axis limits, and gave incorrect labels for `trans = "log10"` with `breaks`. Breaks are now always given as odds ratios.
* `dx_plot_roc()` shaded less than the AUC when predictions were tied, and its curve did not match the AUC when `direction` was not `"<"`. The curve now comes from the same ROC object as the AUC.
* Thresholds created with `seq()` were duplicated in the results and could not be selected with `as.data.frame()` or `summary()`

**Other bug fixes**

* Subgroups are ordered by factor level, unused factor levels are dropped, and `as.data.frame()` and `summary()` can filter on several values. Bootstrapped measures are displayed as decimals instead of percentages. `dx_plot_roc()` and `dx_plot_forest()` no longer give ggplot2 warnings, forest plots style subgroups with a missing odds ratio correctly, and `dx()` and `dx_plot_forest()` no longer change the session's random seed. `dx_compare()` no longer mislabels models when only some are named, and `summary()` gives a clear error when given several thresholds.
* `dx()` no longer fails with column names that contain spaces or with large subgroups (integer overflow in the Breslow-Day test), and unpaired `dx_compare()` no longer fails when a model has no predicted positives or negatives. `dx_plot_pr()` keeps its end point, `dx_plot_forest()` gives a clear error without grouping variables, and kappa of 1 is interpreted as almost perfect agreement.


# diagnosticSummary 0.0.2.14

**Bug fixes**

* `dx_compare()` now compares all pairs of models instead of only the first pair, and labels each row with its own pair
* `dx_z_test()` and `dx_delong()` now report the signed difference (`dx1` minus `dx2`) so the estimate matches its confidence interval. Previously the absolute difference was reported, so estimates may now be negative.
* `dx_compare()` results now keep `estimate`, `p_value`, and `statistic` as numeric columns
* Unpaired `dx_delong()` summaries no longer show an empty confidence interval


# diagnosticSummary 0.0.2.13

**Bug fixes**

* Subgroup analyses no longer include rows where the grouping variable is missing


# diagnosticSummary 0.0.2.12

**New metrics**

* Added `dx_npv_prevalence` and `dx_ppv_prevalence` to calculate NPV and PPV at a specified prevalence
* New `prevalence` argument in `dx` adds prevalence-adjusted NPV and PPV to the output

**Bug fixes**

* `dx_plot_forest` keeps measures in the requested order


# diagnosticSummary 0.0.2.11

**Bug fixes**

* Fixed a row bind warning in `dx_plot_forest` with newer versions of R


# diagnosticSummary 0.0.2.10

**Changes**

* Variables in `grouping_variables` are converted to factors instead of throwing an error

**Bug fixes**

* PPV in `thresholds` output was calculated using NPV


# diagnosticSummary 0.0.2.00

**New metrics**

* Prevalence
* False Negative Rate
* False Positive Rate
* False Discovery Rate
* PR AUC
* Cohen's Kappa
* Matthews Correlation Coefficient
* Balanced Accuracy
* F-beta
* F2
* Informedness
* Markedness
* G-mean
* Fowlkes-Mallows Index
* Brier Score
* Detection Prevalence
* No Information Rate (NIR)


*Also added aliases for metrics with the same name (e.g., recall and true positive rate for sensitivity)* 

**New/modified tests**

* Breslow and Day test implemented within package
* Added Chi-Square Test
* Added Fisher's Exact Test 
* Added G-Test (Log-Likelihood Ratio Test)
* Added McNemar's Chi-squared Test for Paired Proportions
* Added Z-test for Comparing Two Proportions
* Added DeLong's Test for Comparing Two ROC Curves


**New output**

* Added `thresholds` to the output of `dx` containing metrics across a full range of thresholds
* Added `prevalence` to the output of `dx` containing metrics across a full range of prevalencies
* Added `rank` to the output of `dx` containing rank-based summaries from predicted probabilities

**Changes to `dx_obj$measures` output**

* `rawestime` column renamed to `estimate`
* `rawuci` and `rawlci` column renamed to `conf_low` and `conf_high`

**New plots**

* Added lift curve: `dx_plot_lift`
* Added Kolmogorov-Smirnov plot: `dx_plot_ks`
* Added cumulative gains chart: `dx_plot_gain`
* Added Precision-Recall plot: `dx_plot_pr`
* Added NPV and PPV across prevalencies: `dx_plot_predictive_value`
* Added calibration curve: `dx_plot_calibration`
* Added decision curve: `dx_plot_decision_curve`
* Added Youden's J Index Curve: `dx_plot_youden_j`
* Added Cumulative Accuracy Profile (CAP) curve: `dx_plot_cap`
* Added cost curve: `dx_plot_cost`
* Added plot showing metrics across thresholds: `dx_plot_thresholds`
* Add ability to plot multiple ROC curves: `dx_plot_rocs`

**Renamed Functions**

* The confusion matrix plot was renamed from `dx_cm` to `dx_plot_cm` (`dx_cm` now constructs the confusion matrix)
* `dx_roc` was renamed `dx_plot_roc`
* `dx_forest` was renamed `dx_plot_forest`

**Bug Fixes**

* Breslow-Day test no longer silently left out if DescTools not installed
* `levels` argument in `as.data.frame` and `summary` didn't filter to `label` column
* Matthews Correlation Coefficient no longer overflows with large confusion matrix counts
* Chi-square test was included twice in `dx` measures

**Dependencies**

* Removed `dplyr`, `scales`, `boot`, and `tidyr` from Imports
* Moved `gtable`, `grid`, and `gridExtra` to Suggests
* Removed `DescTools` and `e1071` from Suggests
* Added `tibble` to Suggests

**Comparison of dx objects**

* New `dx_compare` function to run pairwise tests on a list of `dx_objects`

**Documentation**

* Added a `NEWS.md` file to track changes to the package.
* Added documentation/descriptions/details to all functions
* Migrated pkgdown website to bootstrap 5



