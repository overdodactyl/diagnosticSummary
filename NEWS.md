# diagnosticSummary 0.0.2.15

**Breaking changes**

* `dx()` now stops with an error if the prediction or truth variable has missing values, or if predictions are outside 0 to 1. Previously these gave unclear errors or, for predictions outside 0 to 1, results such as a meaningless Brier score.
* `direction` in `dx()` and `dx_auc()` now defaults to `"<"` (higher predictions indicate the positive class) instead of `"auto"`. Models that perform worse than chance now report an AUC below 0.5 instead of a flipped AUC. The AUC in `dx()` measures now also respects `direction`.
* Bootstrapped confidence intervals now use `bootseed`, which was previously ignored, so their values will change. Results are now reproducible regardless of the session's random seed.

**Bug fixes that change results**

* The net benefit in `dx_plot_decision_curve()` (and the `net_benefit` column of `thresholds`) used the wrong formula, which overstated the model's benefit relative to treating all or none. It now uses the standard formula from Vickers & Elkin (2006).
* The KS statistic in `dx_plot_ks()` was inflated when predictions were tied
* Cohen's kappa was `NA` for data sets with more than about 46,000 rows due to integer overflow, and its confidence interval could extend beyond -1 to 1
* Subgroups with only one outcome class or very few rows no longer cause `dx()` to fail. Measures that are undefined for these subgroups (AUC, odds ratio, likelihood ratios, chi-square) are `NA`, with a warning, and the Breslow-Day test only uses subgroups with both outcome classes and both predicted classes.

**Other bug fixes**

* Subgroups are ordered by factor level, unused factor levels are dropped, and `as.data.frame()` and `summary()` can filter on several values. Bootstrapped measures are displayed as decimals instead of percentages. `dx_plot_roc()` and `dx_plot_forest()` no longer give ggplot2 warnings, forest plots style subgroups with a missing odds ratio correctly, and `dx()` and `dx_plot_forest()` no longer change the session's random seed. `dx_compare()` no longer mislabels models when only some are named, and `summary()` gives a clear error when given several thresholds.


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



