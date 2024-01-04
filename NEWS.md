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



