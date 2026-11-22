# Package index

## Construction

Generate a dx object

- [`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
  : Set options for diagnostic analysis
- [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md)
  : Create a Confusion Matrix from Predictions and Truth
- [`dx_compare()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_compare.md)
  : Compare Multiple Classification Models

## Metrics

Measures of binary classification tasks

- [`dx_accuracy()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_accuracy.md)
  : Calculate Accuracy
- [`dx_auc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc.md)
  : Calculate Area Under the ROC Curve (AUC)
- [`dx_auc_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_auc_pr.md)
  : Calculate Area Under the Precision-Recall Curve (AUC-PR)
- [`dx_balanced_accuracy()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_balanced_accuracy.md)
  : Calculate Balanced Accuracy
- [`dx_brier()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_brier.md)
  : Calculate Brier Score
- [`dx_cohens_kappa()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cohens_kappa.md)
  : Calculate Cohen's Kappa
- [`dx_detection_prevalence()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_detection_prevalence.md)
  : Calculate Detection Prevalence
- [`dx_f1()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_f1.md)
  : Calculate F1 Score with Confidence Intervals
- [`dx_f2()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_f2.md)
  : Calculate F2 Score with Confidence Intervals
- [`dx_fbeta()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fbeta.md)
  : Calculate F-beta Score with Confidence Intervals
- [`dx_fdr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fdr.md)
  : Calculate False Discovery Rate (FDR)
- [`dx_fowlkes_mallows()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fowlkes_mallows.md)
  : Calculate Fowlkes-Mallows Index
- [`dx_g_mean()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_g_mean.md)
  : Calculate G-mean
- [`dx_lrt_neg()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_lrt_neg.md)
  : Calculate Negative Likelihood Ratio
- [`dx_lrt_pos()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_lrt_pos.md)
  : Calculate Positive Likelihood Ratio
- [`dx_markedness()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_markedness.md)
  : Calculate Markedness
- [`dx_mcc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_mcc.md)
  : Calculate Matthews Correlation Coefficient (MCC)
- [`dx_nir()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_nir.md)
  : Calculate No Information Rate (NIR)
- [`dx_npv()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_npv.md)
  : Calculate Negative Predictive Value (NPV)
- [`dx_npv_prevalence()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_npv_prevalence.md)
  : Calculate Negative Predictive Value (NPV) at Target Prevalence
- [`dx_odds_ratio()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_odds_ratio.md)
  : Calculate Odds Ratio
- [`dx_ppv_prevalence()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_ppv_prevalence.md)
  : Calculate Positive Predictive Value (PPV) at Target Prevalence
- [`dx_prevalence()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_prevalence.md)
  : Calculate Prevalence
- [`dx_fnr()`](https://overdodactyl.github.io/diagnosticSummary/reference/fnr.md)
  [`dx_miss_rate()`](https://overdodactyl.github.io/diagnosticSummary/reference/fnr.md)
  : Calculate False Negative Rate (FNR)
- [`dx_fpr()`](https://overdodactyl.github.io/diagnosticSummary/reference/fpr.md)
  [`dx_fall_out()`](https://overdodactyl.github.io/diagnosticSummary/reference/fpr.md)
  : Calculate False Positive Rate (FPR)
- [`dx_informedness()`](https://overdodactyl.github.io/diagnosticSummary/reference/informedness.md)
  [`dx_youden_j()`](https://overdodactyl.github.io/diagnosticSummary/reference/informedness.md)
  : Calculate Informedness
- [`dx_ppv()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md)
  [`dx_precision()`](https://overdodactyl.github.io/diagnosticSummary/reference/ppv.md)
  : Calculate Positive Predictive Value (PPV, Precision)
- [`dx_sensitivity()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md)
  [`dx_recall()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md)
  [`dx_tpr()`](https://overdodactyl.github.io/diagnosticSummary/reference/sensitivity.md)
  : Calculate Sensitivity (True Positive Rate, Recall)
- [`dx_specificity()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md)
  [`dx_tnr()`](https://overdodactyl.github.io/diagnosticSummary/reference/specificity.md)
  : Calculate Specificity (True Negative Rate)

## Tests

Statistical tests

- [`breslow_day_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/breslow_day_test.md)
  : Breslow-Day Test for Homogeneity of Odds Ratios
- [`dx_chi_square()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_chi_square.md)
  : Chi-Square Test for Independence in a 2x2 Table
- [`dx_delong()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_delong.md)
  : DeLong's Test for Comparing Two ROC Curves
- [`dx_fishers_exact()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_fishers_exact.md)
  : Fisher's Exact Test for Independence in a 2x2 Table
- [`dx_g_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_g_test.md)
  : G-Test (Log-Likelihood Ratio Test) for Independence in 2x2 Table
- [`dx_mcnemars()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_mcnemars.md)
  : McNemar's Chi-squared Test for Paired Proportions
- [`dx_z_test()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_z_test.md)
  : Z-test for Comparing Two Proportions

## Figures

Figures summarizing diagnostic statistics

- [`dx_plot_calibration()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_calibration.md)
  : Plot Calibration Curve
- [`dx_plot_cap()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_cap.md)
  : Plot Cumulative Accuracy Profile (CAP) Curve
- [`dx_plot_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_cm.md)
  : Plot Confusion Matrix with Metrics
- [`dx_plot_cost()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_cost.md)
  : Plot Cost Curve
- [`dx_plot_decision_curve()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_decision_curve.md)
  : Plot Decision Curve
- [`dx_plot_forest()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_forest.md)
  : Create table with odds ratios displayed graphically
- [`dx_plot_gain()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_gain.md)
  : Plot Gain Chart
- [`dx_plot_ks()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_ks.md)
  : Plot Kolmogorov-Smirnov Curve
- [`dx_plot_lift()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_lift.md)
  : Plot Lift Curve
- [`dx_plot_pr()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_pr.md)
  : Plot Precision-Recall Curve
- [`dx_plot_predictive_value()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_predictive_value.md)
  : Plot Predictive Values Against Prevalence
- [`dx_plot_probabilities()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_probabilities.md)
  : Plot Predicted Probabilities
- [`dx_plot_roc()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_roc.md)
  : Plot ROC Curve
- [`dx_plot_rocs()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_rocs.md)
  : Plot ROC Curves for Multiple Models
- [`dx_plot_thresholds()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_thresholds.md)
  : Plot Diagnostic Measures across Thresholds
- [`dx_plot_youden_j()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_plot_youden_j.md)
  : Plot Youden's J Index Curve
- [`dx_edit_cell()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_edit_cell.md)
  : Edit a cell within an object returned from dx_forest

## Utilities

Helper functions and S3 methods

- [`as.data.frame(`*`<dx>`*`)`](https://overdodactyl.github.io/diagnosticSummary/reference/as.data.frame.dx.md)
  : Convert to a data frame
- [`summary(`*`<dx>`*`)`](https://overdodactyl.github.io/diagnosticSummary/reference/summary.dx.md)
  : Summary

## Data

Test data sets

- [`dx_heart_failure`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_heart_failure.md)
  : Heart attack outcomes and predictions
