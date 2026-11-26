# Summarize a diagnostic test

Calculates diagnostic accuracy measures, with confidence intervals, for
predicted probabilities compared to a binary reference standard:
overall, at each threshold in `threshold_range`, and for each subgroup
in `grouping_variables`.

## Usage

``` r
dx(
  data,
  classlabels = c("Negative", "Positive"),
  threshold_range = NA,
  outcome_label = NA,
  pred_varname,
  true_varname,
  setthreshold = 0.5,
  poslabel = 1,
  grouping_variables = NA,
  prevalence = NA,
  citype = "exact",
  bootreps = 2000,
  bootseed = 20191015,
  doboot = FALSE,
  direction = "<",
  ...
)
```

## Arguments

- data:

  A tbl.

- classlabels:

  Deprecated; has no effect.

- threshold_range:

  Optional. A numeric vector of thresholds between 0 and 1 to loop over.

- outcome_label:

  Deprecated; has no effect.

- pred_varname:

  Column name containing the predictions (string). Values should be
  predicted probabilities that the outcome is 1, between 0 and 1, with
  no missing values.

- true_varname:

  Column name containing the reference standard (string). Values should
  be 0 or 1 (numeric, logical, factor, or character), with no missing
  values, and both classes must be present.

- setthreshold:

  A number between 0 and 1 representing the threshold used to classify
  the predictions

- poslabel:

  The outcome class that is the event: 1 (default) or 0. Predictions are
  still probabilities that the outcome is 1, so with `poslabel = 0` a
  prediction below the threshold is a positive test.

- grouping_variables:

  Character vector of variable names to be summarized by. Variables are
  converted to factors if not already one. A column can't be named
  "Overall", which is used for the results of all cases.

- prevalence:

  A single number greater than 0 and less than 1, representing a target
  prevalence for additional NPV and PPV calculations.

- citype:

  Confidence interval type for proportions: one of the methods of
  [`binom::binom.confint()`](https://rdrr.io/pkg/binom/man/binom.confint.html),
  such as "exact" (default), "wilson", or "ac". The estimate is always
  the observed proportion.

- bootreps:

  Number of bootstrap samples used for the bootstrapped CIs.

- bootseed:

  Seed used for the bootstrapped CIs, so results are reproducible on the
  same R version and platform. Bootstrap draws can differ across
  platforms and R versions by Monte Carlo error.

- doboot:

  Logical. Generate percentile bootstrap CIs for the measures without a
  closed-form CI (F1, F2, MCC, balanced accuracy, informedness,
  markedness, G-mean, and Fowlkes-Mallows)? The confusion matrix cell
  counts are resampled from a multinomial distribution, which is
  equivalent to resampling cases.

- direction:

  Deprecated. Predictions are probabilities that the outcome is 1, so
  higher predictions always point to outcome 1; use `poslabel` to choose
  which outcome is the event. Values other than the default, "\<", give
  a warning and only affect the AUC and ROC curve (see ?pROC::roc).
  "auto" lets pROC choose the direction that gives an AUC of at least
  0.5, which hides models that perform worse than chance.

- ...:

  currently unused

## Value

An object of class "dx": a list with the data, the options used, the
measures at each threshold and subgroup (`measures`), measures across
all thresholds (`thresholds`), a prevalence analysis (`prevalence`), the
ranked predictions used by the gain, lift, KS, and CAP plots (`rank`),
the confusion matrix at `setthreshold` (`cm`), and the
[`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html) object (`roc`).
Use
[`summary.dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/summary.dx.md)
or
[`as.data.frame.dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/as.data.frame.dx.md)
to view the measures.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = c("AgeGroup", "Sex")
)
summary(dx_obj)
#> 
#> 
#> Table: Threshold= 0.3
#> 
#> |variable |label   |measure                                   |summary              |
#> |:--------|:-------|:-----------------------------------------|:--------------------|
#> |AgeGroup |(20,50] |AUC ROC                                   |0.896 (0.839, 0.953) |
#> |AgeGroup |(20,50] |Accuracy                                  |80.4% (73.2%, 86.4%) |
#> |AgeGroup |(20,50] |Sensitivity                               |83.3% (70.7%, 92.1%) |
#> |AgeGroup |(20,50] |Specificity                               |78.8% (69.4%, 86.4%) |
#> |AgeGroup |(20,50] |Positive Predictive Value                 |68.2% (55.6%, 79.1%) |
#> |AgeGroup |(20,50] |Negative Predictive Value                 |89.7% (81.3%, 95.2%) |
#> |AgeGroup |(20,50] |LRT+                                      |3.93 (2.64, 5.85)    |
#> |AgeGroup |(20,50] |LRT-                                      |0.21 (0.12, 0.39)    |
#> |AgeGroup |(20,50] |Odds Ratio                                |18.57 (7.84, 44.01)  |
#> |AgeGroup |(20,50] |F1 Score                                  |0.75                 |
#> |AgeGroup |(20,50] |F2 Score                                  |0.80                 |
#> |AgeGroup |(20,50] |Prevalence                                |35.3% (27.7%, 43.4%) |
#> |AgeGroup |(20,50] |False Negative Rate                       |16.7% (7.9%, 29.3%)  |
#> |AgeGroup |(20,50] |False Positive Rate                       |21.2% (13.6%, 30.6%) |
#> |AgeGroup |(20,50] |False Discovery Rate                      |31.8% (20.9%, 44.4%) |
#> |AgeGroup |(20,50] |AUC PR                                    |0.872                |
#> |AgeGroup |(20,50] |Cohen's Kappa                             |0.59 (0.46, 0.72)    |
#> |AgeGroup |(20,50] |Matthews Correlation Coefficient          |0.60                 |
#> |AgeGroup |(20,50] |Balanced Accuracy                         |0.81                 |
#> |AgeGroup |(20,50] |Informedness                              |0.62                 |
#> |AgeGroup |(20,50] |Markedness                                |0.58                 |
#> |AgeGroup |(20,50] |G-mean                                    |0.81                 |
#> |AgeGroup |(20,50] |Fowlkes-Mallows Index                     |0.75                 |
#> |AgeGroup |(20,50] |Brier Score                               |0.114                |
#> |AgeGroup |(20,50] |Pearson's Chi-squared (Yates' correction) |p<0.01               |
#> |AgeGroup |(20,50] |Fisher's Exact                            |p<0.01               |
#> |AgeGroup |(20,50] |G-Test                                    |p<0.01               |
#> |AgeGroup |(50,80] |AUC ROC                                   |0.913 (0.859, 0.967) |
#> |AgeGroup |(50,80] |Accuracy                                  |77.8% (68.8%, 85.2%) |
#> |AgeGroup |(50,80] |Sensitivity                               |86.4% (72.6%, 94.8%) |
#> |AgeGroup |(50,80] |Specificity                               |71.9% (59.2%, 82.4%) |
#> |AgeGroup |(50,80] |Positive Predictive Value                 |67.9% (54.0%, 79.7%) |
#> |AgeGroup |(50,80] |Negative Predictive Value                 |88.5% (76.6%, 95.6%) |
#> |AgeGroup |(50,80] |LRT+                                      |3.07 (2.04, 4.62)    |
#> |AgeGroup |(50,80] |LRT-                                      |0.19 (0.09, 0.41)    |
#> |AgeGroup |(50,80] |Odds Ratio                                |16.19 (5.84, 44.84)  |
#> |AgeGroup |(50,80] |F1 Score                                  |0.76                 |
#> |AgeGroup |(50,80] |F2 Score                                  |0.82                 |
#> |AgeGroup |(50,80] |Prevalence                                |40.7% (31.4%, 50.6%) |
#> |AgeGroup |(50,80] |False Negative Rate                       |13.6% (5.2%, 27.4%)  |
#> |AgeGroup |(50,80] |False Positive Rate                       |28.1% (17.6%, 40.8%) |
#> |AgeGroup |(50,80] |False Discovery Rate                      |32.1% (20.3%, 46.0%) |
#> |AgeGroup |(50,80] |AUC PR                                    |0.881                |
#> |AgeGroup |(50,80] |Cohen's Kappa                             |0.56 (0.41, 0.71)    |
#> |AgeGroup |(50,80] |Matthews Correlation Coefficient          |0.57                 |
#> |AgeGroup |(50,80] |Balanced Accuracy                         |0.79                 |
#> |AgeGroup |(50,80] |Informedness                              |0.58                 |
#> |AgeGroup |(50,80] |Markedness                                |0.56                 |
#> |AgeGroup |(50,80] |G-mean                                    |0.79                 |
#> |AgeGroup |(50,80] |Fowlkes-Mallows Index                     |0.77                 |
#> |AgeGroup |(50,80] |Brier Score                               |0.114                |
#> |AgeGroup |(50,80] |Pearson's Chi-squared (Yates' correction) |p<0.01               |
#> |AgeGroup |(50,80] |Fisher's Exact                            |p<0.01               |
#> |AgeGroup |(50,80] |G-Test                                    |p<0.01               |
#> |AgeGroup |Overall |Breslow-Day                               |p=0.84               |
#> |Sex      |Female  |AUC ROC                                   |0.860 (0.765, 0.954) |
#> |Sex      |Female  |Accuracy                                  |79.7% (68.3%, 88.4%) |
#> |Sex      |Female  |Sensitivity                               |50.0% (21.1%, 78.9%) |
#> |Sex      |Female  |Specificity                               |86.0% (74.2%, 93.7%) |
#> |Sex      |Female  |Positive Predictive Value                 |42.9% (17.7%, 71.1%) |
#> |Sex      |Female  |Negative Predictive Value                 |89.1% (77.8%, 95.9%) |
#> |Sex      |Female  |LRT+                                      |3.56 (1.51, 8.39)    |
#> |Sex      |Female  |LRT-                                      |0.58 (0.33, 1.03)    |
#> |Sex      |Female  |Odds Ratio                                |6.12 (1.58, 23.77)   |
#> |Sex      |Female  |F1 Score                                  |0.46                 |
#> |Sex      |Female  |F2 Score                                  |0.48                 |
#> |Sex      |Female  |Prevalence                                |17.4% (9.3%, 28.4%)  |
#> |Sex      |Female  |False Negative Rate                       |50.0% (21.1%, 78.9%) |
#> |Sex      |Female  |False Positive Rate                       |14.0% (6.3%, 25.8%)  |
#> |Sex      |Female  |False Discovery Rate                      |57.1% (28.9%, 82.3%) |
#> |Sex      |Female  |AUC PR                                    |0.612                |
#> |Sex      |Female  |Cohen's Kappa                             |0.34 (0.06, 0.61)    |
#> |Sex      |Female  |Matthews Correlation Coefficient          |0.34                 |
#> |Sex      |Female  |Balanced Accuracy                         |0.68                 |
#> |Sex      |Female  |Informedness                              |0.36                 |
#> |Sex      |Female  |Markedness                                |0.32                 |
#> |Sex      |Female  |G-mean                                    |0.66                 |
#> |Sex      |Female  |Fowlkes-Mallows Index                     |0.46                 |
#> |Sex      |Female  |Brier Score                               |0.109                |
#> |Sex      |Female  |Pearson's Chi-squared (Yates' correction) |p=0.02               |
#> |Sex      |Female  |Fisher's Exact                            |p=0.01               |
#> |Sex      |Female  |G-Test                                    |p<0.01               |
#> |Sex      |Male    |AUC ROC                                   |0.907 (0.862, 0.953) |
#> |Sex      |Male    |Accuracy                                  |79.2% (72.7%, 84.7%) |
#> |Sex      |Male    |Sensitivity                               |89.5% (81.1%, 95.1%) |
#> |Sex      |Male    |Specificity                               |70.8% (61.1%, 79.2%) |
#> |Sex      |Male    |Positive Predictive Value                 |71.3% (61.8%, 79.6%) |
#> |Sex      |Male    |Negative Predictive Value                 |89.3% (80.6%, 95.0%) |
#> |Sex      |Male    |LRT+                                      |3.06 (2.26, 4.15)    |
#> |Sex      |Male    |LRT-                                      |0.15 (0.08, 0.28)    |
#> |Sex      |Male    |Odds Ratio                                |20.70 (9.23, 46.41)  |
#> |Sex      |Male    |F1 Score                                  |0.79                 |
#> |Sex      |Male    |F2 Score                                  |0.85                 |
#> |Sex      |Male    |Prevalence                                |44.8% (37.6%, 52.1%) |
#> |Sex      |Male    |False Negative Rate                       |10.5% (4.9%, 18.9%)  |
#> |Sex      |Male    |False Positive Rate                       |29.2% (20.8%, 38.9%) |
#> |Sex      |Male    |False Discovery Rate                      |28.7% (20.4%, 38.2%) |
#> |Sex      |Male    |AUC PR                                    |0.900                |
#> |Sex      |Male    |Cohen's Kappa                             |0.59 (0.48, 0.70)    |
#> |Sex      |Male    |Matthews Correlation Coefficient          |0.60                 |
#> |Sex      |Male    |Balanced Accuracy                         |0.80                 |
#> |Sex      |Male    |Informedness                              |0.60                 |
#> |Sex      |Male    |Markedness                                |0.61                 |
#> |Sex      |Male    |G-mean                                    |0.80                 |
#> |Sex      |Male    |Fowlkes-Mallows Index                     |0.80                 |
#> |Sex      |Male    |Brier Score                               |0.115                |
#> |Sex      |Male    |Pearson's Chi-squared (Yates' correction) |p<0.01               |
#> |Sex      |Male    |Fisher's Exact                            |p<0.01               |
#> |Sex      |Male    |G-Test                                    |p<0.01               |
#> |Sex      |Overall |Breslow-Day                               |p=0.12               |
#> |Overall  |Overall |AUC ROC                                   |0.904 (0.864, 0.943) |
#> |Overall  |Overall |Accuracy                                  |79.3% (73.9%, 84.1%) |
#> |Overall  |Overall |Sensitivity                               |84.7% (76.0%, 91.2%) |
#> |Overall  |Overall |Specificity                               |76.1% (68.8%, 82.4%) |
#> |Overall  |Overall |Positive Predictive Value                 |68.0% (59.0%, 76.2%) |
#> |Overall  |Overall |Negative Predictive Value                 |89.2% (82.8%, 93.8%) |
#> |Overall  |Overall |LRT+                                      |3.54 (2.66, 4.71)    |
#> |Overall  |Overall |LRT-                                      |0.20 (0.13, 0.32)    |
#> |Overall  |Overall |Odds Ratio                                |17.59 (9.12, 33.94)  |
#> |Overall  |Overall |F1 Score                                  |0.75                 |
#> |Overall  |Overall |F2 Score                                  |0.81                 |
#> |Overall  |Overall |Prevalence                                |37.5% (31.7%, 43.7%) |
#> |Overall  |Overall |False Negative Rate                       |15.3% (8.8%, 24.0%)  |
#> |Overall  |Overall |False Positive Rate                       |23.9% (17.6%, 31.2%) |
#> |Overall  |Overall |False Discovery Rate                      |32.0% (23.8%, 41.0%) |
#> |Overall  |Overall |AUC PR                                    |0.873                |
#> |Overall  |Overall |Cohen's Kappa                             |0.58 (0.48, 0.68)    |
#> |Overall  |Overall |Matthews Correlation Coefficient          |0.59                 |
#> |Overall  |Overall |Balanced Accuracy                         |0.80                 |
#> |Overall  |Overall |Informedness                              |0.61                 |
#> |Overall  |Overall |Markedness                                |0.57                 |
#> |Overall  |Overall |G-mean                                    |0.80                 |
#> |Overall  |Overall |Fowlkes-Mallows Index                     |0.76                 |
#> |Overall  |Overall |Brier Score                               |0.114                |
#> |Overall  |Overall |Pearson's Chi-squared (Yates' correction) |p<0.01               |
#> |Overall  |Overall |Fisher's Exact                            |p<0.01               |
#> |Overall  |Overall |G-Test                                    |p<0.01               |

# Bootstrapped CIs for measures without a closed-form CI
dx_boot <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  doboot = TRUE,
  bootreps = 500
)
summary(dx_boot, measure = c("F1 Score", "Matthews Correlation Coefficient"))
#> 
#> 
#> Table: Threshold= 0.3
#> 
#> |variable |label   |measure                          |summary           |
#> |:--------|:-------|:--------------------------------|:-----------------|
#> |Overall  |Overall |F1 Score                         |0.75 (0.68, 0.82) |
#> |Overall  |Overall |Matthews Correlation Coefficient |0.59 (0.48, 0.68) |
```
