# Summarize a diagnostic test

Shows the formatted estimate and confidence interval of each measure in
a `dx` object at one threshold.

## Usage

``` r
# S3 method for class 'dx'
summary(
  object,
  thresh = object$options$setthreshold,
  variable = NA,
  label = NA,
  show_var = TRUE,
  show_label = TRUE,
  measure = NA,
  ...
)
```

## Arguments

- object:

  An object of class "dx"

- thresh:

  The threshold to show: `setthreshold` (default) or a value in
  `threshold_range`.

- variable:

  Variables to include: "Overall" for all cases, or names in
  `grouping_variables`. Defaults to all.

- label:

  Subgroups to include: levels of the grouping variables, or "Overall".
  Defaults to all.

- show_var:

  Include the variable column?

- show_label:

  Include the label (subgroup) column?

- measure:

  Names of the measures to include, as in the `measure` column of
  [`as.data.frame.dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/as.data.frame.dx.md),
  such as "Sensitivity". Defaults to all.

- ...:

  Not used.

## Value

A table of the measures and their summaries at `thresh`, made with
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html). If knitr
is not installed, a data frame.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = "Sex"
)
summary(dx_obj)
#> 
#> 
#> Table: Threshold= 0.3
#> 
#> |variable |label   |measure                                   |summary              |
#> |:--------|:-------|:-----------------------------------------|:--------------------|
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
summary(dx_obj, variable = "Overall", show_var = FALSE)
#> 
#> 
#> Table: Threshold= 0.3
#> 
#> |label   |measure                                   |summary              |
#> |:-------|:-----------------------------------------|:--------------------|
#> |Overall |AUC ROC                                   |0.904 (0.864, 0.943) |
#> |Overall |Accuracy                                  |79.3% (73.9%, 84.1%) |
#> |Overall |Sensitivity                               |84.7% (76.0%, 91.2%) |
#> |Overall |Specificity                               |76.1% (68.8%, 82.4%) |
#> |Overall |Positive Predictive Value                 |68.0% (59.0%, 76.2%) |
#> |Overall |Negative Predictive Value                 |89.2% (82.8%, 93.8%) |
#> |Overall |LRT+                                      |3.54 (2.66, 4.71)    |
#> |Overall |LRT-                                      |0.20 (0.13, 0.32)    |
#> |Overall |Odds Ratio                                |17.59 (9.12, 33.94)  |
#> |Overall |F1 Score                                  |0.75                 |
#> |Overall |F2 Score                                  |0.81                 |
#> |Overall |Prevalence                                |37.5% (31.7%, 43.7%) |
#> |Overall |False Negative Rate                       |15.3% (8.8%, 24.0%)  |
#> |Overall |False Positive Rate                       |23.9% (17.6%, 31.2%) |
#> |Overall |False Discovery Rate                      |32.0% (23.8%, 41.0%) |
#> |Overall |AUC PR                                    |0.873                |
#> |Overall |Cohen's Kappa                             |0.58 (0.48, 0.68)    |
#> |Overall |Matthews Correlation Coefficient          |0.59                 |
#> |Overall |Balanced Accuracy                         |0.80                 |
#> |Overall |Informedness                              |0.61                 |
#> |Overall |Markedness                                |0.57                 |
#> |Overall |G-mean                                    |0.80                 |
#> |Overall |Fowlkes-Mallows Index                     |0.76                 |
#> |Overall |Brier Score                               |0.114                |
#> |Overall |Pearson's Chi-squared (Yates' correction) |p<0.01               |
#> |Overall |Fisher's Exact                            |p<0.01               |
#> |Overall |G-Test                                    |p<0.01               |
```
