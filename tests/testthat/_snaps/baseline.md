# Printed summaries match the saved snapshot

    Code
      print(summary(grouped))
    Output
      
      
      Table: Threshold= 0.3
      
      |variable |label   |measure                          |summary              |
      |:--------|:-------|:--------------------------------|:--------------------|
      |AgeGroup |(20,50] |AUC ROC                          |0.896 (0.839, 0.953) |
      |AgeGroup |(20,50] |Accuracy                         |80.4% (73.2%, 86.4%) |
      |AgeGroup |(20,50] |Sensitivity                      |83.3% (70.7%, 92.1%) |
      |AgeGroup |(20,50] |Specificity                      |78.8% (69.4%, 86.4%) |
      |AgeGroup |(20,50] |Positive Predictive Value        |68.2% (55.6%, 79.1%) |
      |AgeGroup |(20,50] |Negative Predictive Value        |89.7% (81.3%, 95.2%) |
      |AgeGroup |(20,50] |LRT+                             |3.93 (2.64, 5.85)    |
      |AgeGroup |(20,50] |LRT-                             |0.21 (0.12, 0.39)    |
      |AgeGroup |(20,50] |Odds Ratio                       |18.57 (7.84, 44.01)  |
      |AgeGroup |(20,50] |F1 Score                         |0.75                 |
      |AgeGroup |(20,50] |F2 Score                         |0.8                  |
      |AgeGroup |(20,50] |Prevalence                       |35.3% (27.7%, 43.4%) |
      |AgeGroup |(20,50] |False Negative Rate              |16.7% (7.9%, 29.3%)  |
      |AgeGroup |(20,50] |False Positive Rate              |21.2% (13.6%, 30.6%) |
      |AgeGroup |(20,50] |False Discovery Rate             |31.8% (20.9%, 44.4%) |
      |AgeGroup |(20,50] |AUC PR                           |0.87                 |
      |AgeGroup |(20,50] |Cohen's Kappa                    |0.59 (0.46, 0.72)    |
      |AgeGroup |(20,50] |Matthews Correlation Coefficient |0.6                  |
      |AgeGroup |(20,50] |Balanced Accuracy                |0.81                 |
      |AgeGroup |(20,50] |Informedness                     |0.62                 |
      |AgeGroup |(20,50] |Markedness                       |0.58                 |
      |AgeGroup |(20,50] |G-mean                           |0.81                 |
      |AgeGroup |(20,50] |Fowlkes-Mallows Index            |0.75                 |
      |AgeGroup |(20,50] |Brier Score                      |0.11                 |
      |AgeGroup |(20,50] |Pearson's Chi-squared            |p<0.01               |
      |AgeGroup |(20,50] |Fisher's Exact                   |p<0.01               |
      |AgeGroup |(20,50] |G-Test                           |p<0.01               |
      |AgeGroup |(50,80] |AUC ROC                          |0.913 (0.859, 0.967) |
      |AgeGroup |(50,80] |Accuracy                         |77.8% (68.8%, 85.2%) |
      |AgeGroup |(50,80] |Sensitivity                      |86.4% (72.6%, 94.8%) |
      |AgeGroup |(50,80] |Specificity                      |71.9% (59.2%, 82.4%) |
      |AgeGroup |(50,80] |Positive Predictive Value        |67.9% (54.0%, 79.7%) |
      |AgeGroup |(50,80] |Negative Predictive Value        |88.5% (76.6%, 95.6%) |
      |AgeGroup |(50,80] |LRT+                             |3.07 (2.04, 4.62)    |
      |AgeGroup |(50,80] |LRT-                             |0.19 (0.09, 0.41)    |
      |AgeGroup |(50,80] |Odds Ratio                       |16.19 (5.84, 44.84)  |
      |AgeGroup |(50,80] |F1 Score                         |0.76                 |
      |AgeGroup |(50,80] |F2 Score                         |0.82                 |
      |AgeGroup |(50,80] |Prevalence                       |40.7% (31.4%, 50.6%) |
      |AgeGroup |(50,80] |False Negative Rate              |13.6% (5.2%, 27.4%)  |
      |AgeGroup |(50,80] |False Positive Rate              |28.1% (17.6%, 40.8%) |
      |AgeGroup |(50,80] |False Discovery Rate             |32.1% (20.3%, 46.0%) |
      |AgeGroup |(50,80] |AUC PR                           |0.88                 |
      |AgeGroup |(50,80] |Cohen's Kappa                    |0.56 (0.40, 0.71)    |
      |AgeGroup |(50,80] |Matthews Correlation Coefficient |0.57                 |
      |AgeGroup |(50,80] |Balanced Accuracy                |0.79                 |
      |AgeGroup |(50,80] |Informedness                     |0.58                 |
      |AgeGroup |(50,80] |Markedness                       |0.56                 |
      |AgeGroup |(50,80] |G-mean                           |0.79                 |
      |AgeGroup |(50,80] |Fowlkes-Mallows Index            |0.77                 |
      |AgeGroup |(50,80] |Brier Score                      |0.11                 |
      |AgeGroup |(50,80] |Pearson's Chi-squared            |p<0.01               |
      |AgeGroup |(50,80] |Fisher's Exact                   |p<0.01               |
      |AgeGroup |(50,80] |G-Test                           |p<0.01               |
      |AgeGroup |Overall |Breslow-Day                      |p=0.84               |
      |Sex      |Female  |AUC ROC                          |0.860 (0.765, 0.954) |
      |Sex      |Female  |Accuracy                         |79.7% (68.3%, 88.4%) |
      |Sex      |Female  |Sensitivity                      |50.0% (21.1%, 78.9%) |
      |Sex      |Female  |Specificity                      |86.0% (74.2%, 93.7%) |
      |Sex      |Female  |Positive Predictive Value        |42.9% (17.7%, 71.1%) |
      |Sex      |Female  |Negative Predictive Value        |89.1% (77.8%, 95.9%) |
      |Sex      |Female  |LRT+                             |3.56 (1.51, 8.39)    |
      |Sex      |Female  |LRT-                             |0.58 (0.33, 1.03)    |
      |Sex      |Female  |Odds Ratio                       |6.12 (1.58, 23.77)   |
      |Sex      |Female  |F1 Score                         |0.46                 |
      |Sex      |Female  |F2 Score                         |0.48                 |
      |Sex      |Female  |Prevalence                       |17.4% (9.3%, 28.4%)  |
      |Sex      |Female  |False Negative Rate              |50.0% (21.1%, 78.9%) |
      |Sex      |Female  |False Positive Rate              |14.0% (6.3%, 25.8%)  |
      |Sex      |Female  |False Discovery Rate             |57.1% (28.9%, 82.3%) |
      |Sex      |Female  |AUC PR                           |0.61                 |
      |Sex      |Female  |Cohen's Kappa                    |0.34 (0.03, 0.65)    |
      |Sex      |Female  |Matthews Correlation Coefficient |0.34                 |
      |Sex      |Female  |Balanced Accuracy                |0.68                 |
      |Sex      |Female  |Informedness                     |0.36                 |
      |Sex      |Female  |Markedness                       |0.32                 |
      |Sex      |Female  |G-mean                           |0.66                 |
      |Sex      |Female  |Fowlkes-Mallows Index            |0.46                 |
      |Sex      |Female  |Brier Score                      |0.11                 |
      |Sex      |Female  |Pearson's Chi-squared            |p=0.02               |
      |Sex      |Female  |Fisher's Exact                   |p=0.01               |
      |Sex      |Female  |G-Test                           |p<0.01               |
      |Sex      |Male    |AUC ROC                          |0.907 (0.862, 0.953) |
      |Sex      |Male    |Accuracy                         |79.2% (72.7%, 84.7%) |
      |Sex      |Male    |Sensitivity                      |89.5% (81.1%, 95.1%) |
      |Sex      |Male    |Specificity                      |70.8% (61.1%, 79.2%) |
      |Sex      |Male    |Positive Predictive Value        |71.3% (61.8%, 79.6%) |
      |Sex      |Male    |Negative Predictive Value        |89.3% (80.6%, 95.0%) |
      |Sex      |Male    |LRT+                             |3.06 (2.26, 4.15)    |
      |Sex      |Male    |LRT-                             |0.15 (0.08, 0.28)    |
      |Sex      |Male    |Odds Ratio                       |20.70 (9.23, 46.41)  |
      |Sex      |Male    |F1 Score                         |0.79                 |
      |Sex      |Male    |F2 Score                         |0.85                 |
      |Sex      |Male    |Prevalence                       |44.8% (37.6%, 52.1%) |
      |Sex      |Male    |False Negative Rate              |10.5% (4.9%, 18.9%)  |
      |Sex      |Male    |False Positive Rate              |29.2% (20.8%, 38.9%) |
      |Sex      |Male    |False Discovery Rate             |28.7% (20.4%, 38.2%) |
      |Sex      |Male    |AUC PR                           |0.9                  |
      |Sex      |Male    |Cohen's Kappa                    |0.59 (0.48, 0.70)    |
      |Sex      |Male    |Matthews Correlation Coefficient |0.6                  |
      |Sex      |Male    |Balanced Accuracy                |0.8                  |
      |Sex      |Male    |Informedness                     |0.6                  |
      |Sex      |Male    |Markedness                       |0.61                 |
      |Sex      |Male    |G-mean                           |0.8                  |
      |Sex      |Male    |Fowlkes-Mallows Index            |0.8                  |
      |Sex      |Male    |Brier Score                      |0.12                 |
      |Sex      |Male    |Pearson's Chi-squared            |p<0.01               |
      |Sex      |Male    |Fisher's Exact                   |p<0.01               |
      |Sex      |Male    |G-Test                           |p<0.01               |
      |Sex      |Overall |Breslow-Day                      |p=0.12               |
      |Overall  |Overall |AUC ROC                          |0.904 (0.864, 0.943) |
      |Overall  |Overall |Accuracy                         |79.3% (73.9%, 84.1%) |
      |Overall  |Overall |Sensitivity                      |84.7% (76.0%, 91.2%) |
      |Overall  |Overall |Specificity                      |76.1% (68.8%, 82.4%) |
      |Overall  |Overall |Positive Predictive Value        |68.0% (59.0%, 76.2%) |
      |Overall  |Overall |Negative Predictive Value        |89.2% (82.8%, 93.8%) |
      |Overall  |Overall |LRT+                             |3.54 (2.66, 4.71)    |
      |Overall  |Overall |LRT-                             |0.20 (0.13, 0.32)    |
      |Overall  |Overall |Odds Ratio                       |17.59 (9.12, 33.94)  |
      |Overall  |Overall |F1 Score                         |0.75                 |
      |Overall  |Overall |F2 Score                         |0.81                 |
      |Overall  |Overall |Prevalence                       |37.5% (31.7%, 43.7%) |
      |Overall  |Overall |False Negative Rate              |15.3% (8.8%, 24.0%)  |
      |Overall  |Overall |False Positive Rate              |23.9% (17.6%, 31.2%) |
      |Overall  |Overall |False Discovery Rate             |32.0% (23.8%, 41.0%) |
      |Overall  |Overall |AUC PR                           |0.87                 |
      |Overall  |Overall |Cohen's Kappa                    |0.58 (0.48, 0.68)    |
      |Overall  |Overall |Matthews Correlation Coefficient |0.59                 |
      |Overall  |Overall |Balanced Accuracy                |0.8                  |
      |Overall  |Overall |Informedness                     |0.61                 |
      |Overall  |Overall |Markedness                       |0.57                 |
      |Overall  |Overall |G-mean                           |0.8                  |
      |Overall  |Overall |Fowlkes-Mallows Index            |0.76                 |
      |Overall  |Overall |Brier Score                      |0.11                 |
      |Overall  |Overall |Pearson's Chi-squared            |p<0.01               |
      |Overall  |Overall |Fisher's Exact                   |p<0.01               |
      |Overall  |Overall |G-Test                           |p<0.01               |

---

    Code
      print(summary(grouped, variable = "Sex", show_var = FALSE))
    Output
      
      
      Table: Threshold= 0.3
      
      |label   |measure                          |summary              |
      |:-------|:--------------------------------|:--------------------|
      |Female  |AUC ROC                          |0.860 (0.765, 0.954) |
      |Female  |Accuracy                         |79.7% (68.3%, 88.4%) |
      |Female  |Sensitivity                      |50.0% (21.1%, 78.9%) |
      |Female  |Specificity                      |86.0% (74.2%, 93.7%) |
      |Female  |Positive Predictive Value        |42.9% (17.7%, 71.1%) |
      |Female  |Negative Predictive Value        |89.1% (77.8%, 95.9%) |
      |Female  |LRT+                             |3.56 (1.51, 8.39)    |
      |Female  |LRT-                             |0.58 (0.33, 1.03)    |
      |Female  |Odds Ratio                       |6.12 (1.58, 23.77)   |
      |Female  |F1 Score                         |0.46                 |
      |Female  |F2 Score                         |0.48                 |
      |Female  |Prevalence                       |17.4% (9.3%, 28.4%)  |
      |Female  |False Negative Rate              |50.0% (21.1%, 78.9%) |
      |Female  |False Positive Rate              |14.0% (6.3%, 25.8%)  |
      |Female  |False Discovery Rate             |57.1% (28.9%, 82.3%) |
      |Female  |AUC PR                           |0.61                 |
      |Female  |Cohen's Kappa                    |0.34 (0.03, 0.65)    |
      |Female  |Matthews Correlation Coefficient |0.34                 |
      |Female  |Balanced Accuracy                |0.68                 |
      |Female  |Informedness                     |0.36                 |
      |Female  |Markedness                       |0.32                 |
      |Female  |G-mean                           |0.66                 |
      |Female  |Fowlkes-Mallows Index            |0.46                 |
      |Female  |Brier Score                      |0.11                 |
      |Female  |Pearson's Chi-squared            |p=0.02               |
      |Female  |Fisher's Exact                   |p=0.01               |
      |Female  |G-Test                           |p<0.01               |
      |Male    |AUC ROC                          |0.907 (0.862, 0.953) |
      |Male    |Accuracy                         |79.2% (72.7%, 84.7%) |
      |Male    |Sensitivity                      |89.5% (81.1%, 95.1%) |
      |Male    |Specificity                      |70.8% (61.1%, 79.2%) |
      |Male    |Positive Predictive Value        |71.3% (61.8%, 79.6%) |
      |Male    |Negative Predictive Value        |89.3% (80.6%, 95.0%) |
      |Male    |LRT+                             |3.06 (2.26, 4.15)    |
      |Male    |LRT-                             |0.15 (0.08, 0.28)    |
      |Male    |Odds Ratio                       |20.70 (9.23, 46.41)  |
      |Male    |F1 Score                         |0.79                 |
      |Male    |F2 Score                         |0.85                 |
      |Male    |Prevalence                       |44.8% (37.6%, 52.1%) |
      |Male    |False Negative Rate              |10.5% (4.9%, 18.9%)  |
      |Male    |False Positive Rate              |29.2% (20.8%, 38.9%) |
      |Male    |False Discovery Rate             |28.7% (20.4%, 38.2%) |
      |Male    |AUC PR                           |0.9                  |
      |Male    |Cohen's Kappa                    |0.59 (0.48, 0.70)    |
      |Male    |Matthews Correlation Coefficient |0.6                  |
      |Male    |Balanced Accuracy                |0.8                  |
      |Male    |Informedness                     |0.6                  |
      |Male    |Markedness                       |0.61                 |
      |Male    |G-mean                           |0.8                  |
      |Male    |Fowlkes-Mallows Index            |0.8                  |
      |Male    |Brier Score                      |0.12                 |
      |Male    |Pearson's Chi-squared            |p<0.01               |
      |Male    |Fisher's Exact                   |p<0.01               |
      |Male    |G-Test                           |p<0.01               |
      |Overall |Breslow-Day                      |p=0.12               |

