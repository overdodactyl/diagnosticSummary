# Intention-to-diagnose (3x2) analysis

Summarize a diagnostic test when some predictions are missing, for
example because a case failed quality control or a pre-defined rule
stopped the model from running. Instead of excluding these cases, a
missing prediction is treated as an ungradable result. Results are
summarized in a 3x2 table, with positive, negative, and ungradable test
results in the rows and the reference standard in the columns (Schuetz
et al., 2012).

## Usage

``` r
dx_itd(
  data,
  pred_varname,
  true_varname,
  setthreshold = 0.5,
  poslabel = 1,
  grouping_variables = NA,
  citype = "exact"
)
```

## Arguments

- data:

  A tbl.

- pred_varname:

  Column name containing the predictions (string). Values should be
  predicted probabilities that the outcome is 1, between 0 and 1.
  Missing values (`NA`) are ungradable results.

- true_varname:

  Column name containing the reference standard (string). Values should
  be 0 or 1 (numeric, logical, factor, or character), with no missing
  values, and both classes must be present.

- setthreshold:

  A number between 0 and 1 representing the threshold used to classify
  the predictions. Predictions at or above the threshold predict outcome
  1.

- poslabel:

  The outcome class that is the event: 1 (default) or 0. Predictions are
  still probabilities that the outcome is 1, so with `poslabel = 0` a
  prediction below the threshold is a positive test.

- grouping_variables:

  Character vector of variable names to be summarized by. Variables are
  converted to factors if not already one.

- citype:

  Confidence interval type for proportions (see
  [`binom::binom.confint()`](https://rdrr.io/pkg/binom/man/binom.confint.html)).

## Value

An object of class `dx_itd`, a list with `data`, `options`, `measures`
(a data frame with one row per measure), and `cm` (the 3x2 cell counts
at `setthreshold`).

## Details

With TP, FN, and UP the positive, negative, and ungradable results among
cases with the event, and FP, TN, and UN the same among cases without
it:

|  |  |
|----|----|
| Measure | Definition |
| Diagnostic yield (disease positive) | (TP + FN) / (TP + FN + UP) |
| Diagnostic yield (disease negative) | (FP + TN) / (FP + TN + UN) |
| Diagnostic yield (overall) | Gradable results / N |
| Pre-test disease risk | (TP + FN + UP) / N |
| Positive predictive value | TP / (TP + FP) |
| Negative predictive value | TN / (TN + FN) |
| Post-test disease risk (positive) | TP / (TP + FP) |
| Post-test disease risk (negative) | FN / (TN + FN) |
| Post-test disease risk (ungradable) | UP / (UP + UN) |
| Likelihood ratio (positive) | (TP / (TP + FN + UP)) / (FP / (FP + TN + UN)) |
| Likelihood ratio (negative) | (FN / (TP + FN + UP)) / (TN / (FP + TN + UN)) |
| Likelihood ratio (ungradable) | (UP / (TP + FN + UP)) / (UN / (FP + TN + UN)) |
| Worst-case sensitivity | TP / (TP + FN + UP) |
| Worst-case specificity | TN / (FP + TN + UN) |
| Worst-case Youden's J | Worst-case sensitivity + worst-case specificity - 1 |

Diagnostic yield is the proportion of cases with a gradable (positive or
negative) result. The worst case counts every ungradable result as
incorrect: as a negative result for cases with the event, and as a
positive result for cases without it. Predictive values only use
gradable results.

The likelihood ratios use all cases with and without the event as
denominators, including ungradable results, so the positive and negative
likelihood ratios differ from
[`dx_lrt_pos()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_lrt_pos.md)
and
[`dx_lrt_neg()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_lrt_neg.md)
on the gradable cases. A likelihood ratio near 1 for ungradable results
means they are about as common with and without the event.

Proportions use binomial confidence intervals (`citype`). Likelihood
ratios use a large sample interval on the log scale. If the 2x2 table of
one row against the others has a zero cell, 0.5 is added to each of its
cells. Youden's J uses a Wald interval that treats the two outcome
classes as independent, truncated to -1 to 1. The interval is too narrow
when the worst-case sensitivity or specificity is 0% or 100%, and has no
width when both are, which is noted in the results. Likelihood ratios
are `NA` for a result no case has, such as ungradable results when no
predictions are missing.

At least one prediction is needed, but a subgroup in
`grouping_variables` can have every prediction missing, for example a
site where every case failed quality control. It is kept with a warning,
with a diagnostic yield of 0%.

## References

Schuetz GM, Schlattmann P, Dewey M. Use of 3x2 tables with an intention
to diagnose approach to assess clinical performance of diagnostic tests:
meta-analytical evaluation of coronary CT angiography studies. BMJ.
2012;345:e6717.

## See also

[`dx_itd_table()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_itd_table.md)
for the 3x2 table,
[`dx()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx.md)
when every prediction is available.

## Examples

``` r
data <- dx_heart_failure
data$predicted[seq(5, 261, by = 20)] <- NA
fit <- dx_itd(data, pred_varname = "predicted", true_varname = "truth")
summary(fit)
#> 
#> 
#> Table: Threshold= 0.5
#> 
#> |variable |label   |measure                             |summary              |
#> |:--------|:-------|:-----------------------------------|:--------------------|
#> |Overall  |Overall |Diagnostic Yield (Disease Positive) |94.9% (88.5%, 98.3%) |
#> |Overall  |Overall |Diagnostic Yield (Disease Negative) |95.1% (90.6%, 97.9%) |
#> |Overall  |Overall |Diagnostic Yield (Overall)          |95.0% (91.6%, 97.3%) |
#> |Overall  |Overall |Pre-Test Disease Risk               |37.5% (31.7%, 43.7%) |
#> |Overall  |Overall |Positive Predictive Value           |84.6% (74.7%, 91.8%) |
#> |Overall  |Overall |Negative Predictive Value           |84.1% (77.7%, 89.3%) |
#> |Overall  |Overall |Post-Test Disease Risk (Positive)   |84.6% (74.7%, 91.8%) |
#> |Overall  |Overall |Post-Test Disease Risk (Negative)   |15.9% (10.7%, 22.3%) |
#> |Overall  |Overall |Post-Test Disease Risk (Ungradable) |38.5% (13.9%, 68.4%) |
#> |Overall  |Overall |Likelihood Ratio (Positive)         |9.15 (5.22, 16.04)   |
#> |Overall  |Overall |Likelihood Ratio (Negative)         |0.31 (0.23, 0.44)    |
#> |Overall  |Overall |Likelihood Ratio (Ungradable)       |1.04 (0.35, 3.09)    |
#> |Overall  |Overall |Worst-Case Sensitivity              |67.3% (57.1%, 76.5%) |
#> |Overall  |Overall |Worst-Case Specificity              |87.7% (81.7%, 92.3%) |
#> |Overall  |Overall |Worst-Case Youden's J               |0.55 (0.45, 0.66)    |
dx_itd_table(fit)
#>                      row     disease_positive     disease_negative
#> 1               Positive                   66                   12
#> 2               Negative                   27                  143
#> 3             Ungradable                    5                    8
#> 4                  Total                   98                  163
#> 5       Diagnostic yield 94.9% (88.5%, 98.3%) 95.1% (90.6%, 97.9%)
#> 6 Worst-case performance 67.3% (57.1%, 76.5%) 87.7% (81.7%, 92.3%)
#>                            total                             predictive_value
#> 1                             78                         84.6% (74.7%, 91.8%)
#> 2                            170                         84.1% (77.7%, 89.3%)
#> 3                             13                                             
#> 4                            261 Pre-test disease risk = 37.5% (31.7%, 43.7%)
#> 5           95.0% (91.6%, 97.3%)                                             
#> 6 Youden's J = 0.55 (0.45, 0.66)                                             
#>         post_test_risk   likelihood_ratio
#> 1 84.6% (74.7%, 91.8%) 9.15 (5.22, 16.04)
#> 2 15.9% (10.7%, 22.3%)  0.31 (0.23, 0.44)
#> 3 38.5% (13.9%, 68.4%)  1.04 (0.35, 3.09)
#> 4                                        
#> 5                                        
#> 6                                        
```
