# Calculate Matthews Correlation Coefficient (MCC)

Calculates the Matthews Correlation Coefficient (MCC), a measure of the
quality of binary classifications. It returns a value between -1 and +1
where +1 indicates perfect prediction, 0 no better than random
prediction, and -1 indicates total disagreement between prediction and
observation. The function can also return a confidence interval for the
MCC value using bootstrapping if detail is set to "full".

## Usage

``` r
dx_mcc(cm, detail = "full", boot = FALSE, bootreps = 1000)
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

- boot:

  Logical specifying if confidence intervals should be generated via
  bootstrapping. Note, this can be slow.

- bootreps:

  The number of bootstrap replications for calculating confidence
  intervals.

## Value

If `detail` is "simple", returns a single numeric value of MCC. If
`detail` is "full", returns a data frame that includes MCC, its
bootstrapped confidence interval, and other key details

## Details

The Matthews Correlation Coefficient is used in machine learning as a
measure of the quality of binary (two-class) classifications. It takes
into account true and false positives and negatives and is generally
regarded as a balanced measure which can be used even if the classes are
of very different sizes. The formula for MCC is: \$\$MCC = \frac{(TP
\times TN) - (FP \times FN)}{\sqrt{(TP + FP)(TP + FN)(TN + FP)(TN +
FN)}}\$\$ where TP, TN, FP, and FN represent the counts of true
positives, true negatives, false positives, and false negatives,
respectively.

For "full" details, bootstrap methods are used to estimate the
confidence interval for the MCC value, providing a more robust
understanding of its stability.

## Examples

``` r
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
mcc_simple <- dx_mcc(cm, detail = "simple")
mcc_full <- dx_mcc(cm)
print(mcc_simple)
#> [1] 0.6428112
print(mcc_full)
#> # A tibble: 1 × 8
#>   measure           summary estimate conf_low conf_high fraction conf_type notes
#>   <chr>             <chr>      <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Matthews Correla… 0.64       0.643       NA        NA ""       NA        Spec…
```
