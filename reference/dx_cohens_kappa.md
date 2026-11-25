# Calculate Cohen's Kappa

Calculates Cohen's Kappa, a statistical measure of inter-rater
reliability or agreement for qualitative (categorical) items. It is
generally thought to be a more robust measure than simple percent
agreement calculation since Kappa takes into account the agreement
occurring by chance.

## Usage

``` r
dx_cohens_kappa(cm, detail = "full")
```

## Arguments

- cm:

  A dx_cm object created by
  [`dx_cm()`](https://overdodactyl.github.io/diagnosticSummary/reference/dx_cm.md).

- detail:

  Character specifying the level of detail in the output: "simple" for
  raw estimate, "full" for detailed estimate including 95% confidence
  intervals.

## Value

If `detail` is "simple", returns a single numeric value of Cohen's
Kappa. If `detail` is "full", returns a list or data frame that includes
Cohen's Kappa, its standard error, 95% confidence intervals, and
interpretative notes.

## Details

Cohen's Kappa is used to measure the agreement between two raters who
each classify items into mutually exclusive categories. The formula for
Cohen's Kappa is: \$\$kappa = (po - pe) / (1 - pe)\$\$ where \\po\\ is
the relative observed agreement among raters, and \\pe\\ is the
hypothetical probability of chance agreement. The value of kappa can
range from -1 (total disagreement) to 1 (perfect agreement), with 0
indicating the amount of agreement that can be expected from random
chance.

Interpretation of Cohen's Kappa varies, but generally, a higher value
indicates better agreement. Typical benchmarks for interpreting Cohen's
Kappa are:

- \<0: Less than chance agreement

- 0.-0.2: Slight agreement

- 0.2-0.4: Fair agreement

- 0.4-0.6: Moderate agreement

- 0.6-0.8: Substantial agreement

- 0.8-1.0: Almost perfect agreement

The 95% confidence interval uses the large sample variance of Fleiss,
Cohen, and Everitt (1969) with a normal approximation, and is limited to
-1 to 1.

## References

Fleiss JL, Cohen J, Everitt BS. Large sample standard errors of kappa
and weighted kappa. Psychological Bulletin. 1969;72(5):323-327.

## Examples

``` r
# Assuming you have a confusion matrix cm with appropriate structure
cm <- dx_cm(dx_heart_failure$predicted, dx_heart_failure$truth,
  threshold =
    0.5, poslabel = 1
)
kappa_simple <- dx_cohens_kappa(cm, detail = "simple")
kappa_full <- dx_cohens_kappa(cm)
print(kappa_simple)
#> [1] 0.6361249
print(kappa_full)
#> # A tibble: 1 × 8
#>   measure       summary     estimate conf_low conf_high fraction conf_type notes
#>   <chr>         <chr>          <dbl>    <dbl>     <dbl> <chr>    <chr>     <chr>
#> 1 Cohen's Kappa 0.64 (0.54…    0.636    0.538     0.734 ""       Fleiss e… Subs…
```
