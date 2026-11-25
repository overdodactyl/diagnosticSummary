# Breslow-Day Test for Homogeneity of Odds Ratios

Performs the Breslow-Day test for homogeneity of the odds ratios across
different strata of data. Optionally includes Tarone's correction. This
test is useful for investigating whether the effect of an explanatory
variable on the outcome is consistent across different strata.

## Usage

``` r
breslow_day_test(contingency_table, odds_ratio = NA, correct = FALSE)
```

## Arguments

- contingency_table:

  A 2x2xK contingency table where K is the number of strata.

- odds_ratio:

  Optionally, the overall odds ratio to use in the test. If not
  provided, the Mantel-Haenszel odds ratio is calculated.

- correct:

  Logical indicating whether to apply Tarone's correction.

## Value

A list with the Breslow-Day test statistic, p-value, method used, and
data name. The statistic and p-value are `NA` when the common odds ratio
is 0 or infinite, e.g. when the outcome is perfectly separated in every
stratum.

## References

Breslow NE, Day NE. Statistical methods in cancer research. Volume I -
The analysis of case-control studies. IARC Sci Publ. 1980;(32):5-338.
