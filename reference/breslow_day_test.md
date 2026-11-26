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

  A 2x2xK contingency table of counts, where K is the number of strata
  (at least 2). Every stratum needs non-zero row and column totals.

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

## Examples

``` r
# Test result by outcome, stratified by sex
tab <- table(
  test = dx_heart_failure$predicted >= .3,
  truth = dx_heart_failure$truth,
  sex = dx_heart_failure$Sex
)
breslow_day_test(tab)
#> $statistic
#> [1] 2.369975
#> 
#> $p_value
#> [1] 0.1236891
#> 
#> $method
#> [1] "Breslow-Day"
#> 
#> $data_name
#> [1] "tab"
#> 
breslow_day_test(tab, correct = TRUE)
#> $statistic
#> [1] 2.36099
#> 
#> $p_value
#> [1] 0.1244032
#> 
#> $method
#> [1] "Tarone's corrected Breslow-Day"
#> 
#> $data_name
#> [1] "tab"
#> 
```
