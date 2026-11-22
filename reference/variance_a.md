# Calculate Variance of Cell Count

Calculates the variance of the top left cell (a) count under the null
hypothesis of homogeneity of odds ratios across strata.

## Usage

``` r
variance_a(tilde_a_value, marginals)
```

## Arguments

- tilde_a_value:

  The expected count for cell 'a' in a single stratum.

- marginals:

  A list containing the 'row' and 'col' marginal sums for the stratum.

## Value

The variance of the count for cell 'a'.
