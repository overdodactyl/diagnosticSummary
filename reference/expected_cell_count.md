# Calculate Expected Cell Count

Calculates the expected count of the top left cell (a) under the null
hypothesis of homogeneity of odds ratios across strata.

## Usage

``` r
expected_cell_count(a_value, marginals, odds_ratio)
```

## Arguments

- a_value:

  The observed count for cell 'a' in a single stratum.

- marginals:

  A list containing the 'row' and 'col' marginal sums for the stratum.

- odds_ratio:

  The overall odds ratio to use in the calculation.

## Value

The expected count for cell 'a'.
