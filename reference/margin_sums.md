# Calculate Margin Sums for a Contingency Table

Calculates the row and column sums for each 2x2 stratum of the
contingency table.

## Usage

``` r
margin_sums(stratum_data)
```

## Arguments

- stratum_data:

  A 2x2 matrix representing a single stratum of the contingency table.

## Value

A list with 'row' and 'col' elements containing the sums of rows and
columns, respectively.
