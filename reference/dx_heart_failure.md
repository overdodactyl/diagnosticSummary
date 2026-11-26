# Heart disease outcomes and predictions

A data set containing heart disease outcomes for 261 patients along with
predicted probabilities of having heart disease from two models.

## Usage

``` r
dx_heart_failure
```

## Format

A data frame with 261 rows and 6 variables:

- AgeGroup:

  Age group

- Sex:

  Sex

- truth:

  Heart disease (outcome)

- predicted:

  Predicted probability from a logistic regression model

- predicted_rf:

  Predicted probability from a random forest model

- AgeSex:

  Age and sex group

## Source

Janosi A, Steinbrunn W, Pfisterer M, Detrano R (1989). Heart Disease.
UCI Machine Learning Repository.
[doi:10.24432/C52P4X](https://doi.org/10.24432/C52P4X) . Licensed under
CC BY 4.0.

## Details

The outcomes come from the Hungarian Institute of Cardiology records in
the UCI Heart Disease data set (complete cases only). The predictions
were fit to the same data, so they are for illustration only. See
`data-raw/dx_heart_failure.R` in the package source.
