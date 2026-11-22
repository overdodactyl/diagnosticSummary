# Convert to a data frame

Convert to a data frame

## Usage

``` r
# S3 method for class 'dx'
as.data.frame(
  x,
  row.names = NULL,
  optional = TRUE,
  thresh = NA,
  variable = NA,
  label = NA,
  measure = NA,
  ...
)
```

## Arguments

- x:

  An object of class "dx"

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If TRUE, setting row names and converting column names (to
  syntactic names: see make.names) is optional.

- thresh:

  The threshold to return values from

- variable:

  Variable to include in returned values

- label:

  Labels to include in returned values

- measure:

  Measures to include

- ...:

  additional arguments to be passed to or from methods
