# Return an pROC::roc object for a dx object

Return an pROC::roc object for a dx object

## Usage

``` r
get_roc(true_varname, pred_varname, data, direction)
```

## Arguments

- true_varname:

  Column name containing AI reference standard (string)

- pred_varname:

  Column name containing AI prediction (string)

- data:

  A tbl.

- direction:

  Direction for roc comparison. See ?pROC::roc
