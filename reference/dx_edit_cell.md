# Edit a cell within an object returned from dx_forest

A convenient wrapper used to edit cells of a gtable.

## Usage

``` r
dx_edit_cell(table, row, col, name = "core-fg", ...)
```

## Arguments

- table:

  a table returned from dx_forest

- row:

  Numeric vector of rows to edit

- col:

  Numeric vector of columns to edit

- name:

  Name of table layer to edit

- ...:

  Parameters passed to grid::editGrob such as grid::gpar or hjust.
