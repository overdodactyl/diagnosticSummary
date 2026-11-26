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

## Value

The table, with the matching cells edited.

## Examples

``` r
dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  setthreshold = .3,
  grouping_variables = "Sex"
)
forest <- dx_plot_forest(dx_obj, return = "grid")
# Make the text in the "Male" row red and bold
forest <- dx_edit_cell(
  forest,
  row = 4, col = 1:5,
  gp = grid::gpar(col = "red", fontface = "bold")
)
grid::grid.newpage()
grid::grid.draw(forest)
```
