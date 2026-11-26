# Build a dx_cm object with specific cell counts by constructing truth and
# prediction vectors and passing them through dx_cm().
make_cm <- function(tp, fn, fp, tn) {
  truth <- rep(c(1, 1, 0, 0), times = c(tp, fn, fp, tn))
  predicted <- rep(c(1, 0, 1, 0), times = c(tp, fn, fp, tn))
  dx_cm(predicted, truth, threshold = .5, poslabel = 1)
}

# Confusion matrices covering balanced, imbalanced, small, large, and
# better/worse than chance classifiers. All cells are non-zero; zero cells
# are covered separately as edge cases.
cm_grid <- list(
  balanced    = c(tp = 40, fn = 10, fp = 20, tn = 30),
  rare        = c(tp = 8, fn = 2, fp = 45, tn = 945),
  common      = c(tp = 850, fn = 50, fp = 30, tn = 70),
  small       = c(tp = 3, fn = 2, fp = 1, tn = 4),
  large       = c(tp = 12000, fn = 3000, fp = 5000, tn = 80000),
  near_chance = c(tp = 25, fn = 25, fp = 24, tn = 26),
  worse       = c(tp = 10, fn = 40, fp = 35, tn = 15)
)

grid_cm <- function(x) {
  make_cm(x[["tp"]], x[["fn"]], x[["fp"]], x[["tn"]])
}

# Results are tibbles when tibble is installed and data frames with row names
# when it is not, so compare them as data frames without row names
plain_df <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}
