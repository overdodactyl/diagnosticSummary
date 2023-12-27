#' Summary
#' @param object An object of class "dx"
#' @param thresh The threshold to return values from
#' @param variable Variable to include in returned values
#' @param level Level to include in returned values
#' @param show_var Include variable column in returned data?
#' @param show_label Include label in returned data?
#' @param ... additional arguments to be passed to or from methods
#' @export
summary.dx <- function(object, thresh = object$options$setthreshold,
                       variable = NA, label = NA, show_var = T,
                       show_label = T, measure = NA, ...) {
  if (is.na(thresh) | length(thresh) > 1) {
    stop("Must pass a numeric value to thresh")
  }

  tmp <- as.data.frame(object,
    thresh = thresh,
    variable = variable,
    label = label,
    measure = measure
  )

  rownames(tmp) <- NULL

  # Dropping columns 'rawestime', 'rawlci', 'rawuci'
  tmp <- tmp[, !names(tmp) %in% c('rawestime', 'rawlci', 'rawuci', 'ci_type', 'notes', 'n')]

  if (!show_var) tmp <- subset(tmp, select = -c(variable))
  if (!show_label) tmp <- subset(tmp, select = -c(label))

  caption <- paste0("N=", comma(tmp$n[1]), "; ", "Threshold=", thresh)

  tmp <- subset(tmp, select = -c(threshold))
  rownames(tmp) <- NULL
  if (requireNamespace("knitr", quietly = TRUE))  {
    knitr::kable(tmp, caption = caption, row.names = F)
  } else {
    print(caption)
    print(tmp)
  }



}

#' Convert to a data frame
#' @param x An object of class "dx"
#' @param thresh The threshold to return values from
#' @param row.names NULL or a character vector giving the row names for the
#'     data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column
#'     names (to syntactic names: see make.names) is optional.
#' @param variable Variable to include in returned values
#' @param level Level to include in returned values
#' @param ... additional arguments to be passed to or from methods
#' @export
as.data.frame.dx <- function(x, row.names = NULL, optional = TRUE, thresh = NA,
                             variable = NA, label = NA, measure = NA, ...) {

  tmp <- x$measures

  if (!is.na(thresh)) {
    tmp <- tmp[tmp$threshold %in% thresh, ]
  }
  if (!is.na(variable)) {
    tmp <- tmp[tmp$variable %in% variable, ]
  }
  if (!is.na(label)) {
    tmp <- tmp[tmp$label %in% label, ]
  }
  if (!is.na(measure)) {
    tmp <- tmp[tmp$measure %in% measure, ]
  }

  tmp
}
