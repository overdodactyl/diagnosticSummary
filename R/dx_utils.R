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
                       variable = NA, level = NA, show_var = T,
                       show_label = T, ...) {
  if (is.na(thresh) | length(thresh) > 1) {
    stop("Must pass a numeric value to thresh")
  }

  tmp <- as.data.frame(object,
    thresh = thresh,
    variable = variable,
    level = level
  )

  tmp <- dplyr::select(tmp, -rawestime, -rawlci, -rawuci)
  tmp <- dplyr::rename(tmp, `CI Type` = CI_Type)
  if (!show_var) tmp <- subset(tmp, select = -c(Variable))
  if (!show_label) tmp <- subset(tmp, select = -c(Label))
  tmp <- subset(tmp, select = -c(threshold))
  knitr::kable(tmp, caption = paste0("Threshold: ", thresh))
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
                             variable = NA, level = NA, ...) {
  tmp <- x$measures
  if (!is.na(thresh)) {
    tmp <- dplyr::filter(tmp, threshold %in% thresh)
  }
  if (!is.na(variable)) {
    tmp <- dplyr::filter(tmp, Variable %in% variable)
  }
  if (!is.na(level)) {
    tmp <- dplyr::filter(tmp, Level %in% level)
  }
  tmp
}
