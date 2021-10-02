#' Plot ROC curve for a dx object
#'
#' @param dx_obj An object of class dx
#' @param levels A character vector of length 2.  x an y labels for +/- classes
#' @param palette A character vector of length 2.  Colors for low and high ends of the fill gradient
#' @export
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_cm(dx_obj)
dx_cm <- function(dx_obj, palette = c("#FFFFFF", "#0057B8"), levels = c("-", "+")) {

  mat <- as.data.frame(table(dx_obj$data$true_binaryf, dx_obj$data$test_binaryf))
  names(mat) <- c("truth", "predicted", "count")
  mat <- dplyr::mutate_at(mat, 1:2, ~factor(., c("Negative", "Positive"),  0:1))


  ggplot2::ggplot(mat, mapping = ggplot2::aes(x = truth, y = predicted)) +
    ggplot2::geom_tile(ggplot2::aes(fill = count), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(count)), vjust = 1, size = 6) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low = palette[1], high = palette[2]) +
    ggplot2::scale_y_discrete(expand = c(0.2, 0), breaks = c(0, 1), labels = levels) +
    ggplot2::scale_x_discrete(expand = c(0.2, 0), breaks = c(0, 1), labels = levels) +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", size = 1.5),
      axis.text = ggplot2::element_text(size = 20),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 20)
    ) +
    ggplot2::labs(x = "\nTruth", y = "Predicted\n")

}
