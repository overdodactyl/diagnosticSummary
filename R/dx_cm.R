#' Plot ROC curve for a dx object
#'
#' @param dx_obj An object of class dx
#' @param levels A character vector of length 2.  x an y labels for -/+ classes
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
dx_cm <- function(dx_obj, palette = c("#e5eef7", "#0057B8"), levels = c("-", "+")) {

  # Obtain the metrics as a dataframe
  metrics <- as.data.frame(dx_obj, thresh = dx_obj$options$setthreshold, variable = "Overall")

  predprob <- dx_obj$data[[dx_obj$options$pred_varname]]
  truth <- dx_obj$data[[dx_obj$options$true_varname]]

  perfdf <- dx_confusion_core(
    predprob = predprob,
    truth = truth,
    threshold = dx_obj$options$setthreshold,
    poslabel = dx_obj$options$poslabel
  )

  mat <- data.frame(
    truth = factor(c(0, 1, 0, 1)),
    predicted = factor(c(0, 0, 1, 1)),
    label = c("True Negative (TN)", "False Negative (FN)", "False Positive (FP)", "True Positive (TP)"),
    count = c(perfdf$tn, perfdf$fn, perfdf$fp, perfdf$tp)
  )

  sens_label <- paste0("Sensitivity", "\n", metrics[metrics$measure == "Sensitivity", "estimate"])
  spec_label <- paste0("Specificity", "\n", metrics[metrics$measure == "Specificity", "estimate"])
  npv_label <- paste0("NPV", "\n", metrics[metrics$measure == "Negative Predictive Value", "estimate"])
  ppv_label <- paste0("PPV", "\n", metrics[metrics$measure == "Positive Predictive Value", "estimate"])


  ggplot2::ggplot(mat, mapping = ggplot2::aes(x = truth, y = predicted)) +
    ggplot2::geom_tile(ggplot2::aes(fill = count), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = count), vjust = 0, size = 6) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = 2, size = 6) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low = palette[1], high = palette[2]) +
    ggplot2::scale_y_discrete(expand = c(0.2, 0), breaks = c(0, 1), labels = levels) +
    ggplot2::scale_x_discrete(expand = c(0.2, 0), breaks = c(0, 1), labels = levels) +
    ggplot2::theme(
      legend.position = "none",
      # panel.border = ggplot2::element_rect(fill = NA, colour = "black", size = 1.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 20),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 20)
    ) +
    ggplot2::annotate("text", x = 2, y = 2.55, vjust = 0 , size = 5, label = sens_label, fontface = 'italic') +
    ggplot2::annotate("text", x = 1, y = 2.55, vjust = 0 , size = 5, label = spec_label, fontface = 'italic') +
    ggplot2::annotate("text", x = 2.55, y = 2, hjust = 0, vjust = 1, size = 5, label = ppv_label, fontface = 'italic') +
    ggplot2::annotate("text", x = 2.55, y = 1, hjust = 0, vjust = 1, size = 5, label = npv_label, fontface = 'italic') +
    ggplot2::coord_cartesian(clip = "off", ylim = c(.8, 2.5), xlim = c(.8, 3)) +
    ggplot2::labs(x = "\nTruth", y = "Predicted\n")

}
