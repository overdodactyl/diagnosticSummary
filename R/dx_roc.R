#' Plot ROC curve for a dx object
#'
#' @param dx_obj An object of class dx
#' @param curve_color Color of ROC curve
#' @param text_color Color of text on ROC curve
#' @param add_text (Logical) Include stats on ROC curve?
#' @param add_ref_lines (Logical) Include reference lines on ROC curve?
#' @param add_fractions (Logical) Include fractions on ROC curve?
#' @param summary_stats Statistics to include on ROC curve
#' @param filename File bane to create on disk.
#'     If left NA, no file will be created.
#' @importFrom grDevices dev.control dev.off pdf recordPlot
#' @importFrom graphics par
#' @export
#' @examples
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   study_name = "Heart Attack Prediction",
#'   data_description = "Validation Data",
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_roc(dx_obj)
dx_roc <- function(dx_obj, curve_color = "red", text_color = "black",
                   add_text = TRUE, add_ref_lines = TRUE, add_fractions = TRUE,
                   summary_stats = c(1, 2, 3, 4, 5, 6, 7, 8), filename = NA) {
  auc_results <- pROC::roc(
    eval(parse(text = paste0("dx_obj$data$", dx_obj$options$true_varname))),
    eval(parse(text = paste0("dx_obj$data$", dx_obj$options$pred_varname))),
    ci = T, quite = TRUE
  )

  sensdf <- dx_obj$data %>%
    dplyr::filter(!!as.name(dx_obj$options$true_varname) == 1) %>%
    dplyr::summarize(sens = mean(
      ifelse(!!as.name(dx_obj$options$pred_varname) <=
        dx_obj$options$setthreshold, 0, 1)
    )) %>%
    as.vector()

  specdf <- dx_obj$data %>%
    dplyr::filter(!!as.name(dx_obj$options$true_varname) == 0) %>%
    dplyr::summarize(spec = 1 - mean(
      ifelse(!!as.name(dx_obj$options$pred_varname) <=
        dx_obj$options$setthreshold, 0, 1)
    )) %>%
    as.vector()

  # Save plot to an object using a null PDF device
  pdf(NULL)
  dev.control(displaylist = "enable")
  par(bg = "white")
  graphics::plot(auc_results,
    main = dx_obj$options$outcome_label,
    print.auc = FALSE,
    ci = FALSE,
    col = curve_color,
    grid = T
  )

  if (add_ref_lines) {
    graphics::abline(h = sensdf[1], lty = 2)
    graphics::abline(v = specdf[1], lty = 2)
  }

  if (add_text) {
    showtext::showtext_begin()
    numsummary <- length(summary_stats)
    ystart <- 0.05
    yend <- ystart + (numsummary - 1) * 0.05
    location_vector <- seq(yend, ystart, -0.05)

    diagsummary_resultsdf <- as.data.frame(dx_obj,
      thresh = dx_obj$options$setthreshold,
      variable = "Overall"
    )

    diagsummary_resultsdf <- subset(diagsummary_resultsdf,
      select = -c(Variable, Label)
    )

    for (i in summary_stats) {
      if (nchar(diagsummary_resultsdf[i, 3]) > 0) {
        numden <- paste0(" (", diagsummary_resultsdf[i, 3], ")")
      } else {
        numden <- ""
      }

      if (!add_fractions) numden <- ""

      text(.01,
        location_vector[i],
        paste0(
          diagsummary_resultsdf[i, 1], ":  ",
          diagsummary_resultsdf[i, 2], numden
        ),
        cex = 1, adj = c(1, 1),
        col = text_color
      )
    }

    showtext::showtext_end()
  }

  plot <- recordPlot()
  invisible(dev.off())

  if (!is.na(filename)) {
    pdf(filename)
    plot
    dev.off()
  }

  plot
}
