#' Plot ROC curve for a dx object
#'
#' @param dx_obj An object of class dx
#' @param curve_color Color of ROC curve
#' @param text_color Color of text on ROC curve
#' @param add_text (Logical) Include stats on ROC curve?
#' @param add_ref_lines (Logical) Include reference lines on ROC curve?
#' @param add_fractions (Logical) Include fractions on ROC curve?
#' @param summary_stats Statistics to include on ROC curve
#' @param axis_color Color of x and y axis
#' @param add_ref_circle (Logical) Include a circle around specificity and
#'     sensitivity at the specified threshold?
#' @param circle_ref_color Color of reference circle
#' @param fill_color Color to be filled in under the ROC curve.
#'     Use "transparent" if you do not want a color to show.
#' @param filename File name to create on disk using ggplot2::ggsave.
#'     If left NA, no file will be created.
#' @param ref_lines_color Color for reference lines
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
dx_roc <- function(dx_obj, curve_color = "#0057B8", fill_color = "#cfcdcb",
                    text_color = "black", add_text = TRUE, add_ref_lines = TRUE,
                    add_fractions = TRUE, axis_color = "#333333",
                    add_ref_circle = TRUE, ref_lines_color = "#8a8887",
                    circle_ref_color = "#E4002B",
                    summary_stats = c(1, 2, 3, 4, 5, 6, 7, 8), filename = NA) {


  auc_results <- pROC::roc(
    eval(parse(text = paste0("dx_obj$data$", dx_obj$options$true_varname))),
    eval(parse(text = paste0("dx_obj$data$", dx_obj$options$pred_varname))),
    #direction=">",
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

  # get and sort the sensitivities and specificities
  se <- sort(auc_results$se, decreasing = FALSE)
  sp <- sort(auc_results$sp, decreasing = TRUE)

  auc_df <- data.frame(
    se = se,
    sp = sp
  )


  auc_poly <- auc_df %>%
    dplyr::group_by(sp) %>%
    dplyr::summarise(se = max(se))


  p <- ggplot2::ggplot(auc_df, ggplot2::aes(sp, se)) +
    ggplot2::geom_area(
      data = auc_poly,
      mapping = ggplot2::aes(sp, se),
      fill = fill_color,
      colour = NA,
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      color = curve_color,
      size = 1
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_hline(
      yintercept = 0,
      size = 1,
      colour = axis_color
    ) +
    ggplot2::geom_vline(
      xintercept = 1.05,
      size = 1,
      colour = axis_color
    )


  if (add_ref_lines) {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(x = 1, y = 0, xend = 0, yend = 1),
        size = .2,
        linetype = "dotted",
        color = ref_lines_color
      ) +
      # horizontal line
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = 1.05, y = sensdf[[1]], yend = sensdf[[1]]),
        size = .2,
        linetype = "dashed",
        color = ref_lines_color
      ) +
      # vertical line
      ggplot2::geom_segment(
        ggplot2::aes(x = specdf[[1]], xend = specdf[[1]], y = 0, yend = 1.05),
        size = .2,
        linetype = "dashed",
        color = ref_lines_color
      )
  }

  if (add_ref_circle) {
    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = specdf[[1]], y = sensdf[[1]]),
        fill = NA,
        size = 6,
        shape = 1,
        stroke = 1,
        color = circle_ref_color,
        inherit.aes = FALSE
      )
  }

  p <- p +
    ggplot2::coord_fixed() +
    dx_roc_ggtheme() +
    ggplot2::labs(
      x = "\nSpecificity",
      y = "Sensitivity\n\n"
    )

  if (add_text) {
    numsummary <- length(summary_stats)
    ystart <- 0.05
    yend <- ystart + (numsummary - 1) * 0.05
    location_vector <- seq(yend, ystart, -0.05)

    diagsummary_resultsdf <-
      as.data.frame(
        dx_obj,
        thresh = dx_obj$options$setthreshold,
        variable = "Overall"
      )

    diagsummary_resultsdf <-
      subset(
        diagsummary_resultsdf,
        select = -c(variable, label)
      )

    diagsummary_resultsdf <- diagsummary_resultsdf %>%
      dplyr::filter(dplyr::row_number() %in% summary_stats) %>%
      dplyr::mutate(
        numden = dplyr::case_when(
          !add_fractions ~ "",
          nchar(fraction) == 0 ~ "",
          TRUE ~ paste0(" (", fraction, ")")
        ),
        y = location_vector,
        label = paste0(measure, ":  ", estimate, numden)
      )

    p <- p +
      ggplot2::geom_text(
        data = diagsummary_resultsdf,
        mapping = ggplot2::aes(x = .05, y = y, label = label, hjust = 1),
        color = text_color
      )
  }

  if (!is.na(filename)) {
    ggplot2::ggsave(
      plot = p,
      filename = filename,
      height = 8,
      width = 8,
      units = "in",
      dpi = 600
    )
  }

  p

}

#' Theme used for dx_roc
dx_roc_ggtheme <- function() {
  font <- "Helvetica"
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = font,
      size = 22,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 16,
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    plot.caption = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(
      family = font,
      size = 14,
      color = "#222222"
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 22, hjust = 0),
    axis.title = ggplot2::element_text(size = 14)
  )
}
