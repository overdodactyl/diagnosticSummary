dx_prevalence_plot <- function(dx_obj) {
  prevalence <- dx_obj$prevalence

  # Assuming 'prevalence' is your dataframe name
  plot_data <- reshape(prevalence,
                       varying = list(c("ppv", "npv")),
                       v.names = "Value",
                       timevar = "Measure",
                       times = c("ppv", "npv"),
                       direction = "long",
                       idvar = "prevalence")

  plot_data$Measure <- toupper(plot_data$Measure)


  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(
      aes(x = prevalence, y = Value, color = Measure),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = c("#0057B8", "#E4002B")) +
    ggplot2::coord_fixed() +
    dx_roc_ggtheme() +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 1,
      colour = axis_color
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 1,
      colour = axis_color
    ) +
    ggplot2::labs(
      x = "\nPrevalence",
      y = "Value\n\n"
    )
}
