dx_gain <- function(dx_obj) {

  data <- dx_obj$rank

  ggplot2::ggplot(data, ggplot2::aes(x = percentile, y = gain)) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
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
    ggplot2::xlab("\nPercentile of Population") +
    ggplot2::ylab("Gain (Fraction of Total Positives Captured)\n\n")


}

dx_lift <- function(dx_obj) {

  data <- dx_obj$rank

  # Plotting the lift curve
  ggplot2::ggplot(data, ggplot2::aes(x = percentile, y = lift)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    dx_roc_ggtheme() +
    ggplot2::geom_hline(
      yintercept = 1,  # Lift of 1 for random model
      linetype = "dashed",
      color = "gray"
    ) +
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
    ggplot2::xlab("\nPercentile of Population") +
    ggplot2::ylab("Lift\n\n")

}


dx_ks_plot <- function(dx_obj) {

  data <- dx_obj$rank

  long_data <- rbind(
    data.frame(Rate = data$cumulativeTPR, Type = "TPR", predprob = data$predprob),
    data.frame(Rate = data$cumulativeFPR, Type = "FPR", predprob = data$predprob)
  )

  # Calculate KS Statistic
  ks_statistic <- max(abs(data$cumulativeTPR - data$cumulativeFPR))

  ks_point <- which.max(abs(data$cumulativeTPR - data$cumulativeFPR))

  ggplot2::ggplot(long_data, ggplot2::aes(x = predprob, y = Rate, color = Type)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::coord_fixed() +
    ggplot2::xlab("Prediction Score (Descending)") +
    ggplot2::ylab("Cumulative Proportion") +
    ggplot2::geom_vline(xintercept = data$predprob[ks_point], linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = data$predprob[ks_point] + 0.02,
      y = 1,
      label = paste("KS =", round(ks_statistic, 2)),
      hjust = 0
    ) +
    ggplot2::scale_color_manual(values = c("TPR" = "blue", "FPR" = "red")) +
    ggplot2::theme_minimal()

}

