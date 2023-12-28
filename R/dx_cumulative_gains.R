dx_gain <- function(dx_obj) {

  truth = dx_obj$data[[dx_obj$options$true_varname]]
  predprob = dx_obj$data[[dx_obj$options$pred_varname]]


  # Ensure the predprob and truth are ordered by predicted probability
  data <- data.frame(truth, predprob)
  data <- data[order(-data$predprob),]  # descending order

  # Calculate cumulative true positives
  data$cumulativeTruePositives <- cumsum(data$truth)

  # Calculate the fraction of positives at each threshold
  totalPositives <- sum(data$truth)
  data$gain <- data$cumulativeTruePositives / totalPositives

  # Calculate the percentile for each instance
  data$percentile <- seq(from = 0, to = 1, length.out = nrow(data))

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

  truth = dx_obj$data[[dx_obj$options$true_varname]]
  predprob = dx_obj$data[[dx_obj$options$pred_varname]]

  # Ensure the predprob and truth are ordered by predicted probability
  data <- data.frame(truth, predprob)
  data <- data[order(-data$predprob),]  # descending order

  # Calculate cumulative true positives and total positives
  data$cumulativeTruePositives <- cumsum(data$truth)
  totalPositives <- sum(data$truth)

  # Calculate the lift for each percentile
  data$percentile <- seq(from = 0, to = 1, length.out = nrow(data))
  data$randomModelGain <- data$percentile * totalPositives  # Expected gain from random model
  data$lift <- data$cumulativeTruePositives / data$randomModelGain

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
