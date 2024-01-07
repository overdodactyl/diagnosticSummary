#' Plot Gain Chart
#'
#' Constructs a Gain chart from a `dx` object. Gain charts are an evaluative tool for binary
#' classification models, displaying the effectiveness of the model in identifying positive instances.
#'
#' @param dx_obj A `dx` object containing diagnostic measurements, including a rank data frame
#' with percentile and gain columns. This object is typically the output of a diagnostic
#' function that computes various metrics for evaluating model performance.
#' @return A `ggplot` object representing the Gain chart, allowing for further customization if desired.
#' @export
#' @details
#' The Gain chart plots the cumulative percentage of true positive cases (gain) against the
#' percentage of cases if evaluated by the model, ranked by the predicted probability of being positive.
#' The x-axis represents the percentile of the population when ordered by the model's predicted
#' probability of being positive. The y-axis represents the cumulative percentage of true positive
#' cases found up to each percentile. The slope of the curve indicates the model's ability to prioritize
#' positive cases over negatives. A steep curve towards the top-left indicates a model that effectively
#' ranks positive cases higher than negatives, capturing a large proportion of positives early in the ranking.
#' Conversely, a curve close to the diagonal suggests performance close to random chance, where the model does
#' not effectively differentiate between positive and negative cases.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_gain(dx_obj)
dx_plot_gain <- function(dx_obj) {
  data <- dx_obj$rank

  ggplot2::ggplot(data, ggplot2::aes(x = .data$percentile, y = .data$gain)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::coord_fixed() +
    ggplot2::xlab("\nPercentile of Population") +
    ggplot2::ylab("Gain (Fraction of Total Positives Captured)\n\n")
}

#' Plot Lift Curve
#'
#' Generates a Lift chart from a `dx` object. Lift charts are used to evaluate the performance
#' of binary classification models by comparing the results of using the model versus a random
#' selection. The Lift chart plots the ratio of the results obtained with the model to those
#' obtained by a random model, across different percentiles of the population.
#'
#' @param dx_obj A `dx` object containing diagnostic measurements, including a rank data frame
#' with percentile and lift columns. The rank data frame should be the result of a diagnostic
#' process that scores and ranks each instance based on the likelihood of being a true positive.
#' @return A `ggplot` object representing the Lift chart, which can be further customized as needed.
#' @export
#' @details
#' The Lift chart visualizes how much more likely we are to capture positive instances when using
#' the model's predictions compared to a random guess. The x-axis represents the percentile of the
#' population when ordered by the predicted probabilities, and the y-axis represents the lift, which
#' is calculated as the ratio of the cumulative gain at each percentile to the gain expected by chance.
#' A value greater than 1 indicates that the model is performing better than random, with higher
#' values representing better performance. A horizontal dashed line at y=1 represents the baseline
#' lift of a random model. The lift curve should ideally stay above this line to indicate that the
#' model has predictive power.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_lift(dx_obj)
dx_plot_lift <- function(dx_obj) {
  data <- dx_obj$rank

  # Plotting the lift curve
  ggplot2::ggplot(data, ggplot2::aes(x = .data$percentile, y = .data$lift)) +
    ggplot2::geom_line(color = "red", linewidth = 1) +
    ggplot2::theme_minimal() +
    # dx_roc_ggtheme() +
    ggplot2::geom_hline(
      yintercept = 1, # Lift of 1 for random model
      linetype = "dashed",
      color = "gray"
    ) +
    ggplot2::xlab("\nPercentile of Population") +
    ggplot2::ylab("Lift\n\n")
}


#' Plot Kolmogorov-Smirnov Curve
#'
#' Generates a Kolmogorov-Smirnov (KS) plot from a `dx` object. The KS plot is a graphical
#' tool used to assess the discriminatory power of a binary classification model by visualizing
#' the difference between the cumulative distribution functions of the true positive rate (TPR)
#' and false positive rate (FPR) across different thresholds.
#'
#' @param dx_obj A `dx` object containing diagnostic measurements, including a rank data frame
#' with columns for cumulative true positive rate (TPR) and false positive rate (FPR), as well
#' as the predicted probabilities associated with each instance.
#' @return A `ggplot` object representing the KS plot, which can be further customized as needed.
#' @export
#' @details
#' The KS plot displays two lines representing the cumulative TPR and FPR, with the x-axis showing
#' the prediction scores in descending order and the y-axis showing the cumulative proportion of
#' positive and negative instances. The point where the difference between the TPR and FPR is maximal
#' indicates the threshold with the highest potential for separating positive from negative instances.
#' This point is marked with a dashed vertical line on the plot, and the KS statistic, representing
#' the maximum distance between the two lines, is annotated on the plot. A higher KS statistic indicates
#' a model with better discriminatory ability. The ideal model would have a KS plot with the TPR line
#' close to the top-left corner and the FPR line close to the bottom-right corner, maximizing the distance
#' between the two lines.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_ks(dx_obj)
dx_plot_ks <- function(dx_obj) {
  data <- dx_obj$rank

  long_data <- rbind(
    data.frame(Rate = data$cumulativeTPR, Type = "TPR", predprob = data$predprob),
    data.frame(Rate = data$cumulativeFPR, Type = "FPR", predprob = data$predprob)
  )

  # Calculate KS Statistic
  ks_statistic <- max(abs(data$cumulativeTPR - data$cumulativeFPR))

  ks_point <- which.max(abs(data$cumulativeTPR - data$cumulativeFPR))

  ggplot2::ggplot(long_data, ggplot2::aes(x = .data$predprob, y = .data$Rate, color = .data$Type)) +
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

#' Plot Precision-Recall Curve
#'
#' Plots a Precision-Recall (PR) curve from a `dx` object. The PR curve is a useful tool for
#' evaluating the performance of a binary classification model, especially in situations where
#' classes are imbalanced. It shows the trade-off between precision and recall for different
#' thresholds.
#'
#' @param dx_obj A `dx` object containing diagnostic measurements, including a data frame
#' with sensitivity (recall) and precision values at various thresholds.
#' @return A `ggplot` object representing the PR curve, which can be further customized as needed.
#' @export
#' @details
#' The PR curve is a plot that demonstrates the relationship between precision (the proportion
#' of true positive predictions among all positive predictions) and recall (the proportion of
#' true positive predictions among all actual positives), for each possible cut-off threshold.
#' A model with perfect performance would show a line that reaches the top-right corner of the
#' plot, indicating both high precision and recall. The area under the PR curve (AUPRC) can also
#' be used as a summary measure of the model performance, with higher values indicating better
#' performance. This function generates the curve without calculating the area; separate functions
#' should be used if AUPRC is required.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_pr(dx_obj)
dx_plot_pr <- function(dx_obj) {
  data <- dx_obj$thresholds
  data <- stats::na.omit(data)

  ggplot2::ggplot(data, ggplot2::aes(x = .data$sensitivity, y = .data$precision)) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::coord_fixed(ylim = c(0, 1)) +
    ggplot2::xlab("Recall") +
    ggplot2::ylab("Precision") +
    ggplot2::theme_minimal()
}

#' Plot Predictive Values Against Prevalence
#'
#' Generates a plot of Positive Predictive Value (PPV) and Negative Predictive Value (NPV)
#' across a range of disease prevalences. This plot helps in understanding how the PPV and NPV
#' of a diagnostic test vary with the prevalence of the condition.
#'
#' @param dx_obj A `dx` object containing the prevalence analysis data, including calculated
#' PPV and NPV for various prevalence levels.
#' @return A `ggplot` object representing the Predictive Value Plot.
#' @export
#' @details
#' The Predictive Value Plot visualizes how the Positive Predictive Value (PPV) and Negative
#' Predictive Value (NPV) of a test change with varying disease prevalence. Typically, as
#' prevalence increases, PPV increases while NPV decreases. This is because when a disease
#' is more common (higher prevalence), a positive test result is more likely to be a true
#' positive, thus increasing the PPV. Conversely, when the disease is less common (lower
#' prevalence), a negative test result is more likely to be a true negative, increasing the NPV.
#'
#' The impact of prevalence on PPV and NPV is a fundamental concept in medical testing, where
#' understanding the population's disease prevalence is crucial in interpreting test results.
#' For rare conditions, even tests with high sensitivity and specificity can have a low PPV,
#' meaning that most positive results are false positives. Similarly, for very common conditions,
#' the NPV can decrease, indicating that negative results become less reliable. This plot helps
#' in visualizing these relationships and is a valuable tool in the evaluation of diagnostic tests,
#' allowing healthcare professionals and researchers to anticipate how well a test will perform
#' in different scenarios.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_predictive_value(dx_obj)
dx_plot_predictive_value <- function(dx_obj) {
  prevalence <- dx_obj$prevalence

  plot_data <- stats::reshape(prevalence,
    varying = list(c("ppv", "npv")),
    v.names = "Value",
    timevar = "Measure",
    times = c("ppv", "npv"),
    direction = "long",
    idvar = "prevalence"
  )

  plot_data$Measure <- toupper(plot_data$Measure)


  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data$prevalence, y = .data$Value, color = .data$Measure),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = c("#0057B8", "#E4002B")) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "\nPrevalence",
      y = "Value\n\n"
    )
}

#' Plot Calibration Curve
#'
#' Generates a calibration plot to assess the calibration of predicted probabilities against observed outcomes.
#' The function plots the mean predicted probability against the observed proportion of positive outcomes
#' for different bins of predicted probabilities.
#'
#' @param dx_obj A list or object containing the model's predicted probabilities (`predicted_probs`) and the actual binary outcomes (`true_labels`).
#' @param bins An integer specifying the number of bins to divide the predicted probabilities into. Default is 10 (deciles).
#'
#' @return A `ggplot` object representing the calibration curve, which can be further customized as needed.
#'
#' @export
#'
#' @details
#' The calibration curve is an important diagnostic tool to understand how well a model's predicted probabilities
#' match the observed frequencies of the outcomes. A well-calibrated model will have a calibration curve that closely
#' follows the diagonal line y = x, indicating that the predicted probabilities are reflective of the true likelihood
#' of the event. The curve is constructed by dividing the predicted probabilities into `bins` number of bins and then
#' plotting the average predicted probability against the observed proportion of positive outcomes in each bin.
#' The number of bins can be adjusted to change the granularity of the calibration curve.
#'
#' A calibration curve that deviates significantly from the diagonal line indicates that the model may be under or overestimating
#' the probabilities. In such cases, calibration methods like isotonic regression or Platt scaling might be applied to adjust
#' the model's predicted probabilities.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_calibration(dx_obj)
dx_plot_calibration <- function(dx_obj, bins = 10) {
  predicted_probs <- dx_obj$data[[dx_obj$options$pred_varname]]
  true_labels <- dx_obj$data[[dx_obj$options$true_varname]]

  # Create bins for predicted probabilities
  prob_bins <- cut(predicted_probs, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)

  # Aggregate data by bins to get mean predicted probability and observed proportion
  calibration_data <- stats::aggregate(cbind(predicted_probs, true_labels) ~ prob_bins, FUN = mean)
  names(calibration_data) <- c("prob_bins", "mean_predicted_prob", "observed_proportion")

  # Create calibration plot
  ggplot2::ggplot(
    calibration_data,
    ggplot2::aes(x = .data$mean_predicted_prob, y = .data$observed_proportion)
  ) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2, color = "red") + # Diagonal line for perfect calibration
    ggplot2::xlab("Mean Predicted Probability") +
    ggplot2::ylab("Observed Proportion") +
    # ggplot2::ggtitle("Calibration Curve") +
    ggplot2::theme_minimal()
}

#' Plot Decision Curve
#'
#' Generates a decision curve to visualize the net benefit of a binary
#' classification model across a specified range of threshold probabilities.
#' The decision curve shows the net benefit of using the model at different
#' threshold probabilities compared to the strategy of treating all or none.
#'
#' @param dx_obj A `dx` object containing a 'thresholds' data frame with columns for 'threshold' and 'net_benefit'.
#' @param plot_range A numeric vector of length 2 specifying the lower and upper bounds for the range of threshold probabilities to be plotted. Defaults to c(0.05, .95).
#'
#' @return A `ggplot` object representing the decision curve, which can be further customized as needed.
#'
#' @export
#'
#' @details
#' Decision Curve Analysis (DCA) is a method for evaluating and comparing the
#' clinical usefulness of prediction models by considering the clinical
#' consequences of decision making. The decision curve plots the net benefit of
#' using the model across a range of threshold probabilities for making a decision.
#' The net benefit is calculated as the true positive rate (sensitivity) minus the
#' weighted false positive rate, where the weight is the ratio of the cost of
#' false positives relative to the benefit of true positives. This ratio is derived from
#' the threshold probability:
#' \deqn{Net Benefit = Sensitivity - (Weight * False Positive Rate)}
#' where Weight = Threshold / (1 - Threshold).
#'
#' The decision curve will typically be compared against two
#' default strategies: treating all patients or treating none.
#' The net benefit is higher when the model's predictions provide a
#' clear advantage over these default strategies. A well-performing model
#' will have a decision curve that stays above the treat-none and treat-all
#' lines for a range of clinically relevant threshold probabilities.
#'
#' The 'plot_range' parameter allows limiting the analysis to a relevant range of thresholds,
#' avoiding the extremes where the net benefit calculation can become unstable or not clinically relevant.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_decision_curve(dx_obj)
dx_plot_decision_curve <- function(dx_obj, plot_range = c(0.05, .95)) {

  # Prepare data for plotting
  data_for_plot <- data.frame(
    threshold = dx_obj$thresholds$threshold,
    net_benefit = dx_obj$thresholds$net_benefit
  )

  data_for_plot <- stats::na.omit(data_for_plot)

  data_for_plot <- data_for_plot[data_for_plot$threshold >= plot_range[1], ]
  data_for_plot <- data_for_plot[data_for_plot$threshold <= plot_range[2], ]

  prevalence <- mean(dx_obj$data[[dx_obj$options$true_varname]])

  # Treat All Strategy: Net Benefit when everyone is treated
  treat_all_net_benefit <- prevalence - (1 - prevalence) * data_for_plot$threshold / (1 - data_for_plot$threshold)

  # Treat None Strategy: Net Benefit when no one is treated (always 0)
  treat_none_net_benefit <- rep(0, length(data_for_plot$threshold))




  # Create the decision curve plot
  ggplot2::ggplot(data_for_plot, ggplot2::aes(x = .data$threshold, y = .data$net_benefit)) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(x = data_for_plot$threshold, y = treat_all_net_benefit), color = "red", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(x = data_for_plot$threshold, y = treat_none_net_benefit), color = "green", linetype = "dashed") +
    ggplot2::xlab("Threshold Probability") +
    ggplot2::ylab("Net Benefit") +
    ggplot2::theme_minimal()
}

#' Plot Youden's J Index Curve
#'
#' Generates a plot of Youden's J Index (Informedness) across a range of threshold
#' probabilities for a binary classification model. The curve shows how the trade-off
#' between sensitivity and specificity varies with the threshold.
#'
#' @param dx_obj A `dx` object containing a 'thresholds' data frame with columns for
#' 'threshold' and 'informedness'.
#'
#' @return A `ggplot` object representing the Youden's J Index curve, which can be
#' further customized as needed.
#'
#' @export
#'
#' @details
#' Youden's J Index is a summary measure of the diagnostic effectiveness of a biomarker
#' or test, defined as the maximum vertical distance between the ROC curve and the
#' diagonal line. It is calculated as \(J = Sensitivity + Specificity - 1\), where a
#' higher Youden's J Index indicates a better test performance with a better trade-off
#' between sensitivity and specificity. The index varies between -1 and 1, where 0
#' indicates no better performance than random guessing and 1 indicates perfect performance.
#'
#' The curve plots the Youden's J Index against different threshold probabilities,
#' enabling the visualization of how model performance (in terms of the balance between
#' sensitivity and specificity) changes across thresholds. The peak of the curve
#' indicates the threshold with the optimal balance between sensitivity and specificity
#' according to the Youden's J Index.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_youden_j(dx_obj)
dx_plot_youden_j <- function(dx_obj) {

  # Prepare data for plotting
  data_for_plot <- data.frame(
    threshold = dx_obj$thresholds$threshold,
    youden_j = dx_obj$thresholds$informedness
  )

  data_for_plot <- stats::na.omit(data_for_plot)


  # Plot Youden's J Index
  ggplot2::ggplot(data_for_plot, ggplot2::aes(x = .data$threshold, y = .data$youden_j)) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::xlab("Threshold Probability") +
    ggplot2::ylab("Youden's J Index") +
    ggplot2::theme_minimal()
}

#' Plot Cumulative Accuracy Profile (CAP) Curve
#'
#' Generates a Cumulative Accuracy Profile (CAP) Curve to evaluate and visualize the
#' performance of a binary classification model by comparing its cumulative accuracy
#' against that of perfect and random models.
#'
#' @param dx_obj A `dx` object containing rank-based statistics, including cumulative
#' true positives and percentiles. It's expected to have a 'rank' element with the necessary data.
#'
#' @return A `ggplot` object representing the CAP Curve with actual, perfect, and random
#' model lines, allowing for further customization if desired.
#'
#' @export
#'
#' @details
#' The CAP Curve provides a visual representation of the effectiveness of a binary classification
#' model in ranking positive instances compared to two baseline strategies: a perfect model that
#' captures all positives immediately and a random model that captures positives uniformly at random.
#' The curve illustrates the cumulative percentage of captured positives (Y-axis) as a function of
#' the cumulative percentage of instances (X-axis).
#'
#' The function plots three lines:
#' - Actual Model: Represents the cumulative accuracy of the given model.
#' - Perfect Model: Represents the hypothetical scenario where all positive instances are ranked
#'   before any negative instances.
#' - Random Model: Represents the baseline scenario of randomly guessing the class.
#'
#' The CAP Curve is useful for understanding not just the accuracy but also the behavior of the
#' model across the entire range of classifications. It's particularly insightful for imbalanced
#' datasets where positive instances are rare or more critical.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_cap(dx_obj)
dx_plot_cap <- function(dx_obj) {
  data <- dx_obj$rank

  # Total positives in the dataset
  total_positives <- sum(data$truth)

  # Create the CAP data
  cap_data <- data.frame(
    Percent_Instances = data$percentile,
    CAP = data$cumulativeTruePositives / total_positives
  )

  # Perfect and Random Model Lines
  cap_data$PerfectModel <- ifelse(
    cap_data$Percent_Instances <= total_positives / nrow(data),
    cap_data$Percent_Instances / (total_positives / nrow(data)),
    1
  )

  cap_data$RandomModel <- cap_data$Percent_Instances

  long_data <- rbind(
    data.frame(x = cap_data$Percent_Instances, y = cap_data$CAP, Model = "Actual Model", lt = "solid"),
    data.frame(x = cap_data$Percent_Instances, y = cap_data$PerfectModel, Model = "Perfect Model", lt = "dotted"),
    data.frame(x = cap_data$Percent_Instances, y = cap_data$RandomModel, Model = "Random Model", lt = "dotted")
  )

  # Plotting the CAP curve
  ggplot2::ggplot(long_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(ggplot2::aes(color = .data$Model, linetype = .data$lt), linewidth = 1) +
    ggplot2::scale_color_manual(values = c("#0057B8", "darkgreen", "darkred")) +
    ggplot2::scale_linetype_identity() +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      # title = "Cumulative Accuracy Profile (CAP) Curve",
      x = "Cumulative Percentage of Instances",
      y = "Cumulative Percentage of Captured Positives"
    ) +
    ggplot2::theme_minimal()
}

#' Plot Cost Curve
#'
#' @description
#' Generates and plots a cost curve for a binary classification model over a range of
#' thresholds, illustrating the total cost associated with different classification
#' thresholds based on specified costs for false positives, false negatives, true positives,
#' and true negatives.
#'
#' @param dx_obj A `dx` object containing threshold-based statistics including false
#' positives (fp) and false negatives (fn) for each threshold.
#' @param cfp Cost of a false positive.
#' @param cfn Cost of a false negative.
#' @param ctp Benefit (negative cost) of a true positive. Default is 0.
#' @param ctn Benefit (negative cost) of a true negative. Default is 0.
#'
#' @return A ggplot object representing the cost curve, displaying total cost on the
#' y-axis and classification threshold on the x-axis.
#'
#' @details
#' The cost curve represents the total cost associated with a binary classification model
#' as the classification threshold is varied. It is calculated as:
#' \deqn{Total Cost = CFP * FP + CFN * FN - CTP * TP - CTN * TN}
#' where FP, FN, TP, and TN are the counts of false positives, false negatives, true positives,
#' and true negatives at each threshold, respectively.
#'
#' The curve helps in identifying the optimal threshold that minimizes total cost or maximizes
#' overall benefit, considering the specific cost/benefit structure of different outcomes in
#' a particular application.
#'
#' Typically, the total cost will vary depending on the threshold, reflecting the trade-off
#' between reducing one type of error and increasing another. By adjusting the threshold,
#' users can determine the point at which the model provides the best balance according to
#' the specified cost matrix.
#'
#' @export
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_cost(dx_obj, cfp = 1000, cfn = 8000)
dx_plot_cost <- function(dx_obj, cfp, cfn, ctp = 0, ctn = 0) {
  thresholds <- dx_obj$thresholds

  # Calculate total cost at each threshold
  thresholds$total_cost <- cfp * thresholds$fp + cfn * thresholds$fn - ctp * thresholds$tp - ctn * thresholds$tn

  ggplot2::ggplot(thresholds, ggplot2::aes(x = .data$threshold, y = .data$total_cost)) +
    ggplot2::geom_line(color = "#0057B8", linewidth = 1) +
    ggplot2::xlab("Threshold") +
    ggplot2::ylab("Total Cost") +
    ggplot2::theme_minimal()
}

#' Plot Confusion Matrix with Metrics
#'
#' Creates a graphical representation of a confusion matrix from a `dx` object.
#' The plot displays the counts of true positives, false negatives, true negatives,
#' and false positives. Additionally, it annotates the plot with relevant performance
#' metrics such as Sensitivity, Specificity, PPV, and NPV, along with their
#' confidence intervals where applicable.
#'
#' @param dx_obj An object of class "dx" containing the diagnostic measurements.
#' @param levels A character vector of length 2 specifying the labels for negative
#' and positive classes, respectively. Defaults to c("-", "+").
#' @param palette A character vector of length 2 specifying the colors for the
#' low and high ends of the fill gradient used in the plot. Defaults to
#' c("#e5eef7", "#0057B8").
#' @return A `ggplot` object that represents the confusion matrix with additional
#' performance metrics.
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_cm(dx_obj)
dx_plot_cm <- function(dx_obj, palette = c("#e5eef7", "#0057B8"), levels = c("-", "+")) {

  # Obtain the metrics as a dataframe
  metrics <- as.data.frame(dx_obj, thresh = dx_obj$options$setthreshold, variable = "Overall")

  predprob <- dx_obj$data[[dx_obj$options$pred_varname]]
  truth <- dx_obj$data[[dx_obj$options$true_varname]]

  perfdf <- dx_cm(
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

  sens_label <- paste0("Sensitivity", "\n", metrics[metrics$measure == "Sensitivity", "summary"])
  spec_label <- paste0("Specificity", "\n", metrics[metrics$measure == "Specificity", "summary"])
  npv_label <- paste0("NPV", "\n", metrics[metrics$measure == "Negative Predictive Value", "summary"])
  ppv_label <- paste0("PPV", "\n", metrics[metrics$measure == "Positive Predictive Value", "summary"])


  ggplot2::ggplot(mat, mapping = ggplot2::aes(x = .data$truth, y = .data$predicted)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$count), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = .data$count), vjust = 0, size = 6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = 2, size = 6) +
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
    ggplot2::annotate("text", x = 2, y = 2.55, vjust = 0, size = 5, label = sens_label, fontface = "italic") +
    ggplot2::annotate("text", x = 1, y = 2.55, vjust = 0, size = 5, label = spec_label, fontface = "italic") +
    ggplot2::annotate("text", x = 2.55, y = 2, hjust = 0, vjust = 1, size = 5, label = ppv_label, fontface = "italic") +
    ggplot2::annotate("text", x = 2.55, y = 1, hjust = 0, vjust = 1, size = 5, label = npv_label, fontface = "italic") +
    ggplot2::coord_cartesian(clip = "off", ylim = c(.8, 2.5), xlim = c(.8, 3)) +
    ggplot2::labs(x = "\nTruth", y = "Predicted\n")
}

#' Plot ROC Curve
#'
#' @description
#' Generates and plots the Receiver Operating Characteristic (ROC) curve for the binary classification model
#' represented by the given dx object. The ROC curve is a graphical representation of the trade-off between
#' the true positive rate (sensitivity) and the false positive rate (1 - specificity) at various threshold settings.
#'
#' @param dx_obj An object of class `dx` containing the necessary data and statistics for generating the ROC curve.
#' @param curve_color Color of the ROC curve. Default is "#0057B8".
#' @param fill_color Color filled under the ROC curve. Use "transparent" for no fill. Default is "#cfcdcb".
#' @param text_color Color of the text included on the ROC curve. Default is "black".
#' @param add_text Logical; if TRUE, includes statistical annotations on the ROC curve. Default is TRUE.
#' @param add_ref_lines Logical; if TRUE, includes reference lines on the ROC curve. Default is TRUE.
#' @param add_fractions Logical; if TRUE, includes fraction details in text annotations. Default is TRUE.
#' @param axis_color Color of the x and y axis. Default is "#333333".
#' @param add_ref_circle Logical; if TRUE, includes a reference circle around the point of specified threshold. Default is TRUE.
#' @param ref_lines_color Color for reference lines. Default is "#8a8887".
#' @param circle_ref_color Color of the reference circle. Default is "#E4002B".
#' @param summary_stats A vector of integers indicating which statistics to include on the ROC curve. Default is c(1, 2, 3, 4, 5, 6, 7, 8).
#' @param filename File name to create on disk using `ggplot2::ggsave`. If left NA, no file will be created.
#'
#' @return A ggplot object representing the ROC curve, allowing for further customization if desired.
#'
#' @details
#' The ROC curve is a widely used tool for diagnosing the performance of binary classification models. It plots the true positive
#' rate (sensitivity) against the false positive rate (1 - specificity) for various thresholds. A model with perfect discrimination
#' (no overlap in the two distributions) has an ROC curve that passes through the upper left corner (100% sensitivity, 100% specificity).
#' Therefore the closer the ROC curve is to the upper left corner, the higher the overall accuracy of the test.
#'
#' The `dx_roc` function allows for extensive customization of the ROC curve, including color schemes, reference lines, text annotations,
#' and more, to accommodate a variety of visualization needs and preferences.
#'
#' The area under the ROC curve (AUC) provides a single scalar value summarizing the performance of the test. The AUC can be interpreted as
#' the probability that a classifier will rank a randomly chosen positive instance higher than a randomly chosen negative instance.
#'
#' The function also provides options to include a reference circle at a specific threshold point, reference lines indicating a 45-degree
#' line (chance line) and lines for the chosen threshold's specificity and sensitivity, and fill color under the curve.
#'
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_plot_roc(dx_obj)
dx_plot_roc <- function(dx_obj, curve_color = "#0057B8", fill_color = "#cfcdcb",
                        text_color = "black", add_text = TRUE, add_ref_lines = TRUE,
                        add_fractions = TRUE, axis_color = "#333333",
                        add_ref_circle = TRUE, ref_lines_color = "#8a8887",
                        circle_ref_color = "#E4002B",
                        summary_stats = c(1, 2, 3, 4, 5, 6, 7, 8), filename = NA) {
  measures <- as.data.frame(dx_obj, variable = "Overall", thresh = dx_obj$options$setthreshold)

  sensdf <- measures[measures$measure == "Sensitivity", ]$estimate

  specdf <- measures[measures$measure == "Specificity", ]$estimate

  auc_df <- data.frame(
    threshold = dx_obj$thresholds$threshold,
    specificity = dx_obj$thresholds$specificity,
    sensitivity = dx_obj$thresholds$sensitivity
  )

  auc_df$lead_specificity <- c(auc_df$specificity[-1], NA)

  p <- ggplot2::ggplot(auc_df) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$specificity,
        xmax = .data$lead_specificity,
        ymin = 0,
        ymax = .data$sensitivity
      ),
      fill = fill_color,
      colour = NA,
      alpha = 0.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(.data$specificity, .data$sensitivity),
      color = curve_color,
      linewidth = 1
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 1,
      colour = axis_color
    ) +
    ggplot2::geom_vline(
      xintercept = 1.05,
      linewidth = 1,
      colour = axis_color
    )


  if (add_ref_lines) {
    p <- p +
      ggplot2::geom_segment(
        ggplot2::aes(x = 1, y = 0, xend = 0, yend = 1),
        linewidth = .2,
        linetype = "dotted",
        color = ref_lines_color
      ) +
      # horizontal line
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = 1.05, y = sensdf, yend = sensdf),
        linewidth = .2,
        linetype = "dashed",
        color = ref_lines_color
      ) +
      # vertical line
      ggplot2::geom_segment(
        ggplot2::aes(x = specdf, xend = specdf, y = 0, yend = 1.05),
        linewidth = .2,
        linetype = "dashed",
        color = ref_lines_color
      )
  }

  if (add_ref_circle) {
    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = specdf, y = sensdf),
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

    diagsummary_resultsdf <- diagsummary_resultsdf[, -which(names(diagsummary_resultsdf) %in% c("variable", "label"))]

    diagsummary_resultsdf <- diagsummary_resultsdf[seq_len(nrow(diagsummary_resultsdf)) %in% summary_stats, ]

    diagsummary_resultsdf$numden <- ifelse(
      !add_fractions | nchar(diagsummary_resultsdf$fraction) == 0,
      "",
      paste0(" (", diagsummary_resultsdf$fraction, ")")
    )

    diagsummary_resultsdf$y <- location_vector

    diagsummary_resultsdf$label <- paste0(diagsummary_resultsdf$measure, ":  ", diagsummary_resultsdf$summary, diagsummary_resultsdf$numden)


    p <- p +
      ggplot2::geom_text(
        data = diagsummary_resultsdf,
        mapping = ggplot2::aes(x = .05, y = .data$y, label = .data$label, hjust = 1),
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

#' Plot Predicted Probabilities
#'
#' @description
#' Generates a plot showing the distribution of predicted probabilities.
#' Offers options to show as a histogram or density plot.
#'
#' @param dx_obj A `dx` object
#' @param plot_type Character string specifying the type of plot: "histogram" or "density".
#' @param bins Optional; number of bins for the histogram (relevant if plot_type = "histogram").
#' @param fill_color Color to fill the bars or density plot.
#' @return A ggplot object representing the distribution of predicted probabilities.
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_probabilities(dx_obj, plot_type = "histogram", bins = 30)
#' dx_plot_probabilities(dx_obj, plot_type = "density")
#'
dx_plot_probabilities <- function(dx_obj, plot_type = "histogram", bins = NULL, fill_color = "#0057B8") {
  density <- NULL # Dummy definition to avoid R CMD check warnings

  # Prepare data for plotting
  data <- data.frame(Predictions = dx_obj$data[[dx_obj$options$pred_varname]])

  # Create the plot based on the specified type
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$Predictions, y = ggplot2::after_stat(density)))

  if (plot_type == "histogram") {
    p <- p + ggplot2::geom_histogram(bins = bins, fill = fill_color)
  } else if (plot_type == "density") {
    p <- p + ggplot2::geom_density(fill = fill_color, alpha = 0.7)
  }

  p <- p + ggplot2::labs(
    x = "Predicted Probability",
    y = "Density"
  ) +
    ggplot2::theme_minimal()

  return(p)
}


#' Plot Diagnostic Measures across Thresholds
#'
#' Generates a line plot to visualize various diagnostic measures across different
#' threshold values for a binary classification model. This visualization can help in
#' selecting an optimal threshold based on the trade-offs between different measures.
#'
#' @param dx_obj A `dx` object containing threshold-based statistics, including values
#'               for various diagnostic measures at different thresholds.
#'
#' @return A `ggplot` object representing the diagnostic measures across thresholds
#'         with the ability to further customize if desired.
#'
#' @export
#'
#' @details
#' The function plots multiple lines representing different diagnostic measures such
#' as NPV, PPV, sensitivity, specificity, and F1 score across a range of threshold
#' values. Each line corresponds to a specific metric, illustrating how the measure
#' changes as the classification threshold is varied. A vertical dashed line indicates
#' the set threshold in the `dx` object for reference.
#'
#' This plot is particularly useful for understanding the behavior of a classifier
#' under different operating conditions and for identifying a threshold that balances
#' the trade-offs between various measures according to the specific needs of the
#' application.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   setthreshold = .3
#' )
#' dx_plot_thresholds(dx_obj)
dx_plot_thresholds <- function(dx_obj) {
  thresholds <- dx_obj$thresholds

  measures <- c(
    "npv", "ppv", "sensitivity", "specificity", "f1"
  )

  data_list <- lapply(measures, function(m) {
    data.frame(
      threshold = thresholds$threshold,
      metric = m,
      measure = thresholds[[m]]
    )
  })

  res <- do.call(rbind, data_list)
  res <- stats::na.omit(res)


  ggplot2::ggplot(res) +
    ggplot2::geom_vline(
      xintercept = dx_obj$options$setthreshold,
      linetype = "dashed",
      color = "gray"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data$threshold, y = .data$measure, color = .data$metric),
      linewidth = 1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Threshold", y = "Measure", color = "Metric")
}

pluck_auc_roc_data <- function(dx_obj) {
  measures <- as.data.frame(dx_obj, variable = "Overall", thresh = dx_obj$options$setthreshold)

  sensdf <- measures[measures$measure == "Sensitivity", ]$estimate

  specdf <- measures[measures$measure == "Specificity", ]$estimate

  auc_df <- data.frame(
    threshold = dx_obj$thresholds$threshold,
    specificity = dx_obj$thresholds$specificity,
    sensitivity = dx_obj$thresholds$sensitivity
  )

  auc_df$lead_specificity <- c(auc_df$specificity[-1], NA)

  list(
    sensitivity = sensdf,
    specificity = specdf,
    auc_df = auc_df
  )
}


#' Plot ROC Curves for Multiple Models
#'
#' Generates Receiver Operating Characteristic (ROC) curves for multiple models
#' and overlays them for comparison. Optionally, it adds text annotations for DeLong's
#' test results to indicate statistical differences between the models' Area Under
#' the ROC Curve (AUC).
#'
#' @param dx_comp A `dx_compare` object containing the results of pairwise model
#'                comparisons and the list of `dx` objects with ROC data.
#' @param add_text Logical, whether to add DeLong's test results as text annotations
#'                on the plot. Defaults to TRUE.
#' @param axis_color Color of the axes lines, specified as a color name or hex code.
#'                   Defaults to "#333333".
#' @param text_color Color of the text annotations, specified as a color name or hex code.
#'                   Defaults to "black".
#'
#' @return A ggplot object representing the ROC curves for the models included in the
#'         `dx_comp` object. Each model's ROC curve is color-coded, and the plot
#'         includes annotations for DeLong's test p-values if `add_text` is TRUE.
#'
#' @details This function is a visualization tool that plots ROC curves for multiple
#'          models to facilitate comparison. It uses DeLong's test to statistically
#'          compare AUC values and, if desired, annotates the plot with the results.
#'          The function expects a `dx_compare` object as input, which should contain
#'          the necessary ROC and test comparison data. Ensure that the ROC data and
#'          DeLong's test results are appropriately generated and stored in the
#'          `dx_compare` object before using this function.
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' dx_glm <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted")
#' dx_rf <- dx(data = dx_heart_failure, true_varname = "truth", pred_varname = "predicted_rf")
#' dx_list <- list(dx_glm, dx_rf)
#' dx_comp <- dx_compare(dx_list, paired = TRUE)
#' dx_plot_rocs(dx_comp)
#' @seealso \code{\link{dx_compare}} to generate the required input object.
#'          \code{\link{dx_delong}} for details on DeLong's test used in comparisons.
#' @export
dx_plot_rocs <- function(dx_comp, add_text = TRUE, axis_color = "#333333", text_color = "black") {
  roc_data <- lapply(dx_comp$dx_list, pluck_auc_roc_data)
  delong <- dx_comp$tests
  delong <- delong[delong$test == "DeLong's test for ROC curves", ]

  for (i in seq_along(roc_data)) {
    roc_data[[i]]$auc_df$model <- names(roc_data)[i]
  }

  df <- do.call(rbind, lapply(roc_data, function(model) model$auc_df))

  df <- stats::na.omit(df)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(
      ggplot2::aes(.data$specificity, .data$sensitivity, color = .data$model),
      linewidth = 1
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 1,
      colour = axis_color
    ) +
    ggplot2::geom_vline(
      xintercept = 1.05,
      linewidth = 1,
      colour = axis_color
    ) +
    ggplot2::coord_fixed() +
    dx_roc_ggtheme() +
    ggplot2::labs(
      x = "\nSpecificity",
      y = "Sensitivity\n\n",
      color = "Model"
    ) +
    ggplot2::theme(legend.position = "bottom")


  if (add_text) {
    numsummary <- nrow(delong)
    ystart <- 0.05
    yend <- ystart + (numsummary - 1) * 0.05
    location_vector <- seq(yend, ystart, -0.05)



    text_df <- data.frame(
      y = location_vector,
      label = paste0(delong$models, ": ", sapply(delong$p_value, format_pvalue))
    )

    p <- p +
      ggplot2::geom_text(
        data = text_df,
        mapping = ggplot2::aes(x = .05, y = .data$y, label = .data$label, hjust = 1),
        color = text_color
      )
  }

  return(p)
}
