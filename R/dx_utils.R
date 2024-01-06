#' Summary
#' @param object An object of class "dx"
#' @param thresh The threshold to return values from
#' @param variable Variable to include in returned values
#' @param label Labels to include in returned values
#' @param show_var Include variable column in returned data?
#' @param show_label Include label in returned data?
#' @param measure Measures to include
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

  tmp <- tmp[, c("variable", "label", "measure", "summary")]

  rownames(tmp) <- NULL

  # Dropping columns 'rawestime', 'rawlci', 'rawuci'
  # tmp <- tmp[, !names(tmp) %in% c('rawestime', 'rawlci', 'rawuci', 'ci_type', 'notes', 'n')]

  if (!show_var) tmp <- subset(tmp, select = -c(variable))
  if (!show_label) tmp <- subset(tmp, select = -c(label))

  # caption <- paste0("N=", comma(tmp$n[1]), "; ", "Threshold=", thresh)
  caption <- paste("Threshold=", thresh)

  # tmp <- subset(tmp, select = -c(threshold))
  # tmp <- tmp[, -which(names(tmp) %in% c("threshold"))]



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
#' @param label Labels to include in returned values
#' @param measure Measures to include
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

comma <- function(x) {
  y <- prettyNum(x, big.mark = ",")
  y[y == "NA"] <- ""
  return(y)
}

format_pvalue <- function(p, accuracy = 0.01) {
  # Define the smallest non-zero number representable with the given accuracy
  smallest_nonzero <- accuracy

  if (p < smallest_nonzero) {
    # Construct the smallest non-zero representation based on accuracy
    smallest_nonzero_str <- formatC(accuracy, format = "f", digits = -log10(accuracy))
    return(paste0("p<", smallest_nonzero_str))
  } else {
    # Round and format the p-value
    rounded_p <- round(p, -log10(accuracy))
    return(paste("p=", formatC(rounded_p, format = "f", digits = -log10(accuracy)), sep = ""))
  }
}

conf_int <- function(est, lower, upper, accuracy = .1, percent = FALSE) {
  format_num <- function(num) {

    if (is.na(num)) {
      "-"
    } else if (percent) {
      paste0(formatC(num * 100, format = "f", digits = -log10(accuracy)), "%")
    } else {
      formatC(num, format = "f", digits = -log10(accuracy), big.mark = ",")
    }
  }

  paste0(format_num(est), " (", format_num(lower), ", ", format_num(upper), ")")
}

check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))  {
    stop(paste0(pkg, " must be must be installed to use this function"))
  }
}





dx_breslow_day <- function(data, options, group_varname) {

  predprob = data[[options$pred_varname]]
  pred = ifelse(predprob >= options$setthreshold, 1, 0)

  tmp <- data.frame(
    predprob = predprob,
    truth = data[[options$true_varname]],
    pred = pred,
    group = data[[group_varname]]
  )

  tmp <- tmp[!is.na(tmp$group), ]

  tab <- table(tmp$pred, tmp$truth, tmp$group)

  bd <- breslow_day_test(tab, odds_ratio = NA, correct = FALSE)

  measure <- measure_df(
    measure = "Breslow-Day",
    estimate = format_pvalue(bd$p_value[[1]], accuracy = 0.01),
    fraction = "",
    ci_type = "",
    notes = "Mantel-Haenszel OR estimate",
    estimate_raw = bd$p_value,
    lci_raw = NA,
    uci_raw = NA
  )

  measure$n <- nrow(tmp)

  measure$variable <- group_varname
  measure$label <- "Overall"
  measure$threshold <- options$setthreshold


  return(measure)


}

recreate_data_from_cm <- function(cm) {
  # Create synthetic truth vector based on true positive (tp), false negative (fn),
  # true negative (tn), and false positive (fp) counts
  truth <- c(rep(1, cm$tp + cm$fn), rep(0, cm$tn + cm$fp))

  # Create synthetic predicted probabilities vector
  # Assuming binary classification, predicted probabilities are either 1 (for positive class) or 0 (for negative class)
  predprob <- c(rep(1, cm$tp), rep(0, cm$fn), rep(1, cm$fp), rep(0, cm$tn))

  return(list(truth = truth, predprob = predprob))
}

boot_metric <- function(truth, predprob, metric_func, metric_args, bootreps, measure_name) {
  boot_metrics <- numeric(bootreps)
  for (i in 1:bootreps) {
    indices <- sample(seq_along(truth), length(truth), replace = TRUE)
    truth_boot <- truth[indices]
    predprob_boot <- predprob[indices]

    cm_boot <- dx_cm(predprob_boot, truth_boot, threshold = 0.5, poslabel = 1)
    boot_metrics[i] <- do.call(metric_func, c(list(cm = cm_boot), metric_args))
  }

  # Example of checking proportion of NAs
  na_proportion <- sum(is.na(boot_metrics)) / length(boot_metrics)
  if(na_proportion > 0.05) {  # or some other threshold
    warning(paste0("High proportion of NAs in bootstrapped samples for ", measure_name, " (", round(na_proportion, digits = 2), ")"))
  }


  # Calculate bootstrapped confidence intervals
  metric_ci <- stats::quantile(boot_metrics, probs = c(0.025, 0.975), na.rm = TRUE)
  return(list(lower = metric_ci[[1]], upper = metric_ci[[2]]))
}

evaluate_metric <- function(cm, metric_func, measure_name, detail, boot, bootreps, ...) {
  validate_detail(detail)
  # Calculate the metric using the provided function
  metric_raw <- metric_func(cm, ...)

  if (detail == "simple") {
    return(metric_raw)
  } else if (detail == "full") {
    if (boot) {
      data_recreated <- recreate_data_from_cm(cm)

      # Perform bootstrapping
      ci_bounds <- boot_metric(
        truth = data_recreated$truth,
        predprob = data_recreated$predprob,
        metric_func,  # Metric calculation function
        list(...),  # Pass additional args if needed
        bootreps,
        measure_name = measure_name
      )
      ci_lower <- ci_bounds$lower
      ci_upper <- ci_bounds$upper

      # Build and return the result with confidence intervals
      return(measure_df(
        measure = measure_name,
        estimate = conf_int(metric_raw, ci_lower, ci_upper, percent = TRUE),
        estimate_raw = metric_raw,
        ci_type = "Bootstrapped",
        lci_raw = ci_lower,
        uci_raw = ci_upper
      ))
    } else {
      # Return results without bootstrapping
      return(measure_df(
        measure = measure_name,
        estimate = format(metric_raw, digits = 2),
        estimate_raw = metric_raw,
        ci_type = NA_character_,
        lci_raw = NA_real_,
        uci_raw = NA_real_,
        notes = "Specify `boot = TRUE` for CIs"
      ))
    }
  } else {
    stop("Invalid detail parameter: should be 'simple' or 'full'")
  }
}


return_df <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE))  {
    return(tibble::as_tibble(x))
  } else {
    return(x)
  }
}

validate_dx_list <- function(dx_list) {
  if (!is.list(dx_list)) {
    stop("dx_list must be a list of `dx` objects")
  }
  if (!length(dx_list) >= 2) {
    stop("dx_list must contain two or more `dx_objects")
  }
  for (x in dx_list) {
    if (! "dx" %in% class(x)) {
      stop("All elements in `dx_list` must be `dx` objects")
    }
  }
  if (is.null(names(dx_list))) {
    names(dx_list) <- paste("Model", seq_along(dx_list))
  }
  for (i in seq_along(dx_list)) {
    if (names(dx_list)[i] == "") {
      names(dx_list)[1] <- paste("Model", i)
    }
  }
  return(dx_list)
}


pluck_probabilities <- function(dx) {
  dx$data[[dx$options$pred_varname]]
}

pluck_predicted <- function(dx) {
  as.numeric(pluck_probabilities(dx) >= dx$options$setthreshold)
}

pluck_truths <- function(dx) {
  dx$data[[dx$options$true_varname]]
}

two_model_name <- function(x, y) {
  paste(x, "vs.", y)
}


#' Create a Data Frame for Metric Measures
#'
#' This internal function creates a data frame for storing metric measures, including
#' the measure name, its estimate, confidence interval type, and any additional notes.
#'
#' @param measure The name of the measure.
#' @param estimate The formatted estimate of the measure.
#' @param fraction The fraction representing the measure (if applicable).
#' @param ci_type The type of confidence interval used.
#' @param notes Additional notes or interpretation about the measure.
#' @param estimate_raw The raw estimate value.
#' @param lci_raw The lower limit of the confidence interval (raw).
#' @param uci_raw The upper limit of the confidence interval (raw).
#' @return A data frame with the specified measure details.
#' @noRd
measure_df <- function(measure = "", estimate = "", fraction = "",
                       ci_type = "", notes = "", estimate_raw = NA,
                       lci_raw = NA, uci_raw = NA) {

  metric <- data.frame(
    measure = measure,
    summary = estimate,
    estimate = estimate_raw,
    conf_low = lci_raw,
    conf_high = uci_raw,
    fraction = fraction,
    conf_type = ci_type,
    notes = notes,
    stringsAsFactors = FALSE
  )

  return_df(metric)

}

compare_df <- function(models = "",
                       test = "",
                       summary = "",
                       p_value = "",
                       estimate = "",
                       conf_low = NA,
                       conf_high = NA,
                       statistic = "",
                       notes = "") {

  metric <- data.frame(
    models = models,
    test = test,
    summary = summary,
    p_value = p_value,
    estimate = estimate,
    conf_low = conf_low,
    conf_high = conf_high,
    statistic = statistic,
    notes = notes,
    stringsAsFactors = FALSE
  )

  return_df(metric)

}

validate_detail <- function(detail) {
  check <- match.arg(detail, choices = c("full", "simple"))
}


