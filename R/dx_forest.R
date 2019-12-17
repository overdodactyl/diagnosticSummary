#' Create table with odds ratios displayed graphically
#'
#' Generate a table of diagnostic measures
#'
#' @param dx_obj An object of class dx
#' @param fraction Logical.  Should the fraction for sensitivity and specificity be shown?
#' @param breaks A numeric vector of breaks to include on axis ticks.  If left empty, breaks will be determined using the min and max value from 95\% CIs.
#' @param limits Limits for axis ticks.  Ticks will be generates using base::breaks.  Ignored if breaks are passed.
#' @param tick_label_size Font size for axis labels.
#' @param return_grid Should a grid object be returned? If FALSE, grid is drawn using grid.draw.
#' @param trans Method to transform the odds ratio by.  Currently, only log10 is supported.
#' @param filename File bane to create on disk.  If left NA, no file will be created.
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grobTree unit gpar editGrob segmentsGrob pointsGrob textGrob
#' @export
#' @examples
#'
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   study_name = "Heart Attack Prediction",
#'   data_description = "Validation Data",
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1,.2,.3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_forest(dx_obj)
#' dx_forest(dx_obj, trans = "log10")

dx_forest <- function(dx_obj, fraction = FALSE, breaks = NA, limits = NA, tick_label_size = 6.5, trans = c(NA, "log10"), return_grid = FALSE, filename = NA) {

  trans = match.arg(trans)

  data <- dx_prep_forest2(dx_obj, fraction = fraction)

  indent_rows <- which(!is.na(data$rawestime))
  bold_rows <- setdiff(1:(nrow(data)),indent_rows)
  indent_rows <- indent_rows[indent_rows != nrow(data)]

  overall_or <- data %>% dplyr::filter(Group == "Overall") %>% dplyr::pull(rawestime)

  lower <- data$rawlci
  estimate <- data$rawestime
  upper <- data$rawuci

  # Find range of OR's
  min_or <- min(data$rawlci, na.rm = T)
  max_or <- max(data$rawuci, na.rm = T)

  # Get plot range, breaks, and labels
  range <- plot_range(limits = limits, breaks = breaks, trans = trans, min_or = min_or, max_or = max_or)
  breaks <- plot_breaks(range = range, breaks = breaks, trans = trans)
  labels <- plot_labels(breaks = breaks, trans = trans)

  ### Transform data
  if (!is.na(trans) & trans == "log10") {
    lower <- log10(lower)
    estimate <- log10(estimate)
    upper <- log10(upper)
    overall_or <- log10(overall_or)
    range = log10(range)
  }

  # Rescale data between 0 and 1
  breaks_scaled <- scales::rescale(breaks)
  lower <- scales::rescale(lower, from = range)
  estimate <- scales::rescale(estimate, from = range)
  upper <- scales::rescale(upper, from = range)
  overall_or <- scales::rescale(overall_or, from = range)

  or_data <- data %>% dplyr::select(dplyr::starts_with("raw"))

  tbl_data <- data %>%
    dplyr::mutate(` ` = "                                          ") %>%
    dplyr::select(Group, AUC, Sensitivity, Specificity, ` `, `Odds Ratio`)

  tbl_data <- tbl_data %>% dplyr::add_row()

  # tbl_data <- tbl_data %>% dplyr::mutate_all(tidyr::replace_na, replace = "")
  tbl_data[is.na(tbl_data)] <- ""

  tbl_data <- tbl_data %>%
    dplyr::mutate(Group = ifelse(dplyr::row_number() %in% indent_rows, paste("   ", Group), Group))

  table_theme <- gridExtra::ttheme_minimal(
    core=list(margin=unit(c(1, 1), "mm"),
              bg_params = list(fill = rep(c("#e6e4e2", "#ffffff")), col=NA),
              fg_params=list(fontface=1, cex = .6)),
    colhead=list(fg_params=list(col="black", fontface=1, cex = .7)))

  or_col <- 5
  nrows <- nrow(tbl_data)
  ncols <- ncol(tbl_data)

  # Convert df to grob
  g <- gridExtra::tableGrob(tbl_data, theme=table_theme, rows = NULL, widths = unit(c(rep(5, 6)), c("cm")))

  # Add border under header
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = 1, l = 1, r = ncols, name = "header_border")

  # Add border under table
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = nrows, l = 1, r = ncols, name = "footer_border_top")
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = nrows-1, l = 1, r = ncols, name = "footer_border_bottom")

  # Add borders to OR column
  g <- dx_vline(g, x = 0, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col, name = "left_or_border")
  g <- dx_vline(g, x = 1, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col, name = "right_or_border")

  # Add dashed line for overall OR
  g <- dx_vline(g, x = overall_or, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col, name = "overall_or", gp = gpar(lwd = .8, lty = 2))

  # Add OR's
  for (i in seq_along(estimate)) {
    g <- dx_forest_add_or(g, i+1, lower[i], estimate[i], upper[i], or_col = or_col)
  }

  # Add ticks and lables
  for (i in seq_along(breaks)) {
    g <- dx_forest_add_tick(g, breaks_scaled[i], labels[i], nrows = nrows, tick_label_size = tick_label_size)
  }

  # Bold bottom row
  g <- dx_edit_cell(g, nrow(g)-1, seq_len(ncol(g)), "core-fg", gp=gpar(fontface="bold"))

  # Bold levels
  g <- dx_edit_cell(g, bold_rows + 1, 1, "core-fg", gp=gpar(fontface="bold"))

  # Left align first column
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "core-fg", x=unit(.05, "npc"), hjust=0)
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "colhead-fg", x=unit(.05, "npc"), hjust=0)

  # Darken total row
  g <- dx_edit_cell(g, nrow(g)-1, seq_len(ncol(g)), "core-bg", gp=gpar(fill="#b8b6b4"))

  # Last row should be white (ticks and lables)
  g <- dx_edit_cell(g, nrow(g), seq_len(ncol(g)), "core-bg", gp=gpar(fill="#ffffff"))

  # Adjust width of plot - some fine tunining here in the future woud be nice
  g$widths <- unit(rep(1/ncol(g), ncol(g)), "npc")

  if (!is.na(filename)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      g2 <- g
      g2$widths <- grid::unit(rep(1/(ncol(g2)+1), ncol(g2)), "npc")
      ggplot2::ggsave(g2, width = 8.5, height = 11, dpi = 600, filename = filename)
    } else {
      warning("ggplot2 needs to be installed to save a plot. No file was generated.")
    }

  }

  if(return_grid) {
    return(g)
  } else {
    grid::grid.draw(g)
  }

}


dx_hline <- function(table, y, x0, x1, t, b = t, l, r = l, name, gp = gpar(lwd = .8), clip = "off") {
  gtable_add_grob(table,
                  grobs = grobTree(
                    segmentsGrob(
                      x0 = unit(x0,"npc"),
                      y0 = unit(y,"npc"),
                      x1 = unit(x1,"npc"),
                      y1 = unit(y,"npc"),
                      gp = gp)),
                  t = t, b = b, l = l, r = r,
                  name = name,
                  z = Inf,
                  clip = clip)
}

dx_vline <- function(table, x, y0, y1, t, b = t, l, r = l, name, gp = gpar(lwd = .8), clip = "off") {
  gtable_add_grob(table,
                  grobs = grobTree(
                    segmentsGrob(
                      x0 = unit(x,"npc"),
                      y0 = unit(y0,"npc"),
                      x1 = unit(x,"npc"),
                      y1 = unit(y1,"npc"),
                      gp = gp)),
                  t = t, b = b, l = l, r = r,
                  name = name,
                  z = Inf,
                  clip = clip)
}

#' Edit a cell within an object returned from dx_forest
#'
#' A convenient wrapper used to edit cells of a gtable.
#'
#' @param table a table returned from dx_forest
#' @param row Numeric vector of rows to edit
#' @param col Numeric vector of columns to edit
#' @param name Name of table layer to edit
#' @param ... Parameters passed to editGrob such as gpar or hjust.
#' @export
dx_edit_cell <- function(table, row, col, name="core-fg", ...){
  l <- table$layout
  ids <- which(l$t %in% row & l$l %in% col & l$name==name)
  for (id in ids){
    newgrob <- editGrob(table$grobs[id][[1]], ...)
    table$grobs[id][[1]] <- newgrob
  }
  table
}


dx_forest_add_or <- function(grob, row, low, est, high, or_col = 4) {
  i <- sample(1:100000,1)

  tmp <- dx_hline(grob, y = .5, x0 = low, x1 = high, t = row, l = or_col, name = paste0("or", i), clip = "on")
  tmp <- dx_vline(tmp, x = low, y0 = .35, y1 = .65, t = row, l = or_col, name = paste0("left_or_cap_", i), clip = "on")
  tmp <- dx_vline(tmp, x = high, y0 = .35, y1 = .65, t = row, l = or_col, name = paste0("right_or_cap_", i), clip = "on")

  gtable_add_grob(tmp,
                  grobs = grobTree(
                    pointsGrob(
                      x = est,
                      y = .5,
                      pch = 16,
                      size = unit(.35, "char"))),
                  t = row, l = or_col, name = "point1", z = Inf)
}


dx_forest_add_tick <- function(grob, tick_scaled, tick, nrows, or_col = 4, tick_label_size) {

  tmp <- dx_vline(grob, x = tick_scaled, y0 = .8, y1 = 1, t = nrows+1, l = or_col, name = paste0("tick_", tick))

  gtable_add_grob(tmp,
                  grobs = grobTree(
                    textGrob(
                      label = tick,
                      x = tick_scaled,
                      y = .5,
                      gp = gpar(fontsize=tick_label_size))),
                  t = nrows+1, l = or_col, name = paste0("tick_label_", tick),
                  z = Inf,
                  clip = "off")
}

dx_prep_variable <- function(dx_obj, data, fraction = FALSE) {
  var <- data$Variable[[1]]
  orig_var <- data$original_variable[[1]]
  tmp <- data %>% dplyr::filter(Measure %in% c("AUC", "Sensitivity", "Specificity", "Odds Ratio"))

  if (fraction) {
    tmp$Estimate <- ifelse(tmp$Fraction == "", tmp$Estimate, paste0(tmp$Estimate, " (",tmp$Fraction, ")"))
  }

  res_sel <- tmp %>% dplyr::select(Group = Label, Measure, Estimate)
  rawdata <- tmp %>% dplyr::select(Group = Label, dplyr::starts_with("raw")) %>% dplyr::filter(!is.na(rawestime))
  res <- utils::unstack(res_sel, form = Estimate ~ Measure)
  names(res) <- gsub("\\.", " ", names(res) )
  if (var == "Overall") res <- as.data.frame(t(res))
  res$Group <- unique(res_sel$Group)
  res <- dplyr::left_join(res, rawdata, by = "Group")
  if (var != "Overall") {
    res$Group <- factor(res$Group, levels = levels(dx_obj$data[[orig_var]]))
    res <- dplyr::arrange(res, Group)
    res$Group <- as.character(res$Group)
    empty_df <- data.frame(Group = var, stringsAsFactors = FALSE)
    res <- dplyr::bind_rows(empty_df, res)
  }
  res %>% dplyr::mutate_if(is.factor, as.character)
}

label_df <- function(data) {
  x <- lapply(data, attr, which = "label", exact = TRUE)
  x <- lapply(x, function(x) ifelse(is.null(x), NA, x))
  data.frame(Variable = names(x),
             VariableLabel = as.character(unlist(x,use.names=F)),
             stringsAsFactors = FALSE)
}

dx_prep_forest2 <- function(dx_obj, fraction = fraction) {

  tmp <- dx_obj$measures %>% dplyr::filter(threshold == dx_obj$options$setthreshold)

  labs <- label_df(data = dx_obj$data)

  tmp <- dplyr::left_join(tmp, labs, by = "Variable")

  tmp <- dplyr::mutate(tmp,
                       original_variable = Variable,
                       Variable = dplyr::coalesce(VariableLabel, Variable))

  tmp_split <- tmp %>% dplyr::group_by(Variable) %>% dplyr::group_split()

  for (i in seq_along(tmp_split)) {
    tmp_split[[i]] <- dx_prep_variable(dx_obj, tmp_split[[i]], fraction = fraction)
  }

  tmp <- do.call("rbind", tmp_split)

  subgroups <- tmp %>% dplyr::filter(Group != "Overall")
  overall <- tmp %>% dplyr::filter(Group == "Overall")

  rbind(subgroups, overall)

}


### Find limits of plot
# Limits: use these
# Breaks: use min(breaks) - max(breaks)
# No trans: use pretty()
# Trans: find min/max pos that contains
plot_range <- function(limits = NA, breaks = NA, trans = NA, min_or = NA, max_or = NA) {
  if (!identical(limits, NA)) {
    res <- limits
  } else if (!identical(breaks, NA)) {
    res <- c(min(breaks), max(breaks))
  } else {
    if (is.na(trans)) {
      pretty <- pretty(c(min_or, max_or), n = 6)
      res <- c(min(pretty), max(pretty))
    } else {
      high <- ceiling(log10(max_or))
      low <- max(c(0, floor(log10(min_or))))
      res <- 10^(c(low, high))
    }
  }
  res
}


### Find breaks
# Breaks: use these
# No trans: use pretty()
# Trans: use 0:pos within limits
plot_breaks <- function(range, breaks = NA, trans = NA) {
  if (!identical(breaks, NA)) {
    res <- breaks
  } else if (is.na(trans)) {
    res <- pretty(c(range[1], range[2]), n = 6)
  } else {
    first <- log10(range[1])
    last <- log10(range[2])
    res <- first:last
  }
  res
}


### Find labels
# No trans: breaks
# Trans: use 10^breaks
plot_labels <- function(breaks, trans) {
  if (is.na(trans)) {
    breaks
  } else {
    10^breaks
  }
}
