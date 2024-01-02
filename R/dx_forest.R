zero_range <- function (x, tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) {
    return(TRUE)
  }
  if (length(x) != 2)
    stop("x must be length 1 or 2")
  if (any(is.na(x))) {
    return(NA)
  }
  if (x[1] == x[2]) {
    return(TRUE)
  }
  if (all(is.infinite(x))) {
    return(FALSE)
  }
  m <- min(abs(x))
  if (m == 0) {
    return(FALSE)
  }
  abs((x[1] - x[2])/m) < tol
}

rescale <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  if (zero_range(from) || zero_range(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  (x - from[1])/diff(from) * diff(to) + to[1]
}

#' Create table with odds ratios displayed graphically
#'
#' Generate a table of diagnostic measures
#'
#' @param dx_obj An object of class dx
#' @param fraction Logical.  Should the fraction for sensitivity
#'     and specificity be shown?
#' @param breaks A numeric vector of breaks to include on axis ticks.
#'     If left empty, breaks will be determined using
#'     the min and max value from 95\% CIs.
#' @param limits Limits for axis ticks.
#'     Ticks will be generates using base::breaks.
#'     Ignored if breaks are passed.
#' @param tick_label_size Font size for axis labels.
#' @param return Should a grid or ggplot object be returned?
#' @param trans Method to transform the odds ratio by.
#'     Currently, only log10 is supported.
#' @param measures Measures to be included in the plot
#' @param filename File bane to create on disk.
#'     If left NA, no file will be created.
#' @param header_bg Background color of the header
#' @param header_col Color of text in the header
#' @param header_fontsize Font size of header text
#' @param body_bg Background color of table rows.  If values are less than total
#'     number of rows, values are repeated.
#' @param body_fontsize Font size of body text
#' @param footer_bg Background color if the footer row.
#' @param footer_col Color of the footer row.
#' @param body_or_col Color of odds ratios in the table body
#' @param footer_or_col Color of odds ratios in the table footer
#' @param fraction_multiline Logical. Should fractions be split onto 2 lines?
#' @param or_lwd Line width for OR
#' @param or_size Size of OR point
#' @export
#' @examples
#'
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1, .2, .3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' #dx_forest(dx_obj)
#' #dx_forest(dx_obj, trans = "log10")
dx_forest <- function(dx_obj, fraction = FALSE, breaks = NA, limits = NA,
                      tick_label_size = 6.5, trans = c(NA, "log10"),
                      measures = c("AUC", "Sensitivity", "Specificity","Odds Ratio"),
                      return = c("ggplot", "grid"),
                      filename = NA,
                      header_bg = "white", header_col = "black",
                      body_bg = c("#e6e4e2", "#ffffff"),
                      footer_bg = "#b8b6b4", footer_col = "black",
                      header_fontsize = 10, body_fontsize = 9,
                      fraction_multiline = FALSE,
                      or_lwd = .8, or_size = .35,
                      body_or_col = "black", footer_or_col = footer_col) {

  check_package("gridExtra")
  check_package("grid")
  check_package("gtable")


  trans <- match.arg(trans)
  return_type <- match.arg(return)

  stopifnot("Odds Ratio" %in% measures)

  data <- dx_prep_forest(
    dx_obj,
    fraction = fraction,
    fraction_multiline = fraction_multiline,
    measures = measures
  )

  indent_rows <- which(!is.na(data$rawestime))
  bold_rows <- setdiff(1:(nrow(data)), indent_rows)
  indent_rows <- indent_rows[indent_rows != nrow(data)]

  overall_or <- data[data$group == "Overall", ]$rawestime


  lower <- data$rawlci
  estimate <- data$rawestime
  upper <- data$rawuci

  # Find range of OR's
  min_or <- min(data$rawlci, na.rm = T)
  max_or <- max(data$rawuci, na.rm = T)

  # Get plot range, breaks, and labels
  range <- plot_range(
    limits = limits, breaks = breaks, trans = trans,
    min_or = min_or, max_or = max_or
  )
  breaks <- plot_breaks(range = range, breaks = breaks, trans = trans)
  labels <- plot_labels(breaks = breaks, trans = trans)

  ### Transform data
  if (!is.na(trans) & trans == "log10") {
    lower <- log10(lower)
    estimate <- log10(estimate)
    upper <- log10(upper)
    overall_or <- log10(overall_or)
    range <- log10(range)
  }

  # Rescale data between 0 and 1
  breaks_scaled <- rescale(breaks)
  lower <- rescale(lower, from = range)
  estimate <- rescale(estimate, from = range)
  upper <- rescale(upper, from = range)
  overall_or <- rescale(overall_or, from = range)


  select_measuers <- measures[!measures %in% c("Breslow-Day")]

  # Create a new column with spaces
  data$` ` <- "                                          "

  # Format the 'n' column
  data$n <- comma(data$n)

  # Select and rename the necessary columns. Ensure 'select_measures' is a character vector of column names
  cols_to_select <- c("group", "N", select_measuers, " ")
  names(data)[names(data) == "n"] <- "N"
  tbl_data <- data[cols_to_select]

  # Order the columns, moving the space column before 'Odds Ratio'
  # Assuming 'Odds Ratio' is one of the 'select_measures'
  odds_ratio_index <- which(names(tbl_data) == "Odds Ratio")
  space_index <- which(names(tbl_data) == " ")
  tbl_data[c("group", "N", select_measuers, " ")]


  order <- append(names(tbl_data), " ", after = odds_ratio_index-1)
  order <- order[1:length(order)-1]

  tbl_data <- tbl_data[order]
  rownames(tbl_data) <- NULL

  tbl_data <- rbind_all(tbl_data, NA)

  tbl_data[is.na(tbl_data)] <- ""

  # Create a sequence of row numbers
  row_nums <- seq_len(nrow(tbl_data))

  # Use ifelse to conditionally prepend spaces to the 'group' column
  tbl_data$group <- ifelse(row_nums %in% indent_rows, paste("   ", tbl_data$group), tbl_data$group)

  names(tbl_data)[names(tbl_data) == "group"] <- "Group"


  names(tbl_data) <- gsub("Positive Predictive Value", "PPV", names(tbl_data))
  names(tbl_data) <- gsub("Negative Predictive Value", "NPV", names(tbl_data))

  table_theme <- gridExtra::ttheme_minimal(
    core = list(
      margin = grid::unit(c(1, 1), "mm"),
      bg_params = list(fill = rep(body_bg), col = NA),
      fg_params = list(fontface = 1, fontsize = body_fontsize)
    ),
    colhead = list(
      fg_params = list(col = header_col, fontface = 1, fontsize = header_fontsize),
      bg_params = list(fill = header_bg, col = NA)
    )
  )

  or_col <- which(names(tbl_data) == " ")
  nrows <- nrow(tbl_data)
  ncols <- ncol(tbl_data)

  # Copy tbl_data to a new variable and apply gsub to retain only the second line
  cell_width <- lapply(tbl_data, function(x) gsub("(.*)\n(.*)", "\\2", x))
  # Calculate the number of characters in each cell
  cell_width <- lapply(cell_width, nchar)
  # Convert the list back to a data frame
  cell_width <- as.data.frame(cell_width)

  column_widths <- apply(cell_width, 2, max)
  # column_widths[8] <- 0
  # column_widths[2] <- column_widths[2] + 5
  # column_widths[8] <- max(column_widths)

  column_widths <- column_widths / sum(column_widths)


  # Convert df to grob
  g <- gridExtra::tableGrob(tbl_data,
    theme = table_theme, rows = NULL,
    widths = grid::unit(c(rep(5, ncols)), c("cm"))
  )

  # Add border under header
  g <- dx_hline(g,
    y = 0, x0 = 0, x1 = 1, t = 1, l = 1, r = ncols,
    name = "header_border"
  )

  # Add border under table
  g <- dx_hline(g,
    y = 0, x0 = 0, x1 = 1, t = nrows, l = 1, r = ncols,
    name = "footer_border_top"
  )
  g <- dx_hline(g,
    y = 0, x0 = 0, x1 = 1, t = nrows - 1, l = 1, r = ncols,
    name = "footer_border_bottom"
  )

  # Add borders to OR column
  g <- dx_vline(g,
    x = 0, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col,
    name = "left_or_border"
  )
  g <- dx_vline(g,
    x = 1, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col,
    name = "right_or_border"
  )

  # Add dashed line for overall OR
  g <- dx_vline(g,
    x = overall_or, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col,
    name = "overall_or", gp = grid::gpar(lwd = .8, lty = 2)
  )

  # Add OR's
  for (i in seq_along(estimate)) {
    col <- ifelse(i == length(estimate), footer_or_col, body_or_col)
    g <- dx_forest_add_or(
      g, i + 1, lower[i], estimate[i], upper[i],
      or_col = or_col, col = col, lwd = or_lwd, size = or_size
    )
  }

  # Add ticks and lables
  for (i in seq_along(breaks)) {
    g <- dx_forest_add_tick(g, breaks_scaled[i], labels[i],
      or_col = or_col,
      nrows = nrows, tick_label_size = tick_label_size
    )
  }

  # Bold bottom row
  g <- dx_edit_cell(g, nrow(g) - 1, seq_len(ncol(g)), "core-fg",
    gp = grid::gpar(fontface = "bold")
  )

  # Bold levels
  g <- dx_edit_cell(g, bold_rows + 1, 1, "core-fg",
    gp = grid::gpar(fontface = "bold")
  )

  # Left align first column
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "core-fg",
    x = grid::unit(.05, "npc"), hjust = 0
  )
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "colhead-fg",
    x = grid::unit(.05, "npc"), hjust = 0
  )

  # Darken total row
  g <- dx_edit_cell(
    g, nrow(g) - 1, seq_len(ncol(g)), "core-bg",
    gp = grid::gpar(fill = footer_bg)
  )

  # Color total row
  g <- dx_edit_cell(
    g, nrow(g) - 1, seq_len(ncol(g)), "core-fg",
    gp = grid::gpar(col = footer_col)
  )

  # Last row should be white (ticks and lables)
  g <- dx_edit_cell(g, nrow(g), seq_len(ncol(g)), "core-bg",
    gp = grid::gpar(fill = "#ffffff")
  )

  if (all(c("Odds Ratio", "Breslow-Day") %in% measures)) {

    col <- which(names(tbl_data) == "Odds Ratio")

    g <- dx_edit_cell(
      g, bold_rows + 1, col, "core-fg",
      gp = grid::gpar(fontface = "italic")
    )
  }


  # Adjust width of plot - some fine tunining here in the future woud be nice
  # g$widths <- grid::unit(rep(1 / ncol(g), ncol(g)), "npc")
  g$widths <- grid::unit(column_widths, "npc")
  # g$heights <- max(g$heights)

  row_height <- ifelse(fraction & fraction_multiline, 1.2, 1)

  g$heights <- rep(
    grid::unit(0.05*row_height, "npc"),
    length(g$heights)
  )

  if (!is.na(filename)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      g2 <- g
      g2$widths <- grid::unit(rep(1 / (ncol(g2) + 1), ncol(g2)), "npc")
      ggplot2::ggsave(g2,
        width = 8.5, height = 11, dpi = 600,
        filename = filename
      )
    } else {
      warning("ggplot2 needs to be installed to save a plot.
              No file was generated.")
    }
  }

  if (return_type == "ggplot") {
    g <- dx_forest_to_gg(g)
    # print(g)
  } else {
    # grid::grid.draw(g)
  }

  return(g)

}


dx_hline <- function(table, y, x0, x1, t, b = t, l, r = l, name,
                     gp = grid::gpar(lwd = .8), clip = "off") {
  gtable::gtable_add_grob(table,
    grobs = grid::grobTree(
      grid::segmentsGrob(
        x0 = grid::unit(x0, "npc"),
        y0 = grid::unit(y, "npc"),
        x1 = grid::unit(x1, "npc"),
        y1 = grid::unit(y, "npc"),
        gp = gp
      )
    ),
    t = t, b = b, l = l, r = r,
    name = name,
    z = Inf,
    clip = clip
  )
}

dx_vline <- function(table, x, y0, y1, t, b = t, l, r = l, name,
                     gp = grid::gpar(lwd = .8, col = "black"), clip = "off") {
  gtable::gtable_add_grob(table,
    grobs = grid::grobTree(
      grid::segmentsGrob(
        x0 = grid::unit(x, "npc"),
        y0 = grid::unit(y0, "npc"),
        x1 = grid::unit(x, "npc"),
        y1 = grid::unit(y1, "npc"),
        gp = gp
      )
    ),
    t = t, b = b, l = l, r = r,
    name = name,
    z = Inf,
    clip = clip
  )
}

#' Edit a cell within an object returned from dx_forest
#'
#' A convenient wrapper used to edit cells of a gtable.
#'
#' @param table a table returned from dx_forest
#' @param row Numeric vector of rows to edit
#' @param col Numeric vector of columns to edit
#' @param name Name of table layer to edit
#' @param ... Parameters passed to grid::editGrob such as grid::gpar or hjust.
#' @export
dx_edit_cell <- function(table, row, col, name = "core-fg", ...) {
  l <- table$layout
  ids <- which(l$t %in% row & l$l %in% col & l$name == name)
  for (id in ids) {
    newgrob <- grid::editGrob(table$grobs[id][[1]], ...)
    table$grobs[id][[1]] <- newgrob
  }
  table
}


dx_forest_add_or <- function(grob, row, low, est, high,
                             or_col = 4, col = "black", lwd = .8, pch = 16, size = .35) {

  i <- sample(1:100000, 1)

  tmp <- dx_hline(
    grob, gp = grid::gpar(lwd = lwd, col = col),
    y = .5, x0 = low, x1 = high, t = row, l = or_col,
    name = paste0("or", i), clip = "on"
  )
  tmp <- dx_vline(
    tmp, gp = grid::gpar(lwd = lwd, col = col),
    x = low, y0 = .35, y1 = .65, t = row, l = or_col,
    name = paste0("left_or_cap_", i), clip = "on"
  )
  tmp <- dx_vline(
    tmp, gp = grid::gpar(lwd = lwd, col = col),
    x = high, y0 = .35, y1 = .65, t = row, l = or_col,
    name = paste0("right_or_cap_", i), clip = "on"
  )

  gtable::gtable_add_grob(tmp,
    grobs = grid::grobTree(
      grid::pointsGrob(
        x = est,
        y = .5,
        pch = 16,
        gp = grid::gpar(col = col),
        size = grid::unit(size, "char")
      )
    ),
    t = row, l = or_col, name = "point1", z = Inf
  )
}


dx_forest_add_tick <- function(grob, tick_scaled, tick, nrows,
                               or_col = 4, tick_label_size) {
  tmp <- dx_vline(grob,
    x = tick_scaled, y0 = .8, y1 = 1, t = nrows + 1,
    l = or_col, name = paste0("tick_", tick)
  )

  gtable::gtable_add_grob(tmp,
    grobs = grid::grobTree(
      grid::textGrob(
        label = tick,
        x = tick_scaled,
        y = .5,
        gp = grid::gpar(fontsize = tick_label_size)
      )
    ),
    t = nrows + 1, l = or_col, name = paste0("tick_label_", tick),
    z = Inf,
    clip = "off"
  )
}

dx_prep_variable <- function(dx_obj, data,
                             measures = c("AUC", "Sensitivity", "Specificity","Odds Ratio"),
                             fraction = FALSE, fraction_multiline) {


  var <- data$variable[[1]]
  orig_var <- data$original_variable[[1]]
  tmp <- data[data$measure %in% measures, ]

  # Breslow-Day test will be added separately
  bd_test <- tmp[tmp$measure == "Breslow-Day", ]

  # Subset for rows where measure is not "Breslow-Day"
  tmp <- tmp[tmp$measure != "Breslow-Day", ]

  if (fraction) {
    if (fraction_multiline) {
      tmp$estimate <- ifelse(
        tmp$fraction == "",
        tmp$estimate,
        paste0(tmp$fraction, "\n", tmp$estimate)
      )
    } else {
      tmp$estimate <- ifelse(
        tmp$fraction == "",
        tmp$estimate,
        paste0(tmp$estimate, " (", tmp$fraction, ")")
      )
    }
  }

  # Selecting and renaming specific columns
  res_sel <- tmp[c("label", "measure", "estimate")]
  names(res_sel)[names(res_sel) == "label"] <- "group"


  # Filter for rows where measure is "Odds Ratio"
  filtered_data <- tmp[tmp$measure == "Odds Ratio", ]

  # Select and rename columns: 'label' to 'group', include 'n', and all columns starting with 'raw'
  cols_to_select <- c("label", "n", grep("^raw", names(filtered_data), value = TRUE))
  rawdata <- filtered_data[cols_to_select]
  names(rawdata)[names(rawdata) == "label"] <- "group"

  # Filter out rows where 'rawestime' is NA
  rawdata <- rawdata[!is.na(rawdata$rawestime), ]

  res <- utils::unstack(res_sel, form = estimate ~ measure)
  names(res) <- gsub("\\.", " ", names(res))
  if (var == "Overall") res <- as.data.frame(t(res))
  res$group <- unique(res_sel$group)
  res <- merge(res, rawdata, by = "group", all.x = TRUE)
  if (var != "Overall") {
    res$group <- factor(res$group, levels = levels(dx_obj$data[[orig_var]]))
    res <- res[order(res$group), ]
    res$group <- as.character(res$group)
    empty_df <- data.frame(group = var, stringsAsFactors = FALSE)
    res <- rbind_all(empty_df, res)

    if (nrow(bd_test) == 1) {
      res$`Odds Ratio`[res$group == var] <- bd_test$estimate
    }

  }

  res[] <- lapply(res, function(x) if(is.factor(x)) as.character(x) else x)

  res

}

rbind_all <- function(df1, df2) {
  df1[setdiff(names(df2), names(df1))] <- NA
  df2[setdiff(names(df1), names(df2))] <- NA
  rbind(df1, df2)
  # # Step 1: Identify all unique column names
  # all_cols <- union(names(x), names(y))
  #
  # # Step 2: Align columns by ensuring both data frames have all columns
  # # This fills in any missing columns with NA
  # empty_df_aligned <- setNames(lapply(all_cols, function(cn) x[[cn]]), all_cols)
  # res_aligned <- setNames(lapply(all_cols, function(cn) y[[cn]]), all_cols)
  #
  # # Step 3: Bind the rows together
  # rbind(empty_df_aligned, res_aligned)
}

label_df <- function(data) {
  x <- lapply(data, attr, which = "label", exact = TRUE)
  x <- lapply(x, function(x) ifelse(is.null(x), NA, x))
  data.frame(
    variable = names(x),
    variable_label = as.character(unlist(x, use.names = F)),
    stringsAsFactors = FALSE
  )
}

dx_prep_forest <- function(dx_obj, fraction = fraction, fraction_multiline, measures) {

  tmp <- dx_obj$measures[dx_obj$measures$threshold == dx_obj$options$setthreshold, ]


  labs <- label_df(data = dx_obj$data)

  tmp <- merge(tmp, labs, by = "variable", all.x = TRUE)

  # Copying the original 'variable' column to a new 'original_variable' column
  tmp$original_variable <- tmp$variable

  # Updating 'variable' column with 'variable_label' where it's not NA, otherwise keep 'variable'
  tmp$variable <- ifelse(is.na(tmp$variable_label), tmp$variable, tmp$variable_label)

  tmp_split <- split(tmp, tmp$variable)

  # Vector to store the current order of our split list
  # Alphabetical by label/variable
  var_order <- vector(mode = "character", length = length(tmp_split))

  for (i in seq_along(tmp_split)) {
    var_order[[i]] <- tmp_split[[i]]$original_variable[1]

    tmp_split[[i]] <- dx_prep_variable(
      dx_obj = dx_obj,
      data = tmp_split[[i]],
      fraction = fraction,
      fraction_multiline = fraction_multiline,
      measures = measures
    )
  }

  # Order based on dx input, plus Overall
  final_order <- c(dx_obj$options$grouping_variables, "Overall")

  # Numeric current order
  current_order <- vector(mode = "numeric", length = length(final_order))

  for (i in seq_along(final_order)) {
    current_order[[i]] <- which(var_order[[i]] == final_order)
  }

  # Re-order back to input
  tmp_split <- tmp_split[order(current_order)]


  do.call("rbind", tmp_split)
}


### Find limits of plot
# Limits: use these
# Breaks: use min(breaks) - max(breaks)
# No trans: use pretty()
# Trans: find min/max pos that contains
plot_range <- function(limits = NA, breaks = NA, trans = NA,
                       min_or = NA, max_or = NA) {
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
      low <- floor(log10(min_or))
      res <- 10^c(low, high)
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

dx_forest_to_gg <- function(plot, scale = 1, hjust = 0, vjust = 0, ...) {

  ymin <- xmin <- 1 - scale
  xmax <- ymax <- scale

  ggplot2::ggplot(data.frame(x = 0:1, y = 0:1), ggplot2::aes_(x = ~x, y = ~y)) +
    ggplot2::geom_blank() +
    ggplot2::scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    ggplot2::annotation_custom(
      plot,
      xmin = xmin + hjust,
      xmax = xmax + hjust,
      ymin = ymin + vjust,
      ymax = ymax + vjust
    )  +
    ggplot2::theme_void()
}
