#' Create table with odds ratios displayed graphically
#'
#' Generate a table of diagnostic measures
#'
#' @param dx_obj An object of class dx
#' @param breaks A numeric vector of breaks to include on axis ticks.  If left empty, breaks will be determined using the min and max value from 95\% CIs.
#' @param limits Limits for axis ticks.  Ticks will be generates using base::breaks.  Ignored if breaks are passed.
#' @param tick_label_size Font size for axis labels.
#' @param return_grid Should a grid object be returned? If FALSE, grid is drawn using grid.draw.
#' @importFrom gtable gtable_add_grob
#' @importFrom grid grobTree unit gpar editGrob segmentsGrob pointsGrob textGrob
#' @export
#' @examples
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
#' grDevices::png(width=9, height=12)
#' dx_forrest(dx_obj)

dx_forrest <- function(dx_obj, breaks = NA, limits = NA, tick_label_size = 6.5, return_grid = FALSE) {

  data <- dx_prep_forrest(dx_obj)

  overall_or <- data %>% dplyr::filter(Group == "Overall") %>% dplyr::pull(rawestime)

  lower <- data$rawlci
  estimate <- data$rawestime
  upper <- data$rawuci

  #### Ticks and scaling

  if (!identical(breaks, NA)) { # Breaks provided
    ticks <- breaks
  } else if (!identical(limits, NA)) { # Only limits provided
    ticks <- pretty(limits, n = 6)
  } else { # Default tick behavior
    min_or <- min(data$rawlci, na.rm = T)
    max_or <- max(data$rawuci, na.rm = T)
    ticks <- pretty(c(min_or, max_or), n = 6)
  }

  # Rescale ticks between 0 and 1
  ticks_scaled <- scales::rescale(ticks)

  lower <- scales::rescale(lower, from = c(min(ticks), max(ticks)))
  estimate <- scales::rescale(estimate, from = c(min(ticks), max(ticks)))
  upper <- scales::rescale(upper, from = c(min(ticks), max(ticks)))
  overall_or <- scales::rescale(overall_or, from = c(min(ticks), max(ticks)))

  or_data <- data %>% dplyr::select(dplyr::starts_with("raw"))

  tbl_data <- data %>%
    dplyr::mutate(` ` = "                                          ") %>%
    dplyr::select(Group, Sensitivity, Specificity, ` `, `Odds Ratio`)

  tbl_data <- tbl_data %>% dplyr::add_row()

  # tbl_data <- tbl_data %>% dplyr::mutate_all(tidyr::replace_na, replace = "")
  tbl_data[is.na(tbl_data)] <- ""

  table_theme <- gridExtra::ttheme_minimal(
    core=list(margin=unit(c(1, 1), "mm"),
              bg_params = list(fill = rep(c("#e6e4e2", "#ffffff")), col=NA),
              fg_params=list(fontface=1, cex = .6)),
    colhead=list(fg_params=list(col="black", fontface=1, cex = .7)))

  or_col <- 4
  nrows <- nrow(tbl_data)
  ncols <- ncol(tbl_data)

  # Convert df to grob
  g <- gridExtra::tableGrob(tbl_data, theme=table_theme, rows = NULL)

  # Add border under header
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = 1, l = 1, r = ncols, name = "header_border")

  # Add border under table
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = nrows, l = 1, r = ncols, name = "footer_border_top")
  g <- dx_hline(g, y = 0, x0 = 0, x1 = 1, t = nrows-1, l = 1, r = ncols, name = "footer_border_bottom")

  # Add borders to OR column
  g <- dx_vline(g, x = 0, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col, name = "left_or_border")
  g <- dx_vline(g, x = 1, y0 = 0, y1 = 1, t = 2, b = nrows, l = or_col, name = "right_or_border")


  # Add OR's
  for (i in seq_along(estimate)) {
    g <- dx_forrest_add_or(g, i+1, lower[i], estimate[i], upper[i])
  }

  # Add ticks and lables
  for (i in seq_along(ticks)) {
    g <- dx_forrest_add_tick(g, ticks_scaled[i], ticks[i], nrows = nrows, tick_label_size = tick_label_size)
  }

  # Bold bottom row
  g <- dx_edit_cell(g, nrow(g)-1, seq_len(ncol(g)), "core-fg", gp=gpar(fontface="bold"))

  # Left align first column
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "core-fg", x=unit(.05, "npc"), hjust=0)
  g <- dx_edit_cell(g, seq_len(nrow(g)), 1, "colhead-fg", x=unit(.05, "npc"), hjust=0)

  # Darken total row
  g <- dx_edit_cell(g, nrow(g)-1, seq_len(ncol(g)), "core-bg", gp=gpar(fill="#b8b6b4"))

  # Last row should be white (ticks and lables)
  g <- dx_edit_cell(g, nrow(g), seq_len(ncol(g)), "core-bg", gp=gpar(fill="#ffffff"))

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

#' Edit a cell within an object returned from dx_forrest
#'
#' A convenient wrapper used to edit cells of a gtable.
#'
#' @param table a table returned from dx_forrest
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


dx_forrest_add_or <- function(grob, row, low, est, high, or_col = 4) {
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


dx_forrest_add_tick <- function(grob, tick_scaled, tick, nrows, or_col = 4, tick_label_size) {

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


dx_prep_forrest <- function(dx_obj) {

  tmp <- dx_obj$measures %>% dplyr::filter(threshold == dx_obj$options$setthreshold)

  tmp <- tmp %>% dplyr::mutate(Group = ifelse(Variable == "Overall", "Overall",  paste(Variable, Label)))


  tmp <- tmp %>% dplyr::filter(Measure %in% c("Sensitivity", "Specificity", "Odds Ratio"))
  res_sel <- tmp %>% dplyr::select(Group, Measure, Estimate)
  rawdata <- tmp %>% dplyr::select(Group, dplyr::starts_with("raw")) %>% dplyr::filter(!is.na(rawestime))

  res <- stats::reshape(data = res_sel,
                 idvar= "Group",
                 v.names= c("Estimate"),
                 timevar= "Measure",
                 direction = "wide")

  names(res) <- gsub("Estimate\\.", "", names(res))

  res <- dplyr::left_join(res, rawdata, by = "Group")

  subgroups <- res %>% dplyr::filter(Group != "Overall")
  overall <- res %>% dplyr::filter(Group == "Overall")

  rbind(subgroups, overall)

}
