dx_forrest <- function(data) {
  overall_or <- data %>% filter(Group == "Overall") %>% dplyr::pull(rawestime)
  min_or <- min(data$rawlci, na.rm = T)
  max_or <- max(data$rawuci, na.rm = T)


  ticks <- pretty(c(min_or, max_or), n = 6)
  ticks <- seq(0, 500, by = 100)
  ticks_scaled <- scales::rescale(ticks)

  lower <- data$rawlci
  estimate <- data$rawestime
  upper <- data$rawuci

  lower <- scales::rescale(lower, from = c(min(ticks), max(ticks)))
  estimate <- scales::rescale(estimate, from = c(min(ticks), max(ticks)))
  upper <- scales::rescale(upper, from = c(min(ticks), max(ticks)))
  overall_or <- scales::rescale(overall_or, from = c(min(ticks), max(ticks)))

  or_data <- data %>% select(starts_with("raw"))

  tbl_data <- data %>%
    mutate(` ` = "                                          ") %>%
    select(Group, Sensitivity, Specificity, ` `, `Odds Ratio`)

  tbl_data <- tbl_data %>% add_row()

  tbl_data <- tbl_data %>% mutate_all(tidyr::replace_na, replace = "")

  nrows <- nrow(tbl_data)

  row_colors <- rep(c("#e6e4e2", "#ffffff"), nrows)
  row_colors <- c(row_colors[1:nrows-1], "#ffffff")


  table_theme <- ttheme_minimal(
    core=list(margin=unit(c(1, 1), "mm"),
              bg_params = list(fill = row_colors, col=NA),
              fg_params=list(fontface=1, cex = .6)),
    colhead=list(fg_params=list(col="black", fontface=1, cex = .7)))

  or_col <- 4
  ncols <- ncol(tbl_data)

  g <- tableGrob(tbl_data, theme=table_theme, rows = NULL)

  # ggplot2::ggsave(plot = g, filename = "forrest_test.png")

  # Add border under header
  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(0,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(1,"npc"),
                           y1 = unit(0,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                       t = 1, l = 1, r = ncols, name = "header_border",
                       z = Inf,
                       clip = "off")

  # Add border under table
  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(0,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(1,"npc"),
                           y1 = unit(0,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                       t = nrows, l = 1, r = ncols, name = "header_border",
                       z = Inf,
                       clip = "off")

  # Add left border to OR
  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(0,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(0,"npc"),
                           y1 = unit(1,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                       t = 2, b = nrows, l = or_col , name = "left_or_border",
                       z = Inf,
                       clip = "off")

  # Add right border to OR
  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(1,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(1,"npc"),
                           y1 = unit(1,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                       t = 2, b = nrows, l = or_col, name = "right_or_border",
                       z = Inf,
                       clip = "off")

  # Add horizontal line
  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(overall_or,"npc"),
                           y0 = unit(0,"npc"),
                           x1 = unit(overall_or,"npc"),
                           y1 = unit(1,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, lty = 2, fill = "black"))),
                       t = 2, b = nrows, l = or_col)



  for (i in seq_along(estimate)) {
    g <- fx_forrest_add_or(g, i+1, lower[i], estimate[i], upper[i])
  }

  ### Footer

  g <- gtable_add_grob(g,
                       grobs = grobTree(
                         segmentsGrob(
                           x0 = unit(0,"npc"),
                           y0 = unit(.8,"npc"),
                           x1 = unit(0,"npc"),
                           y1 = unit(1,"npc"),
                           gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                       t = nrows+1, l = or_col, name = "lab1", z = Inf)

  tick_label_size = 6.5

  for (i in seq_along(ticks)) {
    g <- fx_forrest_add_tick(g, ticks_scaled[i], ticks[i],
                             nrows = nrows, tick_label_size = tick_label_size)
  }


  # Ids of first column
  id <- which(grepl("core-fg", g$layout$name ) & g$layout$l == 1 )

  # loop through grobs and left align
  for (i in id) {
    g$grobs[[i]]$x <- unit(.05, "npc")
    g$grobs[[i]]$hjust <- 0
  }

  ggplot2::ggsave(plot = g, filename = "forrest_test.png")

}

fx_forrest_add_or <- function(grob, row, low, est, high, or_col = 4) {
  # Add OR
  tmp <- gtable_add_grob(grob,
                         grobs = grobTree(
                           segmentsGrob(
                             x0 = unit(low,"npc"),
                             y0 = unit(.5,"npc"),
                             x1 = unit(high,"npc"),
                             y1 = unit(.5,"npc"),
                             gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                         t = row, l = or_col)
  # Add OR cap
  tmp <- gtable_add_grob(tmp,
                         grobs = grobTree(
                           segmentsGrob(
                             x0 = unit(low,"npc"),
                             y0 = unit(.35,"npc"),
                             x1 = unit(low,"npc"),
                             y1 = unit(.65,"npc"),
                             gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                         t = row, l = or_col,  name = paste0("left_or_cap_", i), z = Inf)
  tmp <- gtable_add_grob(tmp,
                         grobs = grobTree(
                           segmentsGrob(
                             x0 = unit(high,"npc"),
                             y0 = unit(.35,"npc"),
                             x1 = unit(high,"npc"),
                             y1 = unit(.65,"npc"),
                             gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                         t = row, l = or_col, name = paste0("right_or_cap_", i), z = Inf)

  gtable_add_grob(tmp,
                  grobs = grobTree(
                    pointsGrob(
                      x = est,
                      y = .5,
                      pch = 16,
                      size = unit(.35, "char"))),
                  t = row, l = or_col, name = "point1", z = Inf)
}


fx_forrest_add_tick <- function(grob, tick_scaled, tick, nrows, or_col = 4, tick_label_size) {
    tmp <- gtable_add_grob(grob,
                           grobs = grobTree(
                             segmentsGrob(
                               x0 = unit(tick_scaled,"npc"),
                               y0 = unit(.8,"npc"),
                               x1 = unit(tick_scaled,"npc"),
                               y1 = unit(1,"npc"),
                               gp = gpar(lwd = .8, alpha = 1, fill = "black"))),
                           t = nrows+1, l = or_col, name = paste0("tick_", tick),
                           z = Inf,
                           clip = "off")

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










