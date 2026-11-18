#' Colors of the dashboards
#'
#' Colors used in the tables of the dashboards (see [simple_dashboard()]) according to the status
#' of each statistic.
#'
#' @param good,uncertain,bad colors of the good, uncertain and bad statistics.
#' @param neutral color of the statistics without status (e.g. variance decomposition).
#' @param palette `"pastel"` (default) or `"classic"` (colors of the previous versions).
#' @return A named character vector.
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' dashboard_colors()
#' dashboard_colors(palette = "classic")
#' @export
dashboard_colors <- function(good = NULL, uncertain = NULL, bad = NULL, neutral = NULL,
                             palette = c("pastel", "classic")) {
  palette <- match.arg(palette)
  res <- switch(palette,
                pastel = c(good = "#CDE8B4", uncertain = "#FBDDA0", bad = "#F4B6B0", neutral = "#EDEDED"),
                classic = c(good = "#A0CD63", uncertain = "orange", bad = "red", neutral = "grey90"))
  if (!is.null(good)) res["good"] <- good
  if (!is.null(uncertain)) res["uncertain"] <- uncertain
  if (!is.null(bad)) res["bad"] <- bad
  if (!is.null(neutral)) res["neutral"] <- neutral
  res
}

status_presence <- function(pval) {
  res <- ifelse(pval < 0.01, "good", ifelse(pval < 0.05, "uncertain", "bad"))
  res[is.na(pval)] <- NA
  res
}
status_residual <- function(pval) {
  res <- ifelse(pval < 0.01, "bad", ifelse(pval < 0.05, "uncertain", "good"))
  res[is.na(pval)] <- NA
  res
}
status_combined <- function(x, residual = FALSE) {
  res <- ifelse(x == "Present", if (residual) "bad" else "good",
                ifelse(x == "None", if (residual) "good" else "bad", "uncertain"))
  res[is.na(x)] <- NA
  res
}
status2color <- function(status, colors, default = "white") {
  res <- unname(colors[status])
  res[is.na(res)] <- default
  dim(res) <- dim(status)
  res
}

# Formatted tables with the status of each cell
dashboard_tables <- function(x, digits = x$digits, decimal.mark = getOption("OutDec"),
                             digits_outliers = x$digits_outliers) {
  fnum <- function(v, d = digits) format_num(v, digits = d, decimal.mark = decimal.mark)
  # Decomposition statistics
  var_decomp <- x$decomposition$var_decomp
  qstats <- x$decomposition$qstats
  decomp <- as.data.frame(t(fnum(var_decomp)), check.names = FALSE)
  colnames(decomp) <- names(var_decomp)
  decomp_status <- rep("neutral", ncol(decomp))
  if (!is.null(qstats)) {
    q <- as.data.frame(t(fnum(qstats)), check.names = FALSE)
    colnames(q) <- names(qstats)
    decomp <- cbind(q, "   " = "", decomp)
    decomp_status <- c(ifelse(qstats < 1, "good", "bad"), NA, decomp_status)
  }
  decomp_status <- matrix(decomp_status, nrow = 1)

  # Tests
  pval <- x$tests$pvalues
  combined <- x$tests$combined
  tests <- data.frame(matrix(fnum(pval), nrow = nrow(pval)), combined,
                      row.names = rownames(pval))
  colnames(tests) <- c(colnames(pval), "Combined")
  tests[is.na(tests)] <- ""
  tests_status <- cbind(
    rbind(status_presence(pval[1, ]), status_residual(pval[2, ])),
    c(status_combined(combined[1]), status_combined(combined[2], residual = TRUE))
  )
  if (!is.null(x$tests$residual_td)) {
    td <- x$tests$residual_td
    tests <- rbind(tests,
                   "Residual TD effect" = c(fnum(td), rep("", ncol(tests) - 1)))
    tests_status <- rbind(tests_status, c(status_residual(td), rep(NA, ncol(tests) - 1)))
  }

  # Outliers
  outliers <- x$outliers
  if (!is.null(outliers)) {
    outliers[-1] <- lapply(outliers[-1], fnum, d = digits_outliers)
    colnames(outliers)[1] <- sprintf("Last %s", plural(nrow(outliers), "outlier"))
  }
  list(decomposition = list(table = decomp, status = decomp_status),
       tests = list(table = tests, status = tests_status),
       outliers = list(table = outliers))
}

summary_lines <- function(x, compact = FALSE) {
  s <- x$summary
  sep <- if (compact) ", " else "\n"
  td <- if (s$ntd == 0) "No trading days effect" else sprintf("Trading days effect (%s)", s$ntd)
  easter <- if (s$easter) "easter effect" else "no easter effect"
  if (!compact) easter <- paste0(toupper(substr(easter, 1, 1)), substr(easter, 2, nchar(easter)))
  c(if (compact) {
    sprintf("Estimation span: %s to %s (%s obs)", s$span_start, s$span_end, s$nobs)
  } else {
    sprintf("Estimation span: %s to %s\n%s observations", s$span_start, s$span_end, s$nobs)
  },
  if (s$log) "Series has been log-transformed" else "Series hasn't been transformed",
  paste(td, easter, sep = sep),
  sprintf("%s detected outliers", s$nout),
  s$arima)
}

#' Plot a simple seasonal adjustment dashboard
#'
#' Functions to plot a simple dashboard of a seasonal adjustment model: `plot()` uses base R graphics
#' and `autoplot()` uses \pkg{ggplot2} (and \pkg{patchwork}).
#' With `autoplot()`, the tables keep their natural size (their font size is reduced if the figure is too small)
#' and only the plots are resized.
#'
#' @param x,object A `simple_dashboard` object.
#' @param main Main title.
#' @param subtitle Subtitle.
#' @param reference_date Boolean indicating if the reference date should be printed.
#' @param color_series Color of the raw time series, the trend and the seasonally adjusted component.
#' @param colors colors of the tables according to the status of the statistics (see [dashboard_colors()]).
#' @param digits,digits_outliers number of digits used in the tables (by default the values used in [simple_dashboard()]).
#' @param decimal.mark the character used as decimal mark.
#' @param ... Other unused parameters.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' data <- window(rjd3toolkit::ABS$X0.2.09.10.M, start = 2003)
#' sa_model <- rjd3x13::x13(data)
#' dashboard_data <- simple_dashboard(sa_model)
#' plot(dashboard_data, main = "Simple dashboard")
#' dashboard_data2 <- simple_dashboard2(sa_model)
#' plot(dashboard_data2, main = "Simple dashboard with outliers")
#' autoplot(dashboard_data2, main = "Simple dashboard with outliers")
#' @seealso \code{\link{simple_dashboard}}.
#' @importFrom graphics box layout legend mtext par plot.new text
#' @export
plot.simple_dashboard <- function(x, main = "Simple Dashboard",
                                  subtitle = NULL,
                                  color_series = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692"),
                                  reference_date = TRUE,
                                  colors = dashboard_colors(),
                                  digits = x$digits,
                                  digits_outliers = x$digits_outliers,
                                  decimal.mark = getOption("OutDec"),
                                  ...) {
  tables <- dashboard_tables(x, digits = digits, decimal.mark = decimal.mark,
                             digits_outliers = digits_outliers)
  is_sd <- !isTRUE(x$show_outliers)
  summary_text <- summary_lines(x, compact = !is_sd)
  main_plot <- x$series

  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  if (is_sd) {
    layout(matrix(c(rep(1, 8),
                    rep(2, 4), rep(3, 4),
                    rep(4, 3), rep(5, 5),
                    rep(4, 3), rep(6, 5)), ncol = 8, byrow = TRUE),
           heights = c(0.2, 2.5, 0.5, 1.3))
    cex <- 1
    y_res_test <- 0.5
  } else {
    layout(matrix(c(rep(1, 8),
                    rep(2, 4), rep(3, 4),
                    rep(4, 3), rep(5, 5),
                    rep(4, 3), rep(6, 5),
                    rep(7, 3), rep(8, 5)), ncol = 8, byrow = TRUE),
           heights = c(0.2, 2.2, 0.5, 0.2, 0.7))
    cex <- 0.95
    y_res_test <- 0.8
  }

  par(oma = rep.int(0, 4))
  base_dashboard_title(main)

  par(mai = c(0, 0.4, 0.2, 0.1))
  cols <- intersect(c("y", "t", "sa", "y_f", "t_f", "sa_f"), colnames(main_plot))
  stats::plot.ts(main_plot[, cols], plot.type = "single",
                 col = rep(color_series, 2)[match(cols, c("y", "t", "sa", "y_f", "t_f", "sa_f"))],
                 lty = rep(c(1, 2), each = 3)[match(cols, c("y", "t", "sa", "y_f", "t_f", "sa_f"))],
                 xlab = NULL, ylab = NULL, main = NULL)
  legend("bottomright", legend = names(color_series),
         col = color_series, lty = 1, pch = NA_integer_,
         inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n")
  par(mai = c(0.0, 0.2, 0.2, 0.4))
  ggdemetra3::siratioplot(x$siratio, main = NULL)

  par(mai = c(0.4, 0.2, 0.2, 0))
  par(mar = rep.int(0.4, 4))
  plot.new()
  legend("topleft", legend = c(NA, summary_text),
         bty = "n", text.font = 2, inset = c(0), cex = cex, xpd = TRUE)

  par(mar = rep(2, 4))
  par(mai = c(0, 0.2, 0, 0.2))
  empty_plot()
  plotrix::addtable2plot(0.5, 0,
                         tables$decomposition$table, bty = "o", display.rownames = FALSE,
                         hlines = TRUE, vlines = TRUE,
                         bg = status2color(tables$decomposition$status, colors),
                         xjust = 0.5, yjust = 1)
  if (!is_sd) {
    empty_plot()
    empty_plot()
    par(mai = c(0, 0.2, 0.2, 0.2))
    if (!is.null(tables$outliers$table)) {
      plotrix::addtable2plot(0.5, 0.7,
                             tables$outliers$table,
                             bty = "o", display.rownames = FALSE,
                             hlines = TRUE, vlines = TRUE,
                             xjust = 0.5, yjust = 0.5)
    }
  }
  empty_plot()
  par(mai = c(0, 0.2, 0.2, 0.2))
  plotrix::addtable2plot(0.5, y_res_test,
                         tables$tests$table, bty = "o",
                         display.rownames = TRUE, hlines = TRUE, vlines = TRUE,
                         bg = status2color(tables$tests$status, colors),
                         xjust = 0.5, yjust = 0.5)
  base_dashboard_subtitle(subtitle, if (reference_date) x$last_date)
  invisible()
}
# Title and subtitle of the dashboards plotted with base R
base_dashboard_title <- function(main) {
  o.par <- par(mar = rep.int(0, 4))
  on.exit(par(o.par))
  plot.new()
  box(which = "inner")
  box()
  text(0.5, 0.5, main, font = 2, cex = 1.2)
}
base_dashboard_subtitle <- function(subtitle = NULL, last_date = NULL) {
  if (!is.null(last_date))
    mtext(sprintf("Reference Date: %s", last_date), side = 3, line = -3,
          outer = TRUE, font = 3, cex = 0.7, at = 0.95, adj = 1)
  if (!is.null(subtitle))
    mtext(subtitle, side = 3, line = -3,
          outer = TRUE, font = 3, cex = 0.7, at = 0.1, adj = 1)
}
empty_plot <- function() {
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)
}

#' @rdname plot.simple_dashboard
#' @param base_size base font size of the plots and of the text.
#' @param table_base_size base font size of the tables.
#' @importFrom ggplot2 autoplot
#' @export
autoplot.simple_dashboard <- function(object, main = "Simple Dashboard",
                                      subtitle = NULL,
                                      color_series = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692"),
                                      reference_date = TRUE,
                                      colors = dashboard_colors(),
                                      digits = object$digits,
                                      digits_outliers = object$digits_outliers,
                                      decimal.mark = getOption("OutDec"),
                                      base_size = 10,
                                      table_base_size = 9,
                                      ...) {
  if (!requireNamespace("patchwork", quietly = TRUE) || !requireNamespace("gridExtra", quietly = TRUE))
    stop("The packages {patchwork} and {gridExtra} are required.", call. = FALSE)
  x <- object
  tables <- dashboard_tables(x, digits = digits, decimal.mark = decimal.mark,
                             digits_outliers = digits_outliers)
  show_outliers <- isTRUE(x$show_outliers)

  p_series <- series_plot(x$series, colors = color_series, base_size = base_size)
  p_si <- ggdemetra3::ggsiratioplot(x$siratio, main = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  # Bottom: text and tables with their natural size
  gap <- 0.25
  text_builder <- function(size) {
    grid::textGrob(paste(summary_lines(x, compact = show_outliers), collapse = "\n"),
                   x = 0, hjust = 0, y = 1, vjust = 1,
                   gp = grid::gpar(fontsize = size, lineheight = 1.2))
  }
  decomp_builder <- function(size) {
    table_grob(tables$decomposition$table, status2color(tables$decomposition$status, colors),
               base_size = size)
  }
  tests_builder <- function(size) {
    table_grob(tables$tests$table, status2color(tables$tests$status, colors),
               rows = rownames(tables$tests$table), base_size = size)
  }
  h_text <- grob_size(text_builder(base_size))["height"] + 0.2
  h_decomp <- grob_size(decomp_builder(table_base_size))["height"]
  h_tests <- grob_size(tests_builder(table_base_size))["height"]
  right <- gridExtra::arrangeGrob(
    fit_grob(decomp_builder, table_base_size),
    fit_grob(tests_builder, table_base_size),
    ncol = 1, heights = grid::unit(c(h_decomp + gap, h_tests), "cm"))
  h_right <- h_decomp + gap + h_tests
  if (show_outliers && !is.null(tables$outliers$table)) {
    out_builder <- function(size) {
      table_grob(tables$outliers$table, NULL, base_size = size)
    }
    h_out <- grob_size(out_builder(table_base_size))["height"]
    left <- gridExtra::arrangeGrob(
      fit_grob(text_builder, base_size, hjust = 0),
      fit_grob(out_builder, table_base_size),
      ncol = 1, heights = grid::unit(c(h_text, h_out), "cm"))
    h_left <- h_text + h_out
  } else {
    left <- fit_grob(text_builder, base_size, hjust = 0)
    h_left <- h_text
  }
  h_bottom <- max(h_left, h_right) + gap

  subtitle_ <- c(subtitle, if (reference_date) sprintf("Reference date: %s", x$last_date))
  subtitle_ <- if (length(subtitle_)) paste(subtitle_, collapse = " \u2014 ") else NULL

  (p_series + p_si + patchwork::plot_layout(widths = c(1.3, 1))) /
    (patchwork::wrap_elements(full = left) + patchwork::wrap_elements(full = right) +
       patchwork::plot_layout(widths = c(1, 1.5))) +
    patchwork::plot_layout(heights = grid::unit(c(1, h_bottom), c("null", "cm"))) +
    patchwork::plot_annotation(
      title = main, subtitle = subtitle_,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.4),
        plot.subtitle = ggplot2::element_text(colour = "grey40", size = base_size * 0.9)))
}

# Table grob with light rules: bold header, no vertical lines, colored cells
table_grob <- function(df, fill = NULL, rows = NULL, base_size = 9) {
  if (is.null(fill))
    fill <- matrix("white", nrow(df), ncol(df))
  th <- gridExtra::ttheme_minimal(
    base_size = base_size,
    core = list(bg_params = list(fill = fill, col = "white", lwd = 1.5),
                fg_params = list(hjust = 1, x = 0.92)),
    colhead = list(fg_params = list(fontface = "bold", hjust = 1, x = 0.92),
                   bg_params = list(fill = "white", col = NA)),
    rowhead = list(fg_params = list(fontface = "plain", hjust = 0, x = 0.04),
                   bg_params = list(fill = "white", col = NA)),
    padding = grid::unit(c(2.5, 1.6), "mm"))
  g <- gridExtra::tableGrob(df, rows = rows, theme = th)
  # Left align the first column if it contains labels
  rule <- grid::segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = grid::gpar(lwd = 0.8))
  g <- gtable::gtable_add_grob(g, rule, t = 1, l = 1, r = ncol(g), name = "header-rule")
  bottom <- grid::segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = grid::gpar(lwd = 0.8))
  gtable::gtable_add_grob(g, bottom, t = nrow(g), l = 1, r = ncol(g), name = "bottom-rule")
}

grob_size <- function(g) {
  if (inherits(g, "gtable")) {
    w <- sum(g$widths)
    h <- sum(g$heights)
  } else {
    w <- grid::grobWidth(g)
    h <- grid::grobHeight(g)
  }
  c(width = grid::convertWidth(w, "cm", valueOnly = TRUE),
    height = grid::convertHeight(h, "cm", valueOnly = TRUE))
}

# Grob whose font size is reduced at drawing time if it doesn't fit in its viewport
fit_grob <- function(builder, base_size, hjust = 0.5) {
  grid::gTree(builder = builder, base_size = base_size, fit_hjust = hjust,
              cl = "rjd3report_fit_grob")
}
#' @importFrom grid makeContent
#' @exportS3Method grid::makeContent
makeContent.rjd3report_fit_grob <- function(x) {
  g <- x$builder(x$base_size)
  size <- grob_size(g)
  available <- c(grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE),
                 grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE))
  scale <- min(1, available / size)
  if (is.finite(scale) && scale < 1) {
    g <- x$builder(x$base_size * scale * 0.97)
    size <- grob_size(g)
  }
  if (inherits(g, "gtable")) {
    # position: centered horizontally (or left aligned), top aligned
    g$vp <- grid::viewport(x = grid::unit(x$fit_hjust, "npc"), y = grid::unit(1, "npc"),
                           width = grid::unit(size["width"], "cm"),
                           height = grid::unit(size["height"], "cm"),
                           just = c(x$fit_hjust, 1))
  }
  grid::setChildren(x, grid::gList(g))
}
