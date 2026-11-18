#' Seasonal adjustment report
#'
#' `sa_report()` gathers, for one or several seasonal adjustment models, the description of the
#' pre-processing ([sa_preprocessing()]), of the decomposition ([sa_decomposition()]),
#' the diagnostics ([sa_diagnostics()]) and some plots.
#' In a Quarto (or R Markdown) document, the result is automatically rendered whatever the
#' output format.
#' `render_report()` directly renders a report in a file using a Quarto template
#' (see [report_template()]).
#'
#' @param x the model(s) to describe. It can be:
#' - a seasonal adjustment model (see [sa_preprocessing()] for the supported objects);
#' - a (named) list of seasonal adjustment models;
#' - a workspace: the path to the workspace file (`.xml`), a Java workspace opened with
#' [rjd3workspace::jws_open()] or a workspace read with [rjd3workspace::read_workspace()].
#' @param preprocessing,decomposition,diagnostics booleans indicating which parts should be printed.
#' @param plots boolean indicating if the plots (series and SI-ratios) should be printed.
#' @param title_level level of the first headings. For a workspace, the multiprocessings are
#' printed with headings of level `title_level` and the SA-items with headings of level `title_level + 1`.
#' For a list of models, the models are printed with headings of level `title_level`.
#' For a single model, the parts of the report (pre-processing, decomposition, diagnostics) are
#' printed with headings of level `title_level`.
#' @param ... other parameters passed to [sa_preprocessing()], [sa_decomposition()] and
#' [sa_diagnostics()] (e.g.: `digits` or `decimal.mark`).
#'
#' @return `sa_report()` returns an object of class `"rjd3report_block"`
#' (see [print.rjd3report_block()]).
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' sa_x13 <- rjd3x13::x13(y)
#' sa_ts <- rjd3tramoseats::tramoseats(y)
#' # In a Quarto document:
#' # sa_report(sa_x13)
#' # sa_report(list(X13 = sa_x13, `TRAMO-SEATS` = sa_ts))
#'
#' \donttest{
#' if (quarto::quarto_available()) {
#'   output_file <- tempfile(fileext = ".html")
#'   render_report(list(X13 = sa_x13, `TRAMO-SEATS` = sa_ts), output_file)
#'   # browseURL(output_file)
#'
#'   # From a workspace
#'   ws <- system.file("workspaces", "workspace_test.xml", package = "rjd3workspace")
#'   output_file <- tempfile(fileext = ".pdf")
#'   render_report(ws, output_file, output_format = "typst")
#' }
#' }
#' @export
sa_report <- function(x,
                      preprocessing = TRUE,
                      decomposition = TRUE,
                      diagnostics = TRUE,
                      plots = TRUE,
                      title_level = 2,
                      ...) {
  models <- normalize_models(x)
  args <- list(preprocessing = preprocessing, decomposition = decomposition,
               diagnostics = diagnostics, plots = plots, ...)
  if (inherits(models, "rjd3report_rslts")) {
    elements <- do.call(model_elements, c(list(models, title_level = title_level), args))
  } else if (is_nested(models)) {
    elements <- unlist(lapply(names(models), function(mp_name) {
      c(list(md_title(mp_name, level = title_level)),
        models_elements(models[[mp_name]], title_level = title_level + 1, args = args))
    }), recursive = FALSE)
  } else {
    elements <- models_elements(models, title_level = title_level, args = args)
  }
  new_block(elements, class = "rjd3report_report")
}

models_elements <- function(models, title_level, args) {
  unlist(lapply(names(models), function(name) {
    c(list(md_title(name, level = title_level)),
      do.call(model_elements, c(list(models[[name]], title_level = title_level + 1), args)))
  }), recursive = FALSE)
}

model_elements <- function(r, preprocessing = TRUE, decomposition = TRUE, diagnostics = TRUE,
                           plots = TRUE, title_level = NULL, ...) {
  elements <- list()
  is_sa <- !is.null(r$rslts$decomposition)
  if (plots && is_sa) {
    elements <- c(elements, list(
      md_plot(function() series_plot(sa_series(r$rslts)),
              caption = "Raw series, seasonally adjusted series and trend-cycle"),
      md_plot(function() ggdemetra3::ggsiratioplot(r$rslts, main = NULL) +
                ggplot2::theme_minimal() +
                ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major.x = ggplot2::element_blank()),
              caption = "SI-ratios")
    ))
  }
  if (preprocessing && !is.null(r$rslts$preprocessing)) {
    elements <- c(elements, call_with(sa_preprocessing, r, title_level = title_level, ...)$elements)
  }
  if (decomposition && is_sa) {
    elements <- c(elements, call_with(sa_decomposition, r, title_level = title_level, ...)$elements)
  }
  if (diagnostics) {
    elements <- c(elements, call_with(sa_diagnostics, r, title_level = title_level, ...)$elements)
  }
  elements
}
# Call a function with only the arguments it accepts
call_with <- function(fun, x, ...) {
  args <- list(...)
  args <- args[names(args) %in% names(formals(fun))]
  do.call(fun, c(list(x), args))
}

# Normalize the input of sa_report() to:
# - a rjd3report_rslts object (single model);
# - a named list of rjd3report_rslts objects;
# - a named list of named lists of rjd3report_rslts objects (workspace).
normalize_models <- function(x) {
  if (is.character(x) && length(x) == 1) {
    if (!requireNamespace("rjd3workspace", quietly = TRUE))
      stop("The package {rjd3workspace} is required to read workspaces.", call. = FALSE)
    x <- rjd3workspace::jws_open(normalizePath(x, mustWork = TRUE))
  }
  if (inherits(x, "jobjRef")) {
    x <- rjd3workspace::read_workspace(x, compute = TRUE)
  }
  if (is.list(x) && !is.null(x$processing) && is.list(x$processing)) {
    # workspace read with rjd3workspace::read_workspace()
    return(lapply(name_list(x$processing, "SAProcessing"), function(mp) {
      lapply(name_list(mp, "Model"), get_rslts)
    }))
  }
  if (is_single_model(x))
    return(get_rslts(x))
  if (is.list(x)) {
    x <- name_list(x, "Model")
    if (all(vapply(x, is_single_model, logical(1))))
      return(lapply(x, get_rslts))
    return(lapply(x, function(mp) {
      if (inherits(mp, "rjd3report_rslts") || is_single_model(mp))
        stop("Mixed lists of models and lists of models are not supported.", call. = FALSE)
      lapply(name_list(mp, "Model"), get_rslts)
    }))
  }
  get_rslts(x)
}
is_single_model <- function(x) {
  inherits(x, c("JD3_X13_OUTPUT", "JD3_TRAMOSEATS_OUTPUT", "JD3_REGARIMA_OUTPUT",
                "JD3_TRAMO_OUTPUT", "JD3_X13_RSLTS", "JD3_TRAMOSEATS_RSLTS",
                "JD3_REGARIMA_RSLTS", "JD3_Object", "rjd3report_rslts")) ||
    (is.list(x) && !is.null(x$results) && !is.null(x$ts)) # SA-item of a workspace
}
is_nested <- function(x) {
  is.list(x) && !inherits(x, "rjd3report_rslts") &&
    all(vapply(x, function(el) is.list(el) && !inherits(el, "rjd3report_rslts"), logical(1)))
}
name_list <- function(x, prefix) {
  if (is.null(names(x)))
    names(x) <- rep("", length(x))
  empty <- is.na(names(x)) | names(x) == ""
  names(x)[empty] <- sprintf("%s %i", prefix, which(empty))
  x
}

# Plots
#' @importFrom ggplot2 .data
NULL
series_plot <- function(x,
                        colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692"),
                        base_size = 11) {
  # x: output of sa_series()
  labels <- c(y = "Raw series", t = "Trend-cycle", sa = "Seasonally adjusted")
  dates <- as.numeric(stats::time(x))
  data <- do.call(rbind, lapply(names(colors), function(name) {
    res <- NULL
    for (type in c("Observed", "Forecast")) {
      col <- if (type == "Observed") name else paste0(name, "_f")
      if (col %in% colnames(x))
        res <- rbind(res, data.frame(date = dates, value = as.numeric(x[, col]),
                                     series = name, type = type))
    }
    res
  }))
  data$series <- factor(data$series, levels = names(colors))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$date, y = .data$value, color = .data$series,
                                          linetype = .data$type))
  if ("y_f" %in% colnames(x) && "y" %in% colnames(x)) {
    first_fcst <- dates[which(is.na(x[, "y"]) & !is.na(x[, "y_f"]))[1]]
    if (!is.na(first_fcst))
      p <- p + ggplot2::annotate("rect", xmin = first_fcst - 0.5 / stats::frequency(x), xmax = Inf,
                                 ymin = -Inf, ymax = Inf, fill = "grey93")
  }
  p +
    ggplot2::geom_line(na.rm = TRUE, linewidth = 0.6) +
    ggplot2::scale_color_manual(values = colors, labels = labels[names(colors)], name = NULL) +
    ggplot2::scale_linetype_manual(values = c(Observed = "solid", Forecast = "22"),
                                   guide = "none") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.box.spacing = grid::unit(2, "pt"))
}
md_plot <- function(fun, caption = NULL) {
  list(type = "plot", fun = fun, caption = caption)
}
plot2md <- function(el) {
  fig_width <- knitr::opts_current$get("fig.width")
  fig_height <- knitr::opts_current$get("fig.height")
  if (is.null(fig_width)) fig_width <- 7
  if (is.null(fig_height)) fig_height <- 4
  dpi <- knitr::opts_current$get("dpi")
  if (is.null(dpi)) dpi <- 150
  fig_dir <- knitr::opts_current$get("fig.path")
  if (is.null(fig_dir)) fig_dir <- "figure/"
  file <- sprintf("%srjd3report-%s.png", fig_dir, next_fig_id())
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({
    ggplot2::ggsave(file, el$fun(), width = fig_width, height = fig_height, dpi = dpi)
    TRUE
  }, error = function(e) {
    warning(sprintf("Plot '%s' not produced: %s", el$caption, conditionMessage(e)), call. = FALSE)
    FALSE
  })
  if (!ok)
    return("")
  caption <- if (is.null(el$caption)) "" else el$caption
  sprintf("![%s](%s){fig-align=\"center\" width=\"90%%\"}", caption, file)
}
fig_env <- new.env(parent = emptyenv())
next_fig_id <- function() {
  id <- if (is.null(fig_env$id)) 1L else fig_env$id + 1L
  fig_env$id <- id
  sprintf("%s-%03i", knitr::opts_current$get("label"), id)
}

#' @rdname sa_report
#' @param output_file the output file (e.g. `"report.html"`, `"report.pdf"` or `"report.docx"`).
#' @param output_format the Quarto output format: `"html"` (default), `"pdf"` (PDF with LaTeX),
#' `"typst"` (PDF with Typst) or `"docx"`. See [quarto::quarto_render()].
#' @param title the title of the report.
#' @param template the Quarto template used to render the report (see [report_template()]).
#' @param quiet boolean indicating if the output of Quarto should be suppressed.
#' @return `render_report()` returns invisibly the path of the output file.
#' @export
render_report <- function(x,
                          output_file,
                          output_format = c("html", "pdf", "typst", "docx"),
                          title = "Seasonal adjustment report",
                          template = report_template(),
                          preprocessing = TRUE,
                          decomposition = TRUE,
                          diagnostics = TRUE,
                          plots = TRUE,
                          title_level = 1,
                          quiet = TRUE,
                          ...) {
  if (!requireNamespace("quarto", quietly = TRUE))
    stop("The package {quarto} is required to render reports.", call. = FALSE)
  output_format <- match.arg(output_format)
  ext <- switch(output_format, html = "html", pdf = "pdf", typst = "pdf", docx = "docx")
  if (missing(output_file))
    output_file <- file.path(getwd(), paste0("sa_report.", ext))
  output_file <- normalizePath(output_file, mustWork = FALSE)
  # Java objects can't be saved: the models are converted to R objects
  models <- normalize_models(x)

  work_dir <- tempfile("rjd3report")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  input <- file.path(work_dir, "sa_report.qmd")
  file.copy(template, input)
  saveRDS(list(models = models,
               options = list(preprocessing = preprocessing, decomposition = decomposition,
                              diagnostics = diagnostics, plots = plots,
                              title_level = title_level, ...)),
          file.path(work_dir, "models.rds"))
  quarto::quarto_render(input,
                        output_format = output_format,
                        execute_params = list(models = "models.rds"),
                        metadata = list(title = title),
                        quiet = quiet)
  output <- file.path(work_dir, paste0("sa_report.", ext))
  if (!file.exists(output))
    stop("The report has not been rendered.", call. = FALSE)
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  file.copy(output, output_file, overwrite = TRUE)
  invisible(output_file)
}

#' Quarto template of the seasonal adjustment report
#'
#' `report_template()` returns the path of the Quarto template used by [render_report()].
#' `use_report_template()` copies it in a directory so that it can be customized
#' (and then used with `render_report(template = )`).
#'
#' @param path the path of the copied template.
#' @param overwrite boolean indicating if an existing file should be overwritten.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' report_template()
#' @export
report_template <- function() {
  system.file("quarto", "sa_report.qmd", package = "rjd3report", mustWork = TRUE)
}
#' @rdname report_template
#' @export
use_report_template <- function(path = "sa_report.qmd", overwrite = FALSE) {
  if (file.exists(path) && !overwrite)
    stop(sprintf("The file '%s' already exists (use `overwrite = TRUE`).", path), call. = FALSE)
  file.copy(report_template(), path, overwrite = overwrite)
  invisible(path)
}
