# Internal helpers for the reporting functions (sa_preprocessing(), sa_decomposition(),
# sa_diagnostics()). Each reporting function returns an object of class "rjd3report_block":
# a list of elements that are either markdown text or tables. The rendering is done by
# `knit_print()` (in a Quarto/R Markdown document, the output format is detected
# automatically by {tinytable}) or by `print()` (in the console).

# Normalize the different SA objects to a list with the R results
get_rslts <- function(x, ...) {
  UseMethod("get_rslts", x)
}
#' @exportS3Method
get_rslts.default <- function(x, ...) {
  stop(sprintf("Object of class '%s' not supported.", paste(class(x), collapse = "', '")),
       call. = FALSE)
}
#' @exportS3Method
get_rslts.JD3_X13_OUTPUT <- function(x, ...) {
  new_rslts(x$result, method = "X13", spec = x$estimation_spec)
}
#' @exportS3Method
get_rslts.JD3_TRAMOSEATS_OUTPUT <- function(x, ...) {
  new_rslts(x$result, method = "TRAMO-SEATS", spec = x$estimation_spec)
}
#' @exportS3Method
get_rslts.JD3_REGARIMA_OUTPUT <- function(x, ...) {
  new_rslts(x$result, method = "RegARIMA", spec = x$estimation_spec)
}
#' @exportS3Method
get_rslts.JD3_TRAMO_OUTPUT <- function(x, ...) {
  new_rslts(x$result, method = "TRAMO", spec = x$estimation_spec)
}
#' @exportS3Method
get_rslts.JD3_X13_RSLTS <- function(x, ...) {
  new_rslts(x, method = "X13")
}
#' @exportS3Method
get_rslts.JD3_TRAMOSEATS_RSLTS <- function(x, ...) {
  new_rslts(x, method = "TRAMO-SEATS")
}
#' @exportS3Method
get_rslts.JD3_REGARIMA_RSLTS <- function(x, ...) {
  new_rslts(list(preprocessing = x), method = "RegARIMA")
}
#' @exportS3Method
get_rslts.JD3_Object <- function(x, ...) {
  jclass <- rJava::.jclass(x$internal)
  if (grepl("x13", jclass, ignore.case = TRUE)) {
    new_rslts(rjd3x13::.x13_rslts(x$internal), method = "X13")
  } else if (grepl("tramoseats", jclass, ignore.case = TRUE)) {
    new_rslts(rjd3tramoseats::.tramoseats_rslts(x$internal), method = "TRAMO-SEATS")
  } else {
    stop(sprintf("Java object of class '%s' not supported.", jclass), call. = FALSE)
  }
}
# SA-item read from a workspace with rjd3workspace::read_sai() or rjd3workspace::read_workspace()
#' @exportS3Method
get_rslts.list <- function(x, ...) {
  if (is.null(x$results))
    stop("The model has no results: compute the workspace before (see `rjd3workspace::read_workspace()`).",
         call. = FALSE)
  res <- get_rslts(x$results)
  res$spec <- x$estimationSpec
  res
}
#' @exportS3Method
get_rslts.rjd3report_rslts <- function(x, ...) {
  x
}
new_rslts <- function(rslts, method, spec = NULL) {
  structure(list(rslts = rslts, method = method, spec = spec),
            class = "rjd3report_rslts")
}
preprocessing_name <- function(method) {
  if (method %in% c("TRAMO-SEATS", "TRAMO")) "TRAMO" else "RegARIMA"
}

# Output format of the current knitr document ("html", "latex", "typst", "docx", ...)
output_format <- function() {
  fmt <- knitr::pandoc_to()
  if (is.null(fmt))
    fmt <- "markdown"
  fmt
}

# Blocks
md_text <- function(...) {
  list(type = "text", content = paste0(...))
}
md_title <- function(title, level = NULL) {
  if (is.null(level)) {
    md_text(sprintf("**%s**", title))
  } else {
    md_text(paste(strrep("#", level), title))
  }
}
md_table <- function(data, caption = NULL, notes = NULL, align = NULL, width = NULL) {
  list(type = "table", data = data, caption = caption, notes = notes, align = align,
       width = width)
}
new_block <- function(elements, class, ...) {
  structure(c(list(elements = elements), list(...)),
            class = c(class, "rjd3report_block"))
}

# Build a tinytable object from a table block
block2tt <- function(x) {
  width <- x$width
  if (is.null(width) && !is.null(x$notes) && identical(output_format(), "typst")) {
    # Typst: with notes spanning all the columns, the automatic width of the columns is too small
    width <- typst_width(x$data)
  }
  tab <- tinytable::tt(x$data, caption = x$caption, notes = x$notes, width = width)
  if (!is.null(x$groups))
    tab <- tinytable::group_tt(tab, i = x$groups)
  if (!is.null(x$align))
    tab <- tinytable::style_tt(tab, j = seq_len(ncol(x$data)), align = x$align)
  tab <- tinytable::format_tt(tab, escape = TRUE)
  tinytable::theme_latex(tab, placement = getOption("rjd3report.latex_placement", "H"))
}

typst_width <- function(data) {
  n <- ncol(data)
  w <- c(2.5, rep(1, n - 1))
  if (identical(colnames(data)[n], " "))
    w[n] <- 0.5
  total <- min(0.9, 0.15 * sum(w))
  w / sum(w) * total
}

#' Print reporting objects
#'
#' Methods to print the objects created by [sa_preprocessing()], [sa_decomposition()],
#' [sa_diagnostics()] and [sa_report()].
#' In a Quarto or R Markdown document, the objects are automatically rendered with
#' `knit_print()`: the output format (HTML, PDF, Typst, Word...) is detected and the tables are
#' built with \pkg{tinytable}.
#'
#' @param x the object to print.
#' @param ... unused parameters.
#' @name print.rjd3report_block
#' @export
print.rjd3report_block <- function(x, ...) {
  for (el in x$elements) {
    if (identical(el$type, "text")) {
      txt <- gsub("**", "", el$content, fixed = TRUE)
      txt <- gsub("$", "", txt, fixed = TRUE)
      cat(txt, "\n\n", sep = "")
    } else if (identical(el$type, "table")) {
      print(block2tt(el), output = "markdown")
      cat("\n")
    } else if (identical(el$type, "plot")) {
      print(el$fun())
    }
  }
  invisible(x)
}

#' @rdname print.rjd3report_block
#' @param options,inline see [knitr::knit_print()].
#' @importFrom knitr knit_print
#' @exportS3Method knitr::knit_print
knit_print.rjd3report_block <- function(x, options = NULL, inline = FALSE, ...) {
  knitr::asis_output(paste(c("", block2md(x), ""), collapse = "\n\n"))
}
block2md <- function(x) {
  vapply(x$elements, function(el) {
    if (identical(el$type, "text")) {
      el$content
    } else if (identical(el$type, "plot")) {
      plot2md(el)
    } else {
      paste(knitr::knit_print(block2tt(el)), collapse = "\n")
    }
  }, character(1))
}

# Formatting functions
format_num <- function(x, digits = 3, decimal.mark = getOption("OutDec")) {
  res <- formatC(x, digits = digits, format = "f", decimal.mark = decimal.mark)
  res[is.na(x)] <- ""
  res
}
format_pval <- function(x, digits = 3, decimal.mark = getOption("OutDec")) {
  res <- format_num(x, digits = digits, decimal.mark = decimal.mark)
  small <- !is.na(x) & x < 10^(-digits)
  res[small] <- paste0("<", format_num(10^(-digits), digits = digits, decimal.mark = decimal.mark))
  res
}
signif_stars <- function(pval) {
  res <- as.character(cut(pval,
                          breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                          include.lowest = TRUE,
                          labels = c("***", "**", "*", ".", "")))
  res[is.na(res)] <- ""
  res
}
signif_stars_note <- function() {
  "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
}
plural <- function(n, word) {
  sprintf("%s %s%s", n, word, ifelse(n > 1, "s", ""))
}

# Write a polynomial in the backshift operator B (in markdown math).
# `x` are the coefficients of degree 0, 1, 2...
format_polynomial <- function(x, digits = 3, decimal.mark = getOption("OutDec"),
                              tol = 1e-10) {
  if (length(x) == 0)
    return("$1$")
  degree <- seq_along(x) - 1
  # Sum polynomial 1 + B + ... + B^k
  if (length(x) > 3 && all(abs(x - 1) < tol)) {
    k <- length(x) - 1
    return(sprintf("$1 + B + \\cdots + B^{%i}$", k))
  }
  keep <- abs(x) > tol
  x <- x[keep]
  degree <- degree[keep]
  if (length(x) == 0)
    return("$0$")
  coefs <- format_num(abs(x), digits = digits, decimal.mark = decimal.mark)
  # remove useless decimals for integers
  is_int <- abs(abs(x) - round(abs(x))) < tol
  coefs[is_int] <- as.character(round(abs(x[is_int])))
  if (!identical(decimal.mark, "."))
    coefs <- gsub(decimal.mark, sprintf("{%s}", decimal.mark), coefs, fixed = TRUE)
  b_part <- ifelse(degree == 0, "",
                   ifelse(degree == 1, "B", sprintf("B^{%i}", degree)))
  coefs[degree > 0 & is_int & round(abs(x)) == 1] <- ""
  terms <- paste0(coefs, b_part)
  signs <- ifelse(x < 0, "-", "+")
  res <- paste0(ifelse(signs[1] == "-", "-", ""), terms[1])
  if (length(terms) > 1)
    res <- paste(c(res, paste(signs[-1], terms[-1])), collapse = " ")
  sprintf("$%s$", res)
}
