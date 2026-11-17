#' Report on the pre-processing, the decomposition and the diagnostics
#'
#' Functions to describe a seasonal adjustment model in a report:
#'
#' - `sa_preprocessing()` describes the pre-processing (RegARIMA or TRAMO model):
#' summary of the model, likelihood statistics, ARIMA and regression coefficients;
#' - `sa_decomposition()` describes the decomposition (X-11 or SEATS);
#' - `sa_diagnostics()` prints a table with the main diagnostics tests on the quality of the
#' pre-processing and of the decomposition.
#'
#' The functions return objects that are automatically rendered in a Quarto (or R Markdown)
#' document, whatever the output format (HTML, PDF with LaTeX or Typst, Word...): it is no
#' longer necessary to specify the output format nor to use the chunk option `results: asis`.
#' The tables are built with \pkg{tinytable}.
#' The objects can also be printed in the console.
#'
#' @param x a seasonal adjustment model: result of [rjd3x13::x13()], [rjd3tramoseats::tramoseats()],
#' [rjd3x13::jx13()], [rjd3tramoseats::jtramoseats()] or a SA-item read from a workspace
#' (see [rjd3workspace::read_sai()] or [rjd3workspace::read_workspace()]).
#' The pre-processing models ([rjd3x13::regarima()] and [rjd3tramoseats::tramo()]) are also supported by
#' `sa_preprocessing()` and `sa_diagnostics()`.
#' @param digits number of digits used in the tables and in the text.
#' @param decimal.mark the character used as decimal mark.
#' @param signif_stars boolean indicating if the significance stars should be added to the tables.
#' @param title_level `NULL` (default) to print the titles in bold or an integer to print
#' them as markdown headings of that level (e.g. `title_level = 3` for `### Title`).
#' @param summary,likelihood,arima,regression booleans indicating which parts of the
#' pre-processing description should be printed.
#'
#' @return An object of class `"rjd3report_block"` (see [print.rjd3report_block()]).
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' sa_x13 <- rjd3x13::x13(y)
#' sa_preprocessing(sa_x13)
#' sa_decomposition(sa_x13)
#' sa_diagnostics(sa_x13)
#'
#' sa_ts <- rjd3tramoseats::tramoseats(y)
#' sa_decomposition(sa_ts)
#'
#' # In a Quarto document, just write in a chunk:
#' # sa_preprocessing(sa_x13, title_level = 3)
#' @seealso [sa_report()], [render_report()].
#' @export
sa_preprocessing <- function(x,
                             digits = 3,
                             decimal.mark = getOption("OutDec"),
                             signif_stars = TRUE,
                             title_level = NULL,
                             summary = TRUE,
                             likelihood = TRUE,
                             arima = TRUE,
                             regression = TRUE) {
  r <- get_rslts(x)
  prep <- r$rslts$preprocessing
  if (is.null(prep))
    stop("No pre-processing in the model.", call. = FALSE)
  sum_prep <- summary(prep)
  fnum <- function(x) format_num(x, digits = digits, decimal.mark = decimal.mark)

  elements <- list(md_title(sprintf("Pre-processing (%s)", preprocessing_name(r$method)),
                            level = title_level))
  sub_level <- if (is.null(title_level)) NULL else title_level + 1

  xregs <- sum_prep$xregs
  if (is.null(xregs))
    xregs <- data.frame()
  var_types <- vapply(prep$description$variables, `[[`, character(1), "type")
  var_names <- vapply(prep$description$variables, `[[`, character(1), "name")

  if (summary) {
    elements <- c(elements, list(md_title("Summary", level = sub_level)))
    span <- series_span(r)
    if (!is.null(span)) {
      elements <- c(elements, list(md_text(sprintf("Series span: from %s to %s (%s)",
                                                   span[1], span[2],
                                                   plural(sum_prep$likelihood$nobs, "observation")))))
    } else {
      elements <- c(elements, list(md_text(plural(sum_prep$likelihood$nobs, "observation"))))
    }
    elements <- c(elements, list(md_text(
      if (isTRUE(prep$description$log)) "Series has been log-transformed" else "Series hasn't been transformed"
    )))
    n_td <- sum(vapply(prep$description$variables[var_types == "TD"],
                       function(v) nrow(v$coef), integer(1)))
    elements <- c(elements, list(md_text(
      if (n_td > 0) sprintf("Trading days effect (%s)", plural(n_td, "variable")) else "No trading days effect"
    )))
    if ("LP" %in% var_types)
      elements <- c(elements, list(md_text("Leap year effect")))
    elements <- c(elements, list(md_text(
      if ("EASTER" %in% var_types) "Easter effect" else "No easter effect"
    )))
    out_names <- var_names[var_types %in% c("AO", "LS", "TC", "SO")]
    pre_out <- prespecified_outliers(r)
    n_pre <- sum(out_names %in% pre_out)
    n_det <- length(out_names) - n_pre
    if (n_pre > 0)
      elements <- c(elements, list(md_text(sprintf("%s", plural(n_pre, "pre-specified outlier")))))
    elements <- c(elements, list(md_text(sprintf("%s", plural(n_det, "detected outlier")))))
  }

  if (likelihood) {
    ll <- sum_prep$likelihood
    elements <- c(elements, list(
      md_title("Likelihood statistics", level = sub_level),
      md_text(sprintf("Number of effective observations = %s", ll$neffectiveobs)),
      md_text(sprintf("Number of estimated parameters = %s", ll$nparams)),
      md_text(sprintf("Loglikelihood = %s, AICc = %s, BIC = %s",
                      fnum(ll$ll), fnum(ll$aicc), fnum(ll$bic))),
      md_text(sprintf("Standard error of the regression (ML estimate) = %s", fnum(ll$se)))
    ))
  }

  if (arima) {
    orders <- sum_prep$sarima$sarima_orders
    arima_model <- sprintf("ARIMA (%s)(%s)",
                           paste(unlist(orders[c("p", "d", "q")]), collapse = ","),
                           paste(unlist(orders[c("bp", "bd", "bq")]), collapse = ","))
    elements <- c(elements, list(
      md_title("ARIMA model", level = sub_level),
      md_text(arima_model)
    ))
    coef_arima <- sum_prep$sarima$coef_table
    if (!is.null(coef_arima) && nrow(coef_arima) > 0) {
      elements <- c(elements, list(
        coef_table(coef_arima, caption = "ARIMA coefficients",
                   digits = digits, decimal.mark = decimal.mark,
                   signif_stars = signif_stars)
      ))
    }
  }

  if (regression && nrow(xregs) > 0) {
    elements <- c(elements, list(
      md_title("Regression model", level = sub_level),
      coef_table(xregs, caption = "Regression coefficients",
                 digits = digits, decimal.mark = decimal.mark,
                 signif_stars = signif_stars)
    ))
  }
  new_block(elements, class = "rjd3report_preprocessing")
}

# Table of coefficients from the summary() of rjd3 objects
coef_table <- function(x, caption, digits = 3, decimal.mark = getOption("OutDec"),
                       signif_stars = TRUE) {
  fixed <- !is.null(x$Type) & x$Type == "FIXED"
  pval <- x[["Pr(>|t|)"]]
  tab <- data.frame(
    Variable = rownames(x),
    Coefficient = format_num(x$Estimate, digits = digits, decimal.mark = decimal.mark),
    `Std. Error` = format_num(x[["Std. Error"]], digits = digits, decimal.mark = decimal.mark),
    `T-stat` = format_num(x[["T-stat"]], digits = digits, decimal.mark = decimal.mark),
    `P(>|t|)` = format_pval(pval, digits = digits, decimal.mark = decimal.mark),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  tab[fixed, c("Std. Error", "T-stat", "P(>|t|)")] <- ""
  if (any(fixed))
    tab$Variable[fixed] <- paste(tab$Variable[fixed], "(fixed)")
  notes <- NULL
  align <- c("l", "r", "r", "r", "r")
  if (signif_stars) {
    tab[[" "]] <- signif_stars(pval)
    notes <- signif_stars_note()
    align <- c(align, "l")
  }
  md_table(tab, caption = caption, notes = notes, align = paste(align, collapse = ""))
}

series_span <- function(r) {
  y <- switch(r$method,
              "X13" = r$rslts$preadjust$a1,
              "TRAMO-SEATS" = r$rslts$final$series$data,
              NULL)
  if (is.null(y) || !stats::is.ts(y))
    return(NULL)
  dates <- time_to_date(y)
  format(dates[c(1, length(dates))], "%Y-%m")
}

prespecified_outliers <- function(r) {
  spec <- r$spec
  if (is.null(spec))
    return(character())
  out <- tryCatch({
    if (!is.null(spec$regarima)) spec$regarima$regression$outliers else spec$tramo$regression$outliers
  }, error = function(e) NULL)
  if (length(out) == 0)
    return(character())
  vapply(out, function(o) if (is.null(o$name)) "" else o$name, character(1))
}
