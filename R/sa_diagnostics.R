diagnostics_tests <- data.frame(
  id = c("mean", "skewness", "kurtosis", "doornikhansen", "lb", "seaslb", "lb2",
         "seas.qstest.sa", "seas.ftest.sa", "seas.qstest.i", "seas.ftest.i",
         "td.ftest.sa", "td.ftest.i"),
  label = c("Mean", "Skewness", "Kurtosis", "Normality (Doornik-Hansen)", "Ljung-Box",
            "Ljung-Box at seasonal lags", "Ljung-Box on squared residuals",
            "QS test on SA", "F-test on SA (seasonal dummies)",
            "QS test on I", "F-test on I (seasonal dummies)",
            "F-test on SA (trading days)", "F-test on I (trading days)"),
  group = c(rep("Residuals of the pre-processing", 7),
            rep("Residual seasonality", 4),
            rep("Residual trading days effects", 2)),
  stringsAsFactors = FALSE
)

#' @rdname sa_preprocessing
#' @param tests identifiers of the tests to print. By default all the available tests:
#' `"mean"`, `"skewness"`, `"kurtosis"`, `"doornikhansen"` (normality), `"lb"` (Ljung-Box),
#' `"seaslb"` (Ljung-Box at seasonal lags), `"lb2"` (Ljung-Box on squared residuals),
#' `"seas.qstest.sa"`, `"seas.ftest.sa"`, `"seas.qstest.i"`, `"seas.ftest.i"`
#' (residual seasonality on the seasonally adjusted series and on the irregular),
#' `"td.ftest.sa"` and `"td.ftest.i"` (residual trading days effects).
#' @param var_decomposition boolean indicating if the table with the variance decomposition
#' should be printed (only for seasonal adjustment models).
#' @export
sa_diagnostics <- function(x,
                           tests = diagnostics_tests$id,
                           digits = 3,
                           decimal.mark = getOption("OutDec"),
                           signif_stars = TRUE,
                           var_decomposition = TRUE,
                           title_level = NULL) {
  r <- get_rslts(x)
  tests <- match.arg(tests, diagnostics_tests$id, several.ok = TRUE)
  all_tests <- c(r$rslts$preprocessing$diagnostics, r$rslts$diagnostics)
  def <- diagnostics_tests[diagnostics_tests$id %in% tests &
                             diagnostics_tests$id %in% names(all_tests), ]
  elements <- list(md_title("Diagnostics", level = title_level))
  if (nrow(def) > 0) {
    stat <- vapply(def$id, function(id) test_value(all_tests[[id]]), numeric(1))
    pval <- vapply(def$id, function(id) test_pvalue(all_tests[[id]]), numeric(1))
    tab <- data.frame(
      Test = def$label,
      Statistic = format_num(stat, digits = digits, decimal.mark = decimal.mark),
      `P-value` = format_pval(pval, digits = digits, decimal.mark = decimal.mark),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    notes <- NULL
    align <- "lrr"
    if (signif_stars) {
      tab[[" "]] <- signif_stars(pval)
      notes <- signif_stars_note()
      align <- "lrrl"
    }
    tab_el <- md_table(tab, caption = "Diagnostics tests", notes = notes, align = align)
    # row groups
    groups <- unique(def$group)
    tab_el$groups <- stats::setNames(lapply(groups, function(g) match(g, def$group)), groups)
    elements <- c(elements, list(tab_el))
  }
  var_decomp <- r$rslts$diagnostics$vardecomposition
  if (var_decomposition && !is.null(var_decomp)) {
    var_decomp <- unlist(var_decomp)
    labels <- c(cycle = "Trend-cycle", seasonal = "Seasonal", irregular = "Irregular",
                calendar = "Calendar", others = "Others", total = "Total")
    var_decomp <- var_decomp[names(var_decomp) %in% names(labels)]
    tab_var <- data.frame(
      Component = labels[names(var_decomp)],
      `Contribution (%)` = format_num(100 * var_decomp, digits = max(digits - 2, 1),
                                      decimal.mark = decimal.mark),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    elements <- c(elements, list(md_table(
      tab_var, caption = "Variance decomposition",
      notes = "Relative contribution of the components to the stationary portion of the variance. Others: pre-adjustment (outliers and other regression effects).",
      align = "lr"
    )))
  }
  new_block(elements, class = "rjd3report_diagnostics")
}

test_value <- function(x) {
  if (is.null(x)) return(NA_real_)
  v <- if (!is.null(x$value)) x$value else x[[1]]
  as.numeric(v)
}
test_pvalue <- function(x) {
  if (is.null(x)) return(NA_real_)
  v <- if (!is.null(x$pvalue)) x$pvalue else x[[2]]
  as.numeric(v)
}
