#' Compute data for a simple seasonal adjustment
#'
#' Functions to compute the data to produce a simple seasonal adjustment dashboard.
#' `simple_dashboard2()` is a shortcut of `simple_dashboard(n_last_outliers = 4)`: the
#' description text is smaller to include a table with the last outliers.
#' The dashboard can be plotted with base R ([plot.simple_dashboard()]) or with \pkg{ggplot2}
#' ([autoplot.simple_dashboard()]).
#'
#' @param x A seasonal adjustment model.
#' @param context Context used to estimate the model.
#' @param digits Number of digits used in the tables (default value used by the plot functions).
#' @param scale_var_decomp boolean indicating if the variance decomposition table should be rescaled to 100.
#' @param remove_others_contrib boolean indication if the "Others" contribution (i.e.: the pre-adjustment contribution)
#' should be removed from the variance decomposition table.
#' @param add_obs_to_forecast Boolean indicating if the last observed values should be added to the forecast table (for the plot).
#' @param td_effect Boolean indicating if the residual trading days effect test should be printed.
#' By default (`td_effect = NULL`) the test is only printed for monthly series.
#' @param n_last_outliers number of last outliers to be printed (by default `n_last_outliers = 0` for
#' `simple_dashboard()` and `n_last_outliers = 4` for `simple_dashboard2()`).
#' @param digits_outliers number of digits used in the table of outliers.
#' @param columns_outliers informations about outliers that should be printed in the summary table.
#' Can be either a vector of characters among `c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")`;
#' or an vector of integer: `1` corresponding to the estimate coefficient (`"Estimate"`),
#' `2` corresponding to the standard deviation error (`"Std. Error"`),
#' `3` corresponding to the t-statistic (`"T-stat"`) or
#' `4` corresponding to the p-value (`"Pr(>|t|)"`).
#' By default only the estimate coefficients and the t-statistics are printed
#' (`columns_outliers = c("Estimate", "T-stat")`).
#' @param order_outliers order of the outliers in case of several outliers at the same date.
#' @param ... other parameters passed to `simple_dashboard()`.
#'
#' @details
#' The colors of the tables depend on the status of each statistic (see [dashboard_colors()]):
#' - Q-statistics (X-13 only): `"good"` if lower than 1 and `"bad"` otherwise;
#' - seasonality tests on the linearised series: `"good"` if the p-value is lower than 0.01
#' (seasonality detected), `"uncertain"` if it is between 0.01 and 0.05 and `"bad"` otherwise;
#' - residual seasonality and residual trading days effects tests: `"bad"` if the p-value is lower than 0.01,
#' `"uncertain"` if it is between 0.01 and 0.05 and `"good"` otherwise.
#' For the combined seasonality tests, `"Present"` (seasonality detected) is `"good"` on the linearised
#' series and `"bad"` on the seasonally adjusted series.
#'
#' @examples
#' data <- window(rjd3toolkit::ABS$X0.2.09.10.M, start = 2003)
#' sa_model <- rjd3x13::x13(data)
#' dashboard_data <- simple_dashboard(sa_model)
#' plot(dashboard_data, main = "Simple dashboard")
#' dashboard_data2 <- simple_dashboard2(sa_model)
#' plot(dashboard_data2, main = "Simple dashboard with outliers")
#' # With ggplot2
#' autoplot(dashboard_data2, main = "Simple dashboard with outliers")
#' @importFrom stats ts.union
#' @importFrom utils tail
#' @export
simple_dashboard <- function(x, context = NULL, digits = 2,
                             scale_var_decomp = FALSE,
                             remove_others_contrib = FALSE,
                             add_obs_to_forecast = TRUE,
                             td_effect = NULL,
                             n_last_outliers = 0,
                             digits_outliers = digits,
                             columns_outliers = c("Estimate", "T-stat"),
                             order_outliers = c("AO", "LS", "TC", "SO")) {
  x <- get_jmod(x, context = context)
  if (is.null(x))
    return(NULL)
  series <- sa_series(x, add_obs_to_forecast = add_obs_to_forecast)
  last_date <- format(tail(time_to_date(ggdemetra3::raw(x)), 1), format = "%Y-%m")
  if (is.null(td_effect))
    td_effect <- frequency(series) == 12

  res <- list(series = series,
              siratio = ggdemetra3::siratio(x),
              summary = dashboard_summary(x),
              decomposition = dashboard_decomposition(x,
                                                      scale_var_decomp = scale_var_decomp,
                                                      remove_others_contrib = remove_others_contrib),
              tests = dashboard_tests(x, td_effect = td_effect),
              outliers = dashboard_outliers(x, n_last_outliers = n_last_outliers,
                                            columns_outliers = columns_outliers,
                                            order_outliers = order_outliers),
              show_outliers = n_last_outliers > 0,
              last_date = last_date,
              digits = digits,
              digits_outliers = digits_outliers)
  class(res) <- c("simple_dashboard")
  res
}

#' @rdname simple_dashboard
#' @export
simple_dashboard2 <- function(x, ..., n_last_outliers = 4) {
  simple_dashboard(x, ..., n_last_outliers = n_last_outliers)
}

# Raw series, trend and seasonally adjusted series (with forecasts)
sa_series <- function(x, add_obs_to_forecast = TRUE) {
  data <- list(y = ggdemetra3::raw(x),
               t = ggdemetra3::trendcycle(x),
               sa = ggdemetra3::seasonaladj(x),
               y_f = ggdemetra3::raw(x, forecast = TRUE),
               t_f = ggdemetra3::trendcycle(x, forecast = TRUE),
               sa_f = ggdemetra3::seasonaladj(x, forecast = TRUE))
  data <- data[!vapply(data, function(s) is.null(s) || length(s) == 0, logical(1))]
  data <- do.call(ts.union, data)
  # add observed data for plots
  if (add_obs_to_forecast && all(c("y_f", "t_f", "sa_f") %in% colnames(data))) {
    i_last <- which(is.na(data[, "y"]))[1] - 1
    if (!is.na(i_last) && i_last > 0)
      data[i_last, c("y_f", "t_f", "sa_f")] <- data[i_last, c("y", "t", "sa")]
  }
  data
}

dashboard_summary <- function(x) {
  list(
    span_start = span2text(rjd3toolkit::result(x, "regression.espan.start")),
    span_end = span2text(rjd3toolkit::result(x, "regression.espan.end")),
    nobs = rjd3toolkit::result(x, "regression.espan.n"),
    log = rjd3toolkit::result(x, "log") != 0,
    ntd = rjd3toolkit::result(x, "regression.ntd"),
    easter = !is.null(rjd3toolkit::result(x, "nmh")),
    nout = rjd3toolkit::result(x, "regression.nout"),
    arima = sarima_orders_ch(x)
  )
}

dashboard_decomposition <- function(x, scale_var_decomp = FALSE, remove_others_contrib = FALSE) {
  qstats <- unlist(rjd3toolkit::user_defined(x, c("m-statistics.q", "m-statistics.q-m2")))
  if (length(qstats) == 2) {
    names(qstats) <- c("Q", "Q-M2")
  } else {
    qstats <- NULL # TRAMO-SEATS
  }
  var_decomp <- unlist(rjd3toolkit::user_defined(
    x,
    c("variancedecomposition.cycle",
      "variancedecomposition.seasonality",
      "variancedecomposition.irregular",
      "variancedecomposition.tdh",
      "variancedecomposition.others",
      "variancedecomposition.total")))
  names(var_decomp) <- c("Cycle", "Seasonal", "Irregular", "TDH", "Others", "Total")
  if (remove_others_contrib) {
    var_decomp <- var_decomp[-5]
    i_total <- length(var_decomp)
    var_decomp[i_total] <- sum(var_decomp[-i_total])
  }
  if (scale_var_decomp) {
    i_total <- length(var_decomp)
    var_decomp[-i_total] <- var_decomp[-i_total] / sum(var_decomp[-i_total])
    var_decomp[i_total] <- 1
  }
  list(qstats = qstats, var_decomp = var_decomp * 100)
}

dashboard_tests <- function(x, td_effect = TRUE) {
  tests <- c("F-test" = "f", "QS-test" = "qs", "Kruskal-Wallis" = "kw", "Friedman" = "friedman")
  get_pval <- function(id) {
    res <- rjd3toolkit::user_defined(x, id)[[1]]
    if (is.list(res)) res$pvalue else NA_real_
  }
  pvalues <- rbind(
    "Seasonality" = vapply(sprintf("diagnostics.seas-lin-%s", tests), get_pval, numeric(1)),
    "Residual Seasonality" = vapply(sprintf("diagnostics.seas-sa-%s", tests), get_pval, numeric(1))
  )
  colnames(pvalues) <- names(tests)
  combined <- c("Seasonality" = rjd3toolkit::user_defined(x, "diagnostics.seas-lin-combined")[[1]],
                "Residual Seasonality" = rjd3toolkit::user_defined(x, "diagnostics.seas-sa-combined")[[1]])
  combined[vapply(combined, is.null, logical(1))] <- NA_character_
  combined <- unlist(combined)
  td <- if (td_effect) rjd3toolkit::result(x, "diagnostics.td-sa-last")$pvalue else NULL
  list(pvalues = pvalues, combined = combined, residual_td = td)
}

dashboard_outliers <- function(x, n_last_outliers = 4,
                               columns_outliers = c("Estimate", "T-stat"),
                               order_outliers = c("AO", "LS", "TC", "SO")) {
  all_columns <- c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")
  if (is.numeric(columns_outliers)) {
    columns_outliers <- all_columns[columns_outliers]
  } else {
    columns_outliers <- match.arg(columns_outliers, all_columns, several.ok = TRUE)
  }
  nout <- rjd3toolkit::result(x, "regression.nout")
  if (n_last_outliers <= 0 || is.null(nout) || nout == 0)
    return(NULL)
  outliers <- do.call(rbind, rjd3toolkit::user_defined(x, sprintf("regression.out(%i)", seq_len(nout))))
  colnames(outliers) <- all_columns[seq_len(ncol(outliers))]
  # sort outliers by dates (most recent first)
  dates_out <- outliers_to_dates(rownames(outliers))
  type <- factor(dates_out$type, levels = order_outliers)
  outliers <- outliers[order(dates_out$year, dates_out$period, -as.integer(type), decreasing = TRUE), , drop = FALSE]
  outliers <- outliers[seq_len(min(n_last_outliers, nrow(outliers))), columns_outliers, drop = FALSE]
  data.frame(Outlier = rownames(outliers), outliers, check.names = FALSE, row.names = NULL)
}

# Parse the names of the outliers: "AO (2000-07)", "AO (2000-07-01)", "AO (2000-Q2)" or "AO (VII-2000)"
outliers_to_dates <- function(name_out) {
  types <- gsub(" .*", "", name_out)
  dates <- gsub("^.*\\((.*)\\).*$", "\\1", name_out)
  year <- period <- rep(NA_real_, length(dates))
  iso <- grepl("^\\d{4}-", dates)
  if (any(iso)) {
    parts <- strsplit(dates[iso], "-")
    year[iso] <- as.numeric(vapply(parts, `[`, character(1), 1))
    period[iso] <- as.numeric(gsub("\\D", "", vapply(parts, `[`, character(1), 2)))
  }
  if (any(!iso)) {
    parts <- strsplit(dates[!iso], "-")
    period[!iso] <- as.numeric(utils::as.roman(vapply(parts, `[`, character(1), 1)))
    year[!iso] <- as.numeric(vapply(parts, `[`, character(1), 2))
  }
  data.frame(year = year, period = period, type = types)
}
