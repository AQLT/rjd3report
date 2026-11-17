summary_diagnostics <- function(x, digits = 2, decimal.mark = getOption("OutDec")) {
  m7 <- rjd3toolkit::result(x, "m-statistics.m7")
  m7_status <- NULL
  if (!is.null(m7)) {
    m7_status <- if (m7 < 1.25) "good" else if (m7 < 1.75) "uncertain" else "bad"
    names(m7) <- "Adjustability (M7)"
  }
  # residuals tests
  liste_ind <- c(
    "Residual Seasonality (qs-test on sa)" = "diagnostics.seas-sa-qs",
    "Residual Seasonality (f-test on sa)" = "diagnostics.seas-sa-f",
    "Residual Trading-days effects (f-test on sa)" = "diagnostics.td-sa-all",
    "ARIMA autocorrelation (lb test)" = "residuals.lb"
  )
  residuals_tests <- unlist(lapply(rjd3toolkit::user_defined(x, liste_ind), `[[`, "pvalue"))
  names(residuals_tests) <- names(liste_ind)[seq_along(residuals_tests)]

  recent_outlier <- c("Recent Outliers (current period)" = recent_outlier_status(x, 0),
                      "Recent Outliers (previous period)" = recent_outlier_status(x, 1))
  value <- c(format_num(c(m7, residuals_tests), digits = digits, decimal.mark = decimal.mark),
             c(good = "Regular", uncertain = "Outlier", bad = "Extreme")[recent_outlier])
  names(value) <- c(names(m7), names(residuals_tests), names(recent_outlier))
  status <- c(m7_status, status_residual(residuals_tests), recent_outlier)

  res <- data.frame(Value = value, Indicator = "", Status = status,
                    row.names = names(value), stringsAsFactors = FALSE)
  class(res) <- c("summary_diagnostics", class(res))
  res
}
# Status of the observation `nb_previous_periods` periods before the last one:
# "uncertain" if there is an outlier in the RegARIMA model, "bad" if it is an extreme value
# (X-13: table C17 equals to 0) and "good" otherwise.
recent_outlier_status <- function(x, nb_previous_periods = 0) {
  y <- rjd3toolkit::user_defined(x, "y")[[1]]
  date <- start(ts(0, start = tail(time(y), nb_previous_periods + 1)[1], frequency = frequency(y)))
  regression_var_names <- rjd3toolkit::result(x, "regression.description")
  out_names <- grep("^(AO|LS|TC|SO) \\(", regression_var_names, value = TRUE)
  is_outlier <- FALSE
  if (length(out_names) > 0) {
    dates_out <- outliers_to_dates(out_names)
    is_outlier <- any(dates_out$year == date[1] & dates_out$period == date[2])
  }
  c17 <- rjd3toolkit::user_defined(x, "decomposition.c17")[[1]]
  is_extreme <- !is.null(c17) && isTRUE(all.equal(window(c17, start = date)[1], 0))
  if (is_outlier) {
    "uncertain"
  } else if (is_extreme) {
    "bad"
  } else {
    "good"
  }
}
#'@exportS3Method NULL
plot.summary_diagnostics <- function(x, colors = dashboard_colors(), ...){
  bg_color <- cbind("white", status2color(x$Status, colors))

  row_names <- rownames(x)
  max_str_width <- max(strwidth(row_names))
  row_names <- sapply(row_names, function(n){
    while (strwidth(n) < max_str_width) {
      n <- paste0(" ", n)
    }
    n
  })

  table <- x[, 1:2]
  rownames(table) <- row_names
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE)
  plotrix::addtable2plot(0.5, 0.5,
                         table, bty = "o", display.rownames = TRUE, hlines = TRUE,
                         vlines = TRUE,bg = bg_color, xjust = 0.5, yjust = 0.5)
  title("Summary of Key Diagnostics", line = -3)
}

#' @importFrom graphics arrows axis barplot box layout legend lines mtext par plot plot.new strwidth text title points
#' @importFrom stats cor cycle deltat frequency lag na.omit pchisq pt sd start time ts ts.union window
#' @importFrom utils as.roman head read.csv tail globalVariables head tail
utils::globalVariables(c("TD1", "TD2", "TD3", "TD4", "TD5", "TD6", "TD7",
                         "phi1", "phi2", "phi3", "phi4", "phi5", "phi6",
                         "regarima_coefs","theta1", "theta2", "theta3", "theta4", "theta5", "theta6"))

