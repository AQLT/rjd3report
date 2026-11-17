#' Compute data for the Statistics Canada seasonal adjustment dashboard
#'
#' Function to compute the data to produce the Statistics Canada seasonal adjustment dashboard
#'
#' @inheritParams simple_dashboard
#' @param n_recent_obs number of observation in the recent history panel (see details). By default \code{n_recent_obs = 24} (last 2 years for monthly data).
#'
#' @details
#' \code{sa_model()} reproduces Statistics Canada dashboard used to provide a snapshot of an single seasonal adjustment model at a point in time and to point out some possible problems (see references).
#'
#' The dashboard is divided into four sections:
#' \itemize{
#' \item Recent History (top left panel): plot of the raw series, the seasonal adjusted series and the trend for the most recent periods (\code{n_recent_obs} last observations: 24 by default).  It is intended to identify trend direction, overall volatility and obvious outliers.
#' \item Summary of Key Diagnostics (top right panel):
#' \itemize{
#' \item Adjustability (only for X13 models): M7 statistic. Colors: red if M7 \eqn{\ge} 1.75, yellow if 1.25 \eqn{\le} M7 < 1.75 and green if M7 < 1.25.
#' \item Residual seasonality: qs (auto-correlations at seasonal lags) and f (Friedman) test on seasonal adjusted series. Colors: red if p-value \eqn{\le} 0.01, yellow if 0.01 < p-value \eqn{\le} 0.05 and green if p-value > 0.05.
#' \item Residual trading-days effects: f (Friedman) test on seasonal adjusted serie.  Colors: red if p-value \eqn{\le} 0.01, yellow if 0.01 < p-value \eqn{\le} 0.05 and green if p-value > 0.05.
#' \item Independence of RegARIMA residuals: Ljung-Box test. Colors: red if p-value \eqn{\le} 0.01, yellow if 0.01 < p-value \eqn{\le} 0.05 and green if p-value > 0.05.
#' \item Recent outliers on last (t) and penultimate (t-1) observation. Colors: Red (`"bad"`) if there is an extreme value (only for X13: when table C17 equals to 0), yellow (`"uncertain"`) if there is an outlier in the RegARIMA model and green (`"good"`) otherwise.
#' }
#' \item Estimated Patterns and Anticipated Movements (middle panel): estimated trading day, moving holiday and seasonal pattern.  It presents expected movement in unadjusted series based on the current and previous period.
#' \item Net Effect of Seasonal Adjustment (bottom panel): movement in the raw series, compared to typical ranges centered around "neutral" value (when the seasonal adjusted series of the last period is equal to the penultimate period).  It also shows the movement in the seasonally adjusted series, compared to typical ranges.
#' }
#'
#' @references
#' KIRCHNER R., LADIRAY D., MAZZI G. L. (2018), "Quality Measures and Reporting for Seasonal Adjustment", edited by G. L. Mazzi, co-edited by D. Ladiray, European Union, Luxembourg. \url{https://ec.europa.eu/eurostat/web/products-manuals-and-guidelines/-/KS-GQ-18-001}
#'
#' MATTHEWS S. (2016), "Quality Assurance of Seasonal Adjustment for a Large System of Time Series", 36th International Symposium on Forecasting Santander, Spain.
#'
#' @examples
#' data <- window(rjd3toolkit::ABS$X0.2.09.10.M, start = 2003)
#' sa_model <- rjd3x13::x13(data)
#' sc_dashboard_data <- sc_dashboard(sa_model)
#' plot(sc_dashboard_data, main = "My first seasonal adjustment dashboard",
#'      subtitle = "SA with X13")
#'
#' @seealso \code{\link{plot.sc_dashboard}}.
#' @export
sc_dashboard <- function(x, n_recent_obs = 24, context = NULL){
  x <- get_jmod(x, context = context)

  last_date <- tail(time_to_date(ggdemetra3::raw(x)), 1)
  last_date <- format(last_date, format = "%Y-%m")
  res <- list(recent_history = recent_history(x, n_recent_obs = n_recent_obs),
              summary_diagnostics = summary_diagnostics(x),
              trading_day_pattern = trading_day_pattern(x),
              moving_holiday_pattern = moving_holiday_pattern(x),
              seasonal_pattern = seasonal_pattern(x),
              net_effect = net_effect(x), last_date = last_date)
  class(res) <- c("sc_dashboard")
  res
}

#' Plot a Statistics Canada seasonal adjustment dashboard
#'
#' Function to plot Statistics Canada dashboard of a seasonal adjustment model.
#'
#' @param x a \code{"sc_dashboard"} object.
#' @param main main title.
#' @param subtitle subtitle.
#' @param reference_date boolean indicating if the reference date should be printed (value of `x$last_date`).
#' @param raw_color color for the raw series.
#' @param sa_color color for the seasonal adjusted series.
#' @param trend_color color for the trend.
#' @param colors colors of the summary of key diagnostics according to the status of each statistic
#' (see [dashboard_colors()]).
#' @param ... other parameters (unused).
#'
#' @details
#' \code{sa_model()} reproduces Statistics Canada dashboard used to provide a snapshot of an single seasonal adjustment model at a point in time and to point out some possible problems (see references).
#'
#' The dashboard is divided into four sections:
#' \itemize{
#' \item Recent History (top left panel): plot of the raw series, the seasonal adjusted series and the trend for the most recent periods (\code{n_recent_obs} last observations: 24 by default).  It is intended to identify trendF direction, overall volatility and obvious outliers.
#' \item Summary of Key Diagnostics (top right panel):
#' \itemize{
#' \item Adjustability (only for X13 models): M7 statistic. Colors: red if M7 > 1.75, yellow if 1.25 < M7 < 1.75 and green if M7 < 1.25.
#' \item Residual seasonality: qs (auto-correlations at seasonal lags) and f (Friedman) test on seasonal adjusted series. Colors: red if p-value < 0.01, yellow if 0.01 < p-value < 0.05 and green if p-value > 0.05.
#' \item Residual trading-days effects: f (Friedman) test on seasonal adjusted serie.  Colors: red if p-value < 0.01, yellow if 0.01 < p-value < 0.05 and green if p-value > 0.05.
#' \item Independence of RegARIMA residuals: Ljung-Box test. Colors: red if p-value < 0.01, yellow if 0.01 < p-value < 0.05 and green if p-value > 0.05.
#' \item Recent outliers on last (t) and penultimate (t-1) observation. Colors: Red (`"bad"`) if there is an extreme value (only for X13: when table C17 equals to 0), yellow (`"uncertain"`) if there is an outlier in the RegARIMA model and green (`"good"`) otherwise.
#' }
#' \item Estimated Patterns and Anticipated Movements (middle panel): estimated trading day, moving holiday and seasonal pattern.  It presents expected movement in unadjusted series based on the current and previous period.
#' \item Net Effect of Seasonal Adjustment (bottom panel): movement in the raw series, compared to typical ranges centered around "neutral" value (when the seasonal adjusted series of the last period is equal to the penultimate period).  It also shows the movement in the seasonally adjusted series, compared to typical ranges.
#' }
#'
#' @references
#' KIRCHNER R., LADIRAY D., MAZZI G. L. (2018), "Quality Measures and Reporting for Seasonal Adjustment", edited by G. L. Mazzi, co-edited by D. Ladiray, European Union, Luxembourg. \url{https://ec.europa.eu/eurostat/web/products-manuals-and-guidelines/-/KS-GQ-18-001}
#'
#' MATTHEWS S. (2016), "Quality Assurance of Seasonal Adjustment for a Large System of Time Series", 36th International Symposium on Forecasting Santander, Spain.
#' @examples
#' data <- window(rjd3toolkit::ABS$X0.2.09.10.M, start = 2003)
#' sa_model <- rjd3x13::x13(data)
#' dashboard_data <- sc_dashboard(sa_model)
#' plot(dashboard_data, main = "My first seasonal adjustment dashboard",
#'      subtitle = "SA with X13")
#'
#' @seealso \code{\link{sc_dashboard}}.
#' @export
plot.sc_dashboard <- function(x, main = "Seasonal Adjustment Dashboard",
                              subtitle = "",
                              reference_date = TRUE,
                              raw_color = "#33A02C",
                              sa_color = "#E31A1C",
                              trend_color = "black",
                              colors = dashboard_colors(), ...){
    def.par <- par(no.readonly = TRUE)
    on.exit(par(def.par))
    layout(matrix(c(rep(1, 6),
                    rep(2, 3), rep(3, 3),
                    rep(4, 6),
                    rep(5, 2), rep(7, 2), rep(9, 2),
                    rep(6, 2), rep(8, 2), rep(10, 2),
                    rep(11, 6),
                    rep(12, 2), rep(13, 2), rep(14, 2)), 7, 6, byrow = TRUE),
           heights = c(0.3, 4, 0.3, 4, 1, 0.3, 4))
    base_dashboard_title(main)

    par(mai = c(0.5, 0.4, 0.4, 0.1))
    plot(x$recent_history, raw_color = raw_color,
         sa_color = sa_color,
         trend_color = trend_color)
    par(mai = c(0.0, 0, 0, 0.42))
    plot(x$summary_diagnostics, colors = colors)

    base_section_title("Estimated Patterns and Anticipated Movements")
    par(mai = c(0.4, 0.82, 0.5, 0.42))
    plot(x$trading_day_pattern)
    plot(x$moving_holiday_pattern)
    plot(x$seasonal_pattern)

    base_section_title("Net Effect of Seasonal Adjustment")
    par(mai = c(0.4, 0.4, 0.5, 0.2))
    plot(x$net_effect, raw_color = raw_color, sa_color = sa_color)
    base_dashboard_subtitle(subtitle, if (reference_date) x$last_date)
    invisible()
}
base_section_title <- function(title) {
    o.par <- par(mai = c(1.02, 0.82, 0.82, 0.42), mar = rep.int(0, 4))
    on.exit(par(o.par))
    plot.new()
    box()
    text(0.5, 0.5, title, font = 2, cex = 1.2)
}
