# Estimated patterns and anticipated movements of sc_dashboard():
# trading days, moving holidays and seasonal patterns share the same structure:
# - `estimated_values`: effect of each day/period (in %);
# - `evolution`: effect on the previous, current and next period (in %).
#
# With rjd3 (>= 3.9), the components of multiplicative models (tde, mhe, s) are ratios
# and those of additive models are in the unit of the series: they are converted to ratios
# using the mean of the raw series.

period_name <- function(freq) {
  if (freq == 12) "Month" else if (freq == 4) "Quarter" else "Period"
}
period_labels <- function(freq, cycle) {
  if (freq == 12) {
    month.abb[cycle]
  } else if (freq == 4) {
    sprintf("Q%i", cycle)
  } else {
    paste0("P", cycle)
  }
}
direction <- function(x) {
  ifelse(x > 0, "increase", "decrease")
}

# Convert a component to ratios
as_ratio <- function(x, is_multiplicative, series_mean) {
  if (is_multiplicative) x else x / series_mean + 1
}

new_pattern <- function(name, estimated_values, evolution_ratio, freq) {
  evolution <- NULL
  if (!is.null(evolution_ratio)) {
    evolution <- data.frame(
      Evolution = (as.numeric(evolution_ratio) - 1) * 100,
      row.names = sprintf("%s %s", c("Previous", "Current", "Next"), period_name(freq))
    )
  }
  structure(list(name = name, estimated_values = estimated_values, evolution = evolution),
            class = c("sc_pattern", "list"))
}

# Pattern of a component over the last year (moving holidays and seasonal component)
component_pattern <- function(x, name, cmp) {
  data <- rjd3toolkit::user_defined(x, c(cmp, paste0(cmp, "_f"), "y", "log"))
  values <- data[[cmp]]
  freq <- frequency(data[["y"]])
  if (is.null(values) || all(is.na(values)))
    return(new_pattern(name, NULL, NULL, freq))
  is_multiplicative <- as.logical(data[["log"]])
  series_mean <- mean(data[["y"]], na.rm = TRUE)
  values <- as_ratio(values, is_multiplicative, series_mean)
  fcst <- as_ratio(data[[paste0(cmp, "_f")]], is_multiplicative, series_mean)

  last_year <- window(values, start = time(values)[length(values)] - 1 + deltat(values))
  estimated_values <- (as.numeric(last_year) - 1) * 100
  names(estimated_values) <- period_labels(freq, cycle(last_year))
  evolution <- c(tail(values, 2), head(fcst, 1))
  new_pattern(name, estimated_values, evolution, freq)
}

moving_holiday_pattern <- function(x) {
  component_pattern(x, "Moving Holiday", "mhe")
}
seasonal_pattern <- function(x) {
  component_pattern(x, "Seasonal", "s")
}

trading_day_pattern <- function(x) {
  data <- rjd3toolkit::user_defined(x, c("regression.ntd", "tde", "tde_f", "y", "log"))
  ntd <- data[["regression.ntd"]]
  freq <- frequency(data[["y"]])
  if (is.null(ntd) || ntd == 0) {
    return(new_pattern(
      "Trading Day",
      c(Monday = 0, Tuesday = 0, Wednesday = 0, Thursday = 0, Friday = 0, Saturday = 0, Contrast = 0),
      c(1, 1, 1), freq))
  }
  is_multiplicative <- as.logical(data[["log"]])
  series_mean <- mean(data[["y"]], na.rm = TRUE)
  coefs <- do.call(rbind,
                   lapply(sprintf("regression.td(%s)", seq_len(ntd)), rjd3toolkit::result, object = x))
  estimated_values <- coefs[, 1] * 100
  if (!is_multiplicative)
    estimated_values <- estimated_values / series_mean
  estimated_values <- c(estimated_values, Contrast = -sum(estimated_values))
  evolution <- c(tail(data[["tde"]], 2), head(data[["tde_f"]], 1))
  evolution <- as_ratio(evolution, is_multiplicative, series_mean)
  new_pattern("Trading Day", estimated_values, evolution, freq)
}

#' @exportS3Method NULL
plot.sc_pattern <- function(x, ...) {
  if (is.null(x$evolution)) {
    plot(0, type = "n", axes = FALSE, main = sprintf("%s Pattern", x$name))
    plot.new()
    return(invisible(NULL))
  }
  data_table <- round(x$evolution, 1)
  next_evol <- data_table$Evolution[3]
  title <- if (next_evol > 0) {
    bquote(.(sprintf("%s Effects: ", x$name)) %dblup% " expected")
  } else if (next_evol < 0) {
    bquote(.(sprintf("%s Effects: ", x$name)) %dbldown% " expected")
  } else {
    bquote(.(sprintf("%s Effects: ", x$name)) %=>% " expected")
  }
  data_table$Evolution <- sprintf("%+.1f%%", data_table$Evolution)

  mai <- par("mai")
  barplot(x$estimated_values, names.arg = names(x$estimated_values), ann = FALSE,
          main = sprintf("%s Pattern", x$name))
  par(mai = c(0, 0, 0, 0))
  plot(0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(0, 1))
  plotrix::addtable2plot(0.5, 0.5,
                         data_table[-3, , drop = FALSE], bty = "o", display.rownames = TRUE,
                         hlines = TRUE, display.colnames = FALSE,
                         vlines = TRUE, title = title, xjust = 0.5, yjust = 0.5)
  par(mai = mai)
}
