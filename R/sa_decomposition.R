#' @rdname sa_preprocessing
#' @export
sa_decomposition <- function(x,
                             digits = 3,
                             decimal.mark = getOption("OutDec"),
                             title_level = NULL) {
  r <- get_rslts(x)
  if (is.null(r$rslts$decomposition))
    stop("No decomposition in the model.", call. = FALSE)
  if (identical(r$method, "X13")) {
    x11_decomposition(r, digits = digits, decimal.mark = decimal.mark,
                      title_level = title_level)
  } else {
    seats_decomposition(r, digits = digits, decimal.mark = decimal.mark,
                        title_level = title_level)
  }
}

decomposition_mode <- function(r) {
  mode <- tryCatch(r$spec$x11$mode, error = function(e) NULL)
  if (is.null(mode) || mode %in% c("UNKNOWN", "UNDEFINED")) {
    mode <- if (isTRUE(r$rslts$preprocessing$description$log)) "multiplicative" else "additive"
  }
  tolower(gsub("_", "-", mode))
}

x11_decomposition <- function(r, digits = 3, decimal.mark = getOption("OutDec"),
                              title_level = NULL) {
  decomp <- r$rslts$decomposition
  mstats <- unlist(r$rslts$mstats)
  legend_mstats <- data.frame(
    label = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "q", "qm2"),
    Statistic = c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "M-7", "M-8", "M-9",
                  "M-10", "M-11", "Q", "Q-M2"),
    Description = c(
      "The relative contribution of the irregular over three months span",
      "The relative contribution of the irregular component to the stationary portion of the variance",
      "The amount of period to period change in the irregular component as compared to the amount of period to period change in the trend",
      "The amount of autocorrelation in the irregular as described by the average duration of run",
      "The number of periods it takes the change in the trend to surpass the amount of change in the irregular",
      "The amount of year to year change in the irregular as compared to the amount of year to year change in the seasonal",
      "The amount of moving seasonality present relative to the amount of stable seasonality",
      "The size of the fluctuations in the seasonal component throughout the whole series",
      "The average linear movement in the seasonal component throughout the whole series",
      "The size of the fluctuations in the seasonal component in the recent years",
      "The average linear movement in the seasonal component in the recent years",
      "Weighted average of the M-statistics",
      "Q-statistic without M-2"),
    stringsAsFactors = FALSE
  )
  legend_mstats <- legend_mstats[legend_mstats$label %in% names(mstats), ]
  tab <- data.frame(
    Statistic = legend_mstats$Statistic,
    Value = format_num(mstats[legend_mstats$label], digits = digits, decimal.mark = decimal.mark),
    Description = legend_mstats$Description,
    stringsAsFactors = FALSE
  )
  s_filter <- decomp$final_seasonal
  s_filter <- if (is.null(s_filter)) "?" else gsub("^FILTER_S", "", s_filter)
  s_filter <- tolower(s_filter)
  filters <- sprintf("Final filters: seasonal %s, Henderson %s terms",
                     s_filter, decomp$final_henderson)
  elements <- list(
    md_title("Decomposition (X-11)", level = title_level),
    md_text(sprintf("Mode: %s", decomposition_mode(r))),
    md_text(filters),
    md_table(tab, caption = "M-statistics",
             notes = "The M-statistics range from 0 to 3, values lower than 1 are considered as acceptable.",
             align = "lrl", width = c(0.12, 0.08, 0.8))
  )
  new_block(elements, class = "rjd3report_decomposition")
}

seats_decomposition <- function(r, digits = 3, decimal.mark = getOption("OutDec"),
                                title_level = NULL) {
  ucm <- r$rslts$decomposition$canonicaldecomposition
  models <- c(list(ucm$model), ucm$components, ucm$complements)
  labels <- c(model = "Model", trend = "Trend", seasonal = "Seasonal", transitory = "Transitory",
              irregular = "Irregular", seasonallyadjusted = "Seasonally adjusted")
  names_models <- vapply(models, function(m) m$name, character(1))
  # remove components with null variance
  keep <- vapply(models, function(m) is.null(m$var) || m$name == "model" || m$var > 0, logical(1))
  models <- models[keep]
  names_models <- names_models[keep]
  rows <- vapply(seq_along(models), function(i) {
    m <- models[[i]]
    lab <- if (names_models[i] %in% names(labels)) labels[names_models[i]] else names_models[i]
    sprintf("| %s | %s | %s | %s | %s |",
            lab,
            format_polynomial(m$ar, digits = digits, decimal.mark = decimal.mark),
            format_polynomial(m$delta, digits = digits, decimal.mark = decimal.mark),
            format_polynomial(m$ma, digits = digits, decimal.mark = decimal.mark),
            format_num(m$var, digits = digits, decimal.mark = decimal.mark))
  }, character(1))
  table_md <- paste(c(
    "| Component | AR | Differencing | MA | Innovation variance |",
    "|:--|:--|:--|:--|--:|",
    rows,
    "",
    ": SEATS canonical decomposition (polynomials in the backshift operator $B$)"
  ), collapse = "\n")
  elements <- list(
    md_title("Decomposition (SEATS)", level = title_level),
    md_text(sprintf("Mode: %s", decomposition_mode(r))),
    md_text(table_md)
  )
  new_block(elements, class = "rjd3report_decomposition")
}
