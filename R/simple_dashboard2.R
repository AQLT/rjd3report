

#' @param digits_outliers number of digits used in the table of outliers.
#' @param columns_outliers informations about outliers that should be printed in the summary table.
#' Can be either a vector of characters among `c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")`;
#' or an vector of integer: `1` corresponding to the estimate coefficient (`"Estimate"`),
#' `2` corresponding to the standard deviation error (`"Std. Error"`),
#' `3` corresponding to the t-statistic (`"T-stat"`) or
#' `4` corresponding to the p-value (`"Pr(>|t|)"`).
#' By default only the estimate coefficients and the t-statistics are printed
#' (`columns_outliers = c("Estimate", "T-stat")`).
#' @param n_last_outliers number of last outliers to be printed (by default `n_last_outliers = 4`).
#' @param order_outliers order of the outliers in case of several outliers at the same date.
#'
#'
#' @rdname simple_dashboard
#' @export
simple_dashboard2 <- function(x, context = NULL, digits = 2,
                              scale_var_decomp = FALSE,
                              remove_others_contrib = FALSE,
                              digits_outliers = digits,
                              columns_outliers = c("Estimate", "T-stat"),
                              n_last_outliers = 4,
                              order_outliers = c("AO", "LS", "TC", "SO"),
                              add_obs_to_forecast = TRUE) {
  x <- get_jmod(x, context = context)
  if (is.null(x))
    return(NULL)
  nb_format <- paste0("%.", digits, "f")
  if (is.numeric(columns_outliers)) {
    columns_outliers <- c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)")[columns_outliers]
  } else {
    columns_outliers <- match.arg(columns_outliers,
                                  c("Estimate", "Std. Error", "T-stat", "Pr(>|t|)"),
                                  several.ok = TRUE)
  }

  # Raw, trend, sa
  data_plot <- list(y = ggdemetra3::raw(x),
                    t = ggdemetra3::trendcycle(x),
                    sa = ggdemetra3::seasonaladj(x),
                    y_f = ggdemetra3::raw(x, forecast = TRUE),
                    t_f = ggdemetra3::trendcycle(x, forecast = TRUE),
                    sa_f = ggdemetra3::seasonaladj(x, forecast = TRUE))

  last_date <- tail(time_to_date(data_plot[[1]]), 1)
  last_date <- format(last_date, format = "%Y-%m")

  data_plot <- do.call(ts.union, data_plot)
  # add observed data for plots
  if (add_obs_to_forecast)
    data_plot[which(is.na(data_plot[,"y"]))[1]-1, c("y_f", "t_f", "sa_f")] <-
    data_plot[which(is.na(data_plot[,"y"]))[1]-1, c("y", "t", "sa")]

  # Global info on model
  arima_ord <- sarima_orders_ch(x)
  ntd <- rjd3toolkit::result(x, "regression.ntd") # nombre de JO
  is_easter <- !is.null(rjd3toolkit::result(x, "nmh"))

  est_span <- sprintf("Estimation span: %s to %s (%s obs)",
                      span2text(rjd3toolkit::result(x, "regression.espan.start")),
                      span2text(rjd3toolkit::result(x, "regression.espan.end")),
                      rjd3toolkit::result(x, "regression.espan.n")
  )
  transform <- ifelse(rjd3toolkit::result(x, "log") == 0,
                      "Series hasn't been transformed",
                      "Series has been log-transformed")
  tde <- sprintf("%s, %s",
                 ifelse(ntd==0, "No trading days effect",
                        sprintf("Trading days effect (%s)", ntd)),
                 ifelse(is_easter, "easter effect",
                        "no easter effect"))
  # nb outliers
  nout <- rjd3toolkit::result(x, "regression.nout")
  out <- sprintf("%s detected outliers", nout)
  summary_text <- c(est_span, transform, tde, out, arima_ord)


  # Stats on quality of decomposition
  qstats <- list2DF(rjd3toolkit::user_defined(x, c("m-statistics.q", "m-statistics.q-m2")))
  colnames(qstats) <- c("Q", "Q-M2")
  # Stats on variance decomp
  var_decomp <- list2DF(
    rjd3toolkit::user_defined(
      x,
      c("variancedecomposition.cycle",
        "variancedecomposition.seasonality",
        "variancedecomposition.irregular",
        "variancedecomposition.tdh",
        "variancedecomposition.others",
        "variancedecomposition.total"))
  )
  colnames(var_decomp) <- c("Cycle", "Seasonal", "Irregular", "TDH", "Others", "Total")
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
  var_decomp <- var_decomp * 100
  # Tests on linearised series
  liste_ind_seas <- c("F-test" = "diagnostics.seas-lin-f",
                      "QS-test" = "diagnostics.seas-lin-qs",
                      "Kruskal-Wallis" = "diagnostics.seas-lin-kw",
                      "Friedman" = "diagnostics.seas-lin-friedman",
                      "Combined" = "diagnostics.seas-lin-combined")
  # residuals tests
  liste_ind_res_seas <- c("F-test" = "diagnostics.seas-sa-f",
                          "QS-test" = "diagnostics.seas-sa-qs",
                          "Kruskal-Wallis" = "diagnostics.seas-sa-kw",
                          "Friedman" = "diagnostics.seas-sa-friedman",
                          "Combined" = "diagnostics.seas-sa-combined")

  liste_ind_res_jo <-
    c("Residual TD" = "diagnostics.td-sa-last")
  seas_test <- list2DF(lapply(rjd3toolkit::user_defined(x, liste_ind_seas), function(x) {
    if(length(x) > 1)
      x <- sprintf(nb_format, x$pvalue)
    x
  }))
  seas_res_test <- list2DF(lapply(rjd3toolkit::user_defined(x, liste_ind_res_seas), function(x) {
    if(length(x) > 1)
      x <- sprintf(nb_format, x$pvalue)
    x
  }))
  td_res_test <- data.frame(sprintf(nb_format, rjd3toolkit::result(x, liste_ind_res_jo)$pvalue),
                            "", "", "", "")
  names(seas_test) <- names(seas_res_test) <-
    names(td_res_test) <- names(liste_ind_seas)
  all_tests <- rbind(seas_test, seas_res_test,
                     td_res_test)
  rownames(all_tests) <- c("Seasonality",
                           "Residual Seasonality",
                           "Residual TD effect")
  # On calcule les couleurs
  color_test <- rbind(c(ifelse(seas_test[,-5] < 0.05,  "#A0CD63", "red"),
                        switch(seas_test[,5], "Present" = "#A0CD63",
                               "None" = "red", "orange")),
                      c(ifelse(seas_res_test[,-5] < 0.05,  "red", "#A0CD63"),
                        switch(seas_res_test[,5], "Present" = "red",
                               "None" = "#A0CD63", "orange")),
                      c(ifelse(td_res_test[,1] < 0.05,  "red", "#A0CD63"),
                        rep("white", 4)))


  decomp_stats_color <- c(sapply(qstats, function(x) ifelse(x < 1, "#A0CD63", "red")),
                          "white",
                          rep("grey90", ncol(var_decomp)
                          ))
  qstats[,] <- lapply(qstats, sprintf, fmt = nb_format)
  var_decomp[,] <- lapply(var_decomp, sprintf, fmt = nb_format)

  if (nrow(qstats) == 0) {
    # TRAMO-SEATS
    decomp_stats <- var_decomp
    decomp_stats_color <- unlist(decomp_stats_color[-c(1:3)])
  } else {
    # X-13
    decomp_stats <- cbind(qstats, "   " , var_decomp)
    colnames(decomp_stats)[ncol(qstats)+1] <- "   "
  }

  outliers <- outliers_color <- NULL
  if (nout > 0) {
    outliers <- do.call(rbind, rjd3toolkit::user_defined(x, sprintf("regression.out(%i)", seq_len(nout))))
    # sort outliers by dates
    dates_out <- outliers_to_dates(rownames(outliers))
    dates_out$type <- factor(dates_out$type, levels = order_outliers, ordered = TRUE)
    outliers <- outliers[order(dates_out$year, dates_out$period, dates_out$type, decreasing = TRUE), , drop = FALSE]
    outliers <- outliers[seq_len(min(n_last_outliers, nrow(outliers))), columns_outliers, drop = FALSE]
    outliers <- round(outliers, digits_outliers)
    outliers <- data.frame(rownames(outliers),
                           outliers)
    colnames(outliers)[1] <- sprintf("Last %i outliers", n_last_outliers)
    rownames(outliers) <- NULL

    outliers_color <-
      cbind(rep("grey90", nrow(outliers)),
            matrix("white", ncol = ncol(outliers) - 1, nrow = nrow(outliers)))
  }

  res <- list(main_plot = data_plot,
              siratio_plot = ggdemetra3::siratio(x),
              summary_text = summary_text,
              decomp_stats = list(table = decomp_stats,
                                  colors = decomp_stats_color),
              residuals_tests = list(table = all_tests,
                                     colors = color_test),
              last_date = last_date,
              outliers = list(table = outliers,
                              colors = outliers_color))
  class(res) <- c("simple_dashboard")
  res
}
outliers_to_dates <- function(name_out){
  dates_out <- gsub("\\w. \\((.*)\\)", "\\1", name_out)
  types <- gsub(" .*", "", name_out)
  dates <- do.call(rbind, strsplit(dates_out, "-"))
  periods <- as.numeric(as.roman(dates[,1]))
  years <- as.numeric(dates[,2])
  data.frame(year = years, period = periods, type = types)
}
