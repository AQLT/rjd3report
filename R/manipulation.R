#' Get SARIMA Orders
#'
#' `sarima_orders()` returns the SARIMA orders as a list while
#' `sarima_orders_ch()` returns a string.
#'
#' @param x The model.
#' @param ... Other unused parameters.
#'
#' @examples
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' mod <- rjd3toolkit::sarima_estimate(y, order = c(0,1,1), seasonal = c(0,1,1))
#' sarima_orders(mod)
#' sarima_orders_ch(mod)
#' mod_x13 <- rjd3x13::x13(y)
#' sarima_orders_ch(mod_x13)
#' @export
sarima_orders <- function(x, ...){
  UseMethod("sarima_orders", x)
}
#' @export
sarima_orders.JD3_X13_OUTPUT <- function(x, ...) {
  sarima_orders(x$result, ...)
}
#' @export
sarima_orders.JD3_X13_RSLTS <- function(x, ...) {
  sarima_orders(x$preprocessing, ...)
}
#' @export
sarima_orders.JD3_TRAMOSEATS_OUTPUT <- function(x, ...) {
  sarima_orders(x$result, ...)
}
#' @export
sarima_orders.JD3_TRAMOSEATS_RSLTS <- function(x, ...) {
  sarima_orders(x$preprocessing, ...)
}
#' @export
sarima_orders.JD3_REGARIMA_RSLTS <- function(x, ...) {
  sarima_orders(x$description$arima, ...)
}
#' @export
sarima_orders.JD3_SARIMA_ESTIMATE <- function(x, ...) {
  list(p = x$orders$order[1], d = x$orders$order[2],
       q = x$orders$order[3], bp = x$orders$seasonal$order[1],
       bd = x$orders$seasonal$order[2], bq = x$orders$seasonal$order[3],
       period = x$orders$seasonal$period)
}
#' @export
sarima_orders.JD3_SARIMA_ESTIMATION<- function(x, ...) {
  m <- x
  if (!is.null(m$phi))
    p <- dim(m$phi)[2]
  else p <- 0
  if (!is.null(m$theta))
    q <- dim(m$theta)[2]
  else q <- 0
  if (!is.null(m$bphi))
    bp <- dim(m$bphi)[2]
  else bp <- 0
  if (!is.null(m$btheta))
    bq <- dim(m$btheta)[2]
  else bq <- 0
  list(p = p, d = m$d, q = q, bp = bp, bd = m$bd,
       bq = bq, period = m$period)
}
#' @export
sarima_orders.JD3_Object <- function(x, ...) {
  list(p = rjd3toolkit::result(x, "arima.p"),
       d = rjd3toolkit::result(x, "arima.d"),
       q = rjd3toolkit::result(x, "arima.q"),
       bp = rjd3toolkit::result(x, "arima.bp"),
       bd = rjd3toolkit::result(x, "arima.bd"),
       bq = rjd3toolkit::result(x, "arima.bq"),
       period = rjd3toolkit::result(x, "period"))
}
#' @name sarima_orders
#' @export
sarima_orders_ch <- function(x, ...) {
  orders <- sarima_orders(x, ...)
  arima_ord <- sprintf("SARIMA(%s)(%s)[%s]",
                       paste(unlist(orders[c("p", "d", "q")]), collapse = ","),
                       paste(unlist(orders[c("bp", "bd", "bq")]), collapse = ","),
                       orders[["period"]])
  arima_ord
}

get_jmod <- function(x, context = NULL, ...){
  UseMethod("get_jmod", x)
}
get_jmod.list<- function(x, context = NULL, ...) {
  if (inherits(x$results, "JD3_X13_RSLTS")) {
    rjd3x13::.jx13(ggdemetra3::raw(x$results), x$pointSpec, context = context)
  } else {
    rjd3tramoseats::.jtramoseats(ggdemetra3::raw(x$results), x$pointSpec, context = context)
  }
}
get_jmod.JD3_Object<- function(x, ...) {
  x
}
get_jmod.JD3_X13_OUTPUT <- function(x, context = NULL, ...) {
  rjd3x13::.jx13(ggdemetra3::raw(x), x$result_spec, context = context)
}
get_jmod.JD3_TRAMOSEATS_OUTPUT <- function(x, context = NULL, ...) {
  rjd3tramoseats::.jtramoseats(ggdemetra3::raw(x), x$result_spec, context = context)
}

span2text <- function(x, format = "%Y-%m") {
  as.character(format(as.Date(x), "%Y-%m"))
}
