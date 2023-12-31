% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulation.R
\name{sarima_orders}
\alias{sarima_orders}
\alias{sarima_orders_ch}
\title{Get SARIMA Orders}
\usage{
sarima_orders(x, ...)

sarima_orders_ch(x, ...)
}
\arguments{
\item{x}{The model.}

\item{...}{Other unused parameters.}
}
\description{
\code{sarima_orders()} returns the SARIMA orders as a list while
\code{sarima_orders_ch()} returns a string.
}
\examples{
y <- rjd3toolkit::ABS$X0.2.09.10.M
mod <- rjd3toolkit::sarima_estimate(y, order = c(0,1,1), seasonal = c(0,1,1))
sarima_orders(mod)
sarima_orders_ch(mod)
mod_x13 <- rjd3x13::x13(y)
sarima_orders_ch(mod_x13)
}
