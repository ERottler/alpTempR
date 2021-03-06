% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method_analys.R
\name{sens_slope}
\alias{sens_slope}
\title{Pre-withened nonlinear Sen´s slope.}
\usage{
sens_slope(data_in, cover_thresh = 0.9)
}
\arguments{
\item{data_in}{vector of input data}

\item{cover_thresh}{Trend only calculated when percentage of NAs in input vector
does not exceed cover_thresh (0-1)}
}
\value{
Nonlinear pre-withened Theil-Sen´s slope.
}
\description{
Calculation of nonlinear pre-whitened Theil-Sen´s slope using \link{zyp.trend.vector}.
To remove lag-1 autocorrelation, the input series is pre-withened using the
Zhang approach. Trend magnitude only is calculated when percentage of NAs in
input vector does not exceed a pre-defined threshold.
}
\examples{
data_slope <- c(2, 3, 5, 6 ,8, 5, 8, 12, NA, NA)
sens_slope(data_in, cover_thresh = 0.5)
sens_slope(data_in, cover_thresh = 0.9)
}
