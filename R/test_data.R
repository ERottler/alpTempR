#' Test data temperature time series.
#'
#' A test dataset of temperature time series (radomly generated) of six station
#'
#' @format A data frame with 7 columns (date column + six columns with stations data)
#' and 10958 rows. Temperature values were randomly generated.
"temperature"



#' Test data station meta information.
#'
#' A dataset of meta information of climatological stations
#'
#' @format A data frame with 7 columns:
#' \describe{
#' \item{stn}{Station ID}
#' \item{name}{Station name}
#' \item{alt}{Altitude}
#' \item{lat}{Latitude}
#' \item{lon}{Longitude}
#' \item{category}{Elevation category (low, middle, or high)}
#' \item{data_qual}{quality level of data (homogenized or quality-checked)}
#' }
"station_meta"

