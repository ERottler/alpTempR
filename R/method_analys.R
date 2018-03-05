#' Pre-withened nonlinear Sen´s slope.
#'
#' Calculation of nonlinear pre-whitened Theil-Sen´s slope using \link{zyp.trend.vector}.
#' To remove lag-1 autocorrelation, the input series is pre-withened using the
#' Zhang approach. Trend magnitude only is calculated when percentage of NAs in
#' input vector does not exceed a pre-defined threshold.
#'
#' @param data_in  vector of input data
#' @param cover_thresh Trend only calculated when percentage of NAs in input vector
#' does not exceed cover_thresh (0-1)
#' @return Nonlinear pre-withened Theil-Sen´s slope.
#' @examples
#' data_slope <- c(2, 3, 5, 6 ,8, 5, 8, 12, NA, NA)
#' sens_slope(data_in, cover_thresh = 0.5)
#' sens_slope(data_in, cover_thresh = 0.9)
#' @export
sens_slope <- function(data_in, cover_thresh = 0.9){
  if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
    sens_slo <-  NA
  }else{
    sens_slo <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[2])
  }
  return(sens_slo)
}




#' Pre-withened Mann-Kendall trend test significance.
#'
#' Assessment of trend significance using the Mann-Kendall trend test. Time series
#' is prewithened using Zhang method \link{zyp.trend.vector}.
#'
#' @param data_in  vector of input data
#' @param cover_thresh Significance only calculated when percentage of NAs in
#' input vector does not exceed cover_thresh (0-1)
#' @return Kendall`s P-value computed on the final detrended time series.
#' @examples
#' data_mk <-  c(2, 3, 5, 6 ,8, 5, 8, 12, NA, NA)
#' mann_kendall(data_in, cover_thresh = 0.5)
#' mann_kendall(data_in, cover_thresh = 0.9)
#' @export
mann_kendall <- function(data_in, cover_thresh = 0.9){
  if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
    mann_ken <-  NA
  }else{
    mann_ken <- as.numeric(zyp.trend.vector(data_in, method = "zhang", conf.intervals = F)[6])
  }
  return(mann_ken)
}

