#' Calculate nonlinear prewhitened Sen´s slope trend.
#'
#' Calculation of non-linear prewhitened (mehthod of Yue Pilon) Sen´s slope trend
#' using \link{zyp.trend.vector}.
#'
#' @param data_in  vector of input data
#' @param cover_thresh Trend only calculated when percentage of NAs in input vector
#' does not exceed cover_thresh (0-1)
#' @return Nonlinear prewithened Sen´s slope trend.
#' @examples
#' data_slope <- c(2, 3, 5, 6 ,8, 5, 8, 12, NA, NA)
#' sens_slope(data_in, cover_thresh = 0.5)
#' sens_slope(data_in, cover_thresh = 0.9)
#' @export
sens_slope <- function(data_in, cover_thresh = 0.9){
  if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
    sens_slo <-  NA
  }else{
    sens_slo <- as.numeric(pwmk(data_in)[2])
    #sens_slo <- as.numeric(zyp.trend.vector(data_in, method="yuepilon", conf.intervals=F)[2])
  }
  return(sens_slo)
}




#' Calculate significance of trend using Mann Kendall trend test.
#'
#' Calculation of non-linear prewhitened ...
#'
#' @param data_in  vector of input data
#' @param cover_thresh Trend only calculated when percentage of NAs in input vector
#' does not exceed cover_thresh (0-1)
#' @return Nonlinear prewithened Sen´s slope trend.
#' @examples
#' data_mk <-  c(2, 3, 5, 6 ,8, 5, 8, 12, NA, NA)
#' mann_kendall(data_in, cover_thresh = 0.5)
#' mann_kendall(data_in, cover_thresh = 0.9)
#' @export
mann_kendall <- function(data_in, cover_thresh = 0.9){
  if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
    mann_ken <-  NA
  }else{
    mann_ken <- as.numeric(pwmk(data_in)[4])
    #mann_ken <- as.numeric(zyp.trend.vector(data_in, method="yuepilon", conf.intervals=F)[6])
  }
  return(mann_ken)
}

