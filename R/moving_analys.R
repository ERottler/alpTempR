#' Moving average data analysis.
#'
#' ...
#'
#' @param dates ...
#' @param values ...
#' @param start_year ...
#' @param end_year ...
#' @param window_width ...
#' @param cover_thresh ...
#' @param method_analys ...
#' @return ...
#' @examples
#' pres2ahum(vapor_pres = 5.5, temp = 20)
#' @export


# start_year   <- 1981
#
# end_year     <- 2017
# window_width <- 90
# cover_thresh    <- 32/37
# method_analys <- "sens_slope"
# dates <- tem0Data$date
# values <- tem0Data$ALT

moving_analys <- function(dates, values, start_year, end_year, window_width,
                          cover_thresh, method_analys){

  input_data <- data.frame(dates = dates, values = values)

  #Clip selected time period
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) >= start_year, ]
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) <= end_year, ]

  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")

  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, values[match(full_date, dates)])
  )

  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$date, "%m%d") == "0229"),]

  #Moving average filter
  input_data$ma <- rollapply(data = input_data$values, width = window_width,
                             FUN = mea_na_thres, align = "center", fill = NA)

  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")

  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year

  for(i in 0:(length(start_year:end_year)-1)) {

    data_day[i+1, 2:366] <- input_data$ma[(i*365+1):((i+1)*365)]

  }

  f_sens_slope <- function(data_in){sens_slope(data_in = data_in, cover_thresh = cover_thresh)}
  f_mann_kendall <- function(data_in){mann_kendall(data_in = data_in, cover_thresh = cover_thresh)}

  if(method_analys == "sens_slope"){
    mov_res <- apply(data_day[,-1], 2, f_sens_slope)
  }

  if(method_analys == "mann_kendall"){
    mov_res <- apply(data_day[,-1], 2, f_mann_kendall)
  }

  if(method_analys == "mean"){
    f_mea_na_thres <- function(data_in){mea_na_thres(x = data_in, na_thres = cover_thresh)}
    mov_res <- apply(data_day[,-1], 2, f_mea_na_thres)
  }

  if(method_analys == "snow_likelihood"){
    #Data no moving average before
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year)-1)) {

      data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]

    }

    snow_thres <- 0 #threshold for snow cover yes or no

    f_snow_likelihood <- function(data_in){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        snow_lik <-  NA
      }else{
        snow_lik <-sum(ifelse(data_in > snow_thres, 1, 0), na.rm=T) / (length(data_in)-length(which(is.na(data_in))))}
      return(snow_lik)
    }

    mov_res <- apply(data_day[,-1], 2, f_snow_likelihood)
  }

  if(method_analys == "snow_window_likeli_sens_slope"){
    #Snow yes or no
    snow_thres <- 0 #threshold for snow cover yes or not
    f_snowYN <- function(data_in){ifelse(data_in > snow_thres, 1, 0)}

    input_data$values <- sapply(input_data$values, f_snowYN)

    #Apply moving average
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = mea_na_thres, align = "center", fill = NA)

    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol=366)
    colnames(data_day)=c("year", days)
    data_day[,1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year)-1)) {

      data_day[i+1,2:366] <- input_data$ma[(i*365+1) : ((i+1)*365)]

    }

    #Calculate trends of snow likelihood

    mov_res <- apply(data_day[,-1], 2, f_sens_slope)

  }

  if(method_analys == "snow_window_likeli_mk"){
    #Snow yes or no
    snow_thres <- 0 #threshold for snow cover yes or not
    f_snowYN <- function(data_in){ifelse(data_in > snow_thres, 1, 0)}

    input_data$values <- sapply(input_data$values, f_snowYN)

    #Apply moving average
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = mea_na_thres, align = "center", fill = NA)

    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol=366)
    colnames(data_day)=c("year", days)
    data_day[,1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year)-1)) {

      data_day[i+1,2:366] <- input_data$ma[(i*365+1) : ((i+1)*365)]

    }

    #Calculate significance with Mann Kendall of snow window likelihood trend

    mov_res <- apply(data_day[,-1], 2, f_mann_kendall)

  }

  return(mov_res)

}
