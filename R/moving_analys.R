#' Moving average data analysis.
#'
#' Function for moving average trend statistics.
#'
#' @param dates Input date vector.
#' @param values Input values of time series
#' @param start_year Start year of time frame to be analyzed
#' @param end_year End year of time frame to be analyzed
#' @param window_width Window width of moving averages
#' @param cover_thresh Cover threshold when calculation conducted
#' @param method_analys Select analytical method ("sens_slope", "mann_kendall", "mean",
#' "snow_likelihood", "weather_likelihood", "snow_window_likeli_sens_slope",
#' ""weather_type_window_likeli_sens_slope" or "snow_window_likeli_mk")
#' @return Yearly cycle of e.g. trend magnitude or mean values
#' @examples
#' temp_slo <- moving_analys(dates = temperature$date, values= temperature[, 2],
#'                           start_year = 1985,
#'                           end_year = 2015,
#'                           window_width = 30,
#'                           cover_thres = 0.9,
#'                           method_analys = "sens_slope")
#'
#' plot(temp_slo, type="l", ylab = "Trend mangitude [°C/year]", xlab="Day  of the year")
#' @export

moving_analys <- function(dates, values, start_year, end_year, window_width,
                          cover_thresh, method_analys, weather_type = 1){

  input_data <- data.frame(dates = dates, values = values)

  #Clip selected time period
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) >= start_year, ]
  input_data <- input_data[as.numeric(format(input_data$date,'%Y')) <= end_year, ]

  #Fill possible gaps
  start_date <- as.POSIXct(strptime(paste0(start_year,"-01-01"), "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(paste0(end_year,"-12-31"),   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")

  input_data <- data.frame(dates  = full_date,
                           values = with(input_data, values[match(as.Date(full_date), as.Date(dates))])
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
    f_mea_na_thres <- function(data_in){mea_na_thres(x = data_in, na_thres = 1 - cover_thresh)}
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

  if(method_analys == "weather_likelihood"){
    #Data no moving average before
    #Order data by day
    data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year)-1)) {

      data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]

    }

    f_weather_likelihood <- function(data_in){
      if(length(which(is.na(data_in))) / length(data_in) > (1-cover_thresh)){
        weather_lik <-  NA
      }else{
        weather_lik <-sum(ifelse(data_in == weather_type, 1, 0), na.rm=T) / (length(data_in)-length(which(is.na(data_in))))}
      return(weather_lik)
    }

    mov_res <- apply(data_day[,-1], 2, f_weather_likelihood)
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
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }

    #Calculate trends of snow likelihood
    mov_res <- apply(data_day[, -1], 2, f_sens_slope)

    #When likelihood always (or only one value different from) 0 / 1 no trend
    #calculated (NA), but trend is 0

    for(i in 1:365){

      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){

        if(length(which(data_day[, i+1] == 1)) >=
                  ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }

        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }
      }
    }


    }

  if(method_analys == "weather_type_window_likeli_sens_slope"){
    #Selected weather type: Yes (1) or No (0)

    f_weatherYN <- function(data_in, gwts = weather_type){

         if(data_in %in% gwts){
           data_in <- 1
         }else{
           data_in <- 0
         }
      return(data_in)
    }

    input_data$values <- sapply(input_data$values, f_weatherYN)


    #Apply moving average
    input_data$ma <- rollapply(data = input_data$values, width = window_width,
                               FUN = mea_na_thres, align = "center", fill = NA)

    #Order data by day
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }

    #Calculate trends of window likelihood
    mov_res <- apply(data_day[, -1], 2, f_sens_slope)

    #When likelihood always (or only one value different from) 0 / 1 no trend
    #calculated (NA), but trend is 0

    for(i in 1:365){

      #if trend magnitude calculated
      if((length(which(is.na(data_day[, i+1]))) / nrow(data_day)) < (1 - cover_thresh)){

        if(length(which(data_day[, i+1] == 1)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }

        if(length(which(data_day[, i+1] == 0)) >=
           ((nrow(data_day) - 2) - length(which(is.na(data_day[, i+1]))))){
          mov_res[i] <- 0
        }
      }
    }


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
    data_day = matrix(NA, nrow=length(start_year:end_year), ncol = 366)
    colnames(data_day)=c("year", days)
    data_day[, 1] <- start_year:end_year

    for(i in 0:(length(start_year:end_year) - 1)) {
      data_day[i + 1, 2:366] <- input_data$ma[(i * 365 + 1):((i + 1 )* 365)]
    }

    #Calculate trends of snow likelihood
    mov_res <- apply(data_day[, -1], 2, f_mann_kendall)
  }

  return(mov_res)

}
