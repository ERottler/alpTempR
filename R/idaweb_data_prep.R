#' MeteoSwiss Idaweb climate data preparation.
#'
#' Preparation of raw climatological data obtained from national weather and
#' climate service of Switzerland (MeteoSwiss) via Idaweb.
#'
#' @param param_code  Parameter code from climatological variable as downloaded
#' from Idaweb (e.g. "tre200d0" or "gre000d0")
#' @param order_dir ...
#' @param out_dir ...
#' @param start_day ...
#' @param end_day ...
#' @return The arithmetic mean of values in \code{x}.
#' @examples
#' mea_na(c(2 ,3, NA))
#' mea_na(c(NA, NA, NA))
#' @export

# param_code = "tre200d0"
# order_dir = paste0(base_dir,"rawData/IDAweb/tmean")
# out_dir = base_dir
# start_day = "1981-01-01"
# end_day = "2017-12-31"

idaweb_data_prep <- function(param_code, order_dir, out_dir,
                             start_day="1981-01-01",
                             end_day="2017-12-31"){

  order_folder <- dir(order_dir)

  for(i in 1:length(order_folder)){

    order_number <- substr(order_folder[i], 6, nchar(order_folder[i])-4)

    data_idaweb <- read.table(unz(paste0(order_dir, "/", order_folder[i]),
                                  paste0("order_", order_number, "_data.txt")),
                              sep = ";", header = T, skip = 2, dec = ".",
                              na.strings = "-")

    param_col <- which(grepl(param_code, colnames(data_idaweb)))[1] #column of selected parameter
    data_idaweb$value <- data_idaweb[,param_col]

    data_param <- data.frame(stn=data_idaweb$stn, date=data_idaweb$time, value=data_idaweb$value)

    if(i ==1){data_param_all <- data_param}else{
      data_param_all <- rbind(data_param_all, data_param)}

    print(paste(order_folder[i], "processed."))
  }

  IDs <- unique(data_param_all$stn)
  IDs <- IDs[-which(IDs == "stn")]

  data_param_all$date <- as.POSIXct(strptime(data_param_all$date, "%Y%m%d", tz="UTC"))

  start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
  end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
  full_date  <- seq(start_date, end_date, by="day")

  for(i in 1:length(IDs)){
    #which rows contain data from selected station
    stat_rows <- which(IDs[i] == as.character(data_param_all$stn))

    if(length(stat_rows) < 1 ){#if no data, fill with NAs
      data_stat <- data.frame(value = rep(NA,length(full_date)))
    }else{
      data_stat <- data_param_all[stat_rows,]
      data_stat$value <- fac2num(data_stat$value)
      data_stat <- data.frame(value = with(data_stat, data_stat$value[match(full_date, date)]))
    }
    colnames(data_stat) <- IDs[i]

    if(i == 1){
      out_data <- data.frame(date   = full_date, value  = data_stat[ , 1])
      colnames(out_data) <- c("date", as.character(IDs[i]))
    }else{
      out_data <- cbind(out_data, data_stat[ , 1])
      colnames(out_data)[ncol(out_data)] <- as.character(IDs[i])
    }
  }

  #Save data as .Rdata
  save(file = paste0(out_dir, param_code, ".RData"), list="out_data")

}
