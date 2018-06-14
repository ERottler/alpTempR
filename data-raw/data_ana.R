###

#Data analyses

###

Sys.time()

library("alptempr")
library("zoo")
library("modifiedmk")
library("zyp")

base_dir <- "u:/RhineFlow/Elevation/Data/"

load(paste0(base_dir,"tre200d0.RData")) ; tem0_data <- out_data #Air temperature; daily mean [°C]
load(paste0(base_dir,"tre200dn.RData")) ; temn_data <- out_data #Air temperature; daily minimum [°C]
load(paste0(base_dir,"tre200dx.RData")) ; temx_data <- out_data #Air temperature; daily maximum [°C]
load(paste0(base_dir,"sre000d0.RData")) ; suns_data <- out_data #Sunshine duration [min]
load(paste0(base_dir,"abshumid.RData")) ; ahum_data <- out_data #Absolute air humidity [g/cm3]
load(paste0(base_dir,"gre000d0.RData")) ; radi_data <- out_data #Global radiation; daily mean [W/m²]
load(paste0(base_dir,"nto000d0.RData")) ; clou_data <- out_data #Clouds total; daily mean [%]
load(paste0(base_dir,"hto000d0.RData")) ; snow_data <- out_data #Snow depth; measurement 05:40
load(paste0(base_dir,"pp0qffd0.RData")) ; airp_data <- out_data #Air pressure reference sea level [hPa]

start_year   <- 1981
end_year     <- 2017
window_width <- 30
cover_thres  <- 32/37

#Functions
date_data <- tem0_data$date #full date string; similar for all data sets

f_sl <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        cover_thres = cover_thres,
                                        method_analys = "sens_slope")}

f_mk <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        cover_thres = cover_thres,
                                        method_analys = "mann_kendall")}

f_me <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        cover_thres = cover_thres,
                                        method_analys = "mean")}

f_li <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                        start_year = start_year,
                                        end_year = end_year,
                                        window_width = window_width,
                                        cover_thres = cover_thres,
                                        method_analys = "snow_likelihood")}

f_sl_sn <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                           start_year = start_year,
                                           end_year = end_year,
                                           window_width = window_width,
                                           cover_thres = cover_thres,
                                           method_analys = "snow_window_likeli_sens_slope")}

f_mk_sn <- function(data_in){moving_analys(dates = date_data, values= data_in,
                                             start_year = start_year,
                                             end_year = end_year,
                                             window_width = window_width,
                                             cover_thres = cover_thres,
                                             method_analys = "snow_window_likeli_mk")}

#calculate trend magnitude using Sen's Slope (per decade)
#only keep stations which had sufficient data coverage
tem0_sl <- as.data.frame(apply(tem0_data[, -1], 2 , f_sl))*10 ; tem0_sl <- stat_coverage(tem0_sl)
temx_sl <- as.data.frame(apply(temx_data[, -1], 2 , f_sl))*10 ; temx_sl <- stat_coverage(temx_sl)
temn_sl <- as.data.frame(apply(temn_data[, -1], 2 , f_sl))*10 ; temn_sl <- stat_coverage(temn_sl)
suns_sl <- as.data.frame(apply(suns_data[, -1], 2 , f_sl))*10 ; suns_sl <- stat_coverage(suns_sl)
radi_sl <- as.data.frame(apply(radi_data[, -1], 2 , f_sl))*10 ; radi_sl <- stat_coverage(radi_sl)
clou_sl <- as.data.frame(apply(clou_data[, -1], 2 , f_sl))*10 ; clou_sl <- stat_coverage(clou_sl)
ahum_sl <- as.data.frame(apply(ahum_data[, -1], 2 , f_sl))*10 ; ahum_sl <- stat_coverage(ahum_sl)
airp_sl <- as.data.frame(apply(airp_data[, -1], 2 , f_sl))*10 ; airp_sl <- stat_coverage(airp_sl)
snow_sl <- as.data.frame(apply(snow_data[, -1], 2 , f_sl_sn))*10*100 ; snow_sl <- stat_coverage(snow_sl)

#Calculate singificance of trends using Mann Kendall
tem0_mk <- as.data.frame(apply(tem0_data[, -1], 2 , f_mk)) ; tem0_mk <- stat_coverage(tem0_mk)
temx_mk <- as.data.frame(apply(temx_data[, -1], 2 , f_mk)) ; temx_mk <- stat_coverage(temx_mk)
temn_mk <- as.data.frame(apply(temn_data[, -1], 2 , f_mk)) ; temn_mk <- stat_coverage(temn_mk)
suns_mk <- as.data.frame(apply(suns_data[, -1], 2 , f_mk)) ; suns_mk <- stat_coverage(suns_mk)
radi_mk <- as.data.frame(apply(radi_data[, -1], 2 , f_mk)) ; radi_mk <- stat_coverage(radi_mk)
clou_mk <- as.data.frame(apply(clou_data[, -1], 2 , f_mk)) ; clou_mk <- stat_coverage(clou_mk)
ahum_mk <- as.data.frame(apply(ahum_data[, -1], 2 , f_mk)) ; ahum_mk <- stat_coverage(ahum_mk)
airp_mk <- as.data.frame(apply(airp_data[, -1], 2 , f_mk)) ; airp_mk <- stat_coverage(airp_mk)
snow_mk <- as.data.frame(apply(snow_data[, -1], 2 , f_mk_sn)) ; snow_mk <- stat_coverage(snow_mk)

#Calculate mean values
tem0_me <- as.data.frame(apply(tem0_data[, -1], 2 , f_me)) ; tem0_me <- stat_coverage(tem0_me)
temx_me <- as.data.frame(apply(temx_data[, -1], 2 , f_me)) ; temx_me <- stat_coverage(temx_me)
temn_me <- as.data.frame(apply(temn_data[, -1], 2 , f_me)) ; temn_me <- stat_coverage(temn_me)
suns_me <- as.data.frame(apply(suns_data[, -1], 2 , f_me)) ; suns_me <- stat_coverage(suns_me)
radi_me <- as.data.frame(apply(radi_data[, -1], 2 , f_me)) ; radi_me <- stat_coverage(radi_me)
clou_me <- as.data.frame(apply(clou_data[, -1], 2 , f_me)) ; clou_me <- stat_coverage(clou_me)
ahum_me <- as.data.frame(apply(ahum_data[, -1], 2 , f_me)) ; ahum_me <- stat_coverage(ahum_me)
airp_me <- as.data.frame(apply(airp_data[, -1], 2 , f_me)) ; airp_me <- stat_coverage(airp_me)
snow_me <- as.data.frame(apply(snow_data[, -1], 2 , f_me)) ; snow_me <- stat_coverage(snow_me)

#Snow likelihood; probability of snow being present on e.g. 15th of February
snow_li   <- as.data.frame(apply(snow_data[,-1], 2 , f_li))*100 # [%]

#Calculate trends absolute humidity relative to average water content
IDs_ahum <- colnames(ahum_sl)[which(colnames(ahum_sl) %in% colnames(ahum_me))]

ahum_sr <- ahum_sl[,which(colnames(ahum_sl) %in% colnames(ahum_me))]
ahum_sr[ , ] <- NA
ahum_sr$date <- ahum_sl$date

for(i in 1:length(IDs_ahum)){
  print(i)
  ID_ahum_sl <- which(colnames(ahum_sl) == IDs_ahum[i])
  ID_ahum_sr <- which(colnames(ahum_sr) == IDs_ahum[i])
  ID_ahum_me <- which(colnames(ahum_me) == IDs_ahum[i])
  ahum_sr[,ID_ahum_sr] <- ahum_sl[,ID_ahum_sl] / ahum_me[,ID_ahum_me]
}

#Annual average values
tem0_sl_an <- apply(tem0_sl[,], 2, med_na)
temx_sl_an <- apply(temx_sl[,], 2, med_na)
temn_sl_an <- apply(temn_sl[,], 2, med_na)
suns_sl_an <- apply(suns_sl[,], 2, med_na)
radi_sl_an <- apply(radi_sl[,], 2, med_na)
clou_sl_an <- apply(clou_sl[,], 2, med_na)
ahum_sr_an <- apply(ahum_sr[,], 2, med_na)
airp_sl_an <- apply(airp_sl[,], 2, med_na)
snow_sl_an <- apply(snow_sl[,], 2, mea_na) #mean average instead of median

tem0_me_an <- apply(tem0_me[,], 2, med_na)
temx_me_an <- apply(temx_me[,], 2, med_na)
temn_me_an <- apply(temn_me[,], 2, med_na)
suns_me_an <- apply(suns_me[,], 2, med_na)
radi_me_an <- apply(radi_me[,], 2, med_na)
clou_me_an <- apply(clou_me[,], 2, med_na)
ahum_me_an <- apply(ahum_me[,], 2, med_na)
airp_me_an <- apply(airp_me[,], 2, med_na)
snow_me_an <- apply(snow_me[,], 2, mea_na) #mean average instead of median

#Meta data
stat_meta <- read.table(paste0(base_dir,"rawData/IDAweb/stationMeta.csv"), sep=",", header=T)

stats_data <- unique(c(colnames(tem0_sl),colnames(temx_sl),colnames(temx_sl),
                       colnames(snow_sl),colnames(clou_sl),colnames(radi_sl),
                       colnames(suns_sl),colnames(ahum_sl),colnames(airp_sl)))

stats_data[which(!stats_data %in% stat_meta$stn)]#station in data that are not in meta data
stat_meta$stn[which(!stat_meta$stn %in% stats_data)]#stations in meta data that are not in data

#Get groups of stations
st_all <- stat_meta$stn
st_hig <- stat_meta$stn[which(stat_meta$category == "high")]
st_mid <- stat_meta$stn[which(stat_meta$category == "middle")]
st_low <- stat_meta$stn[which(stat_meta$category == "low")]
st_hom <- stat_meta$stn[which(stat_meta$data_qual  == "homogenized")]
st_qch <- stat_meta$stn[which(stat_meta$data_qual  == "quality-checked")]
st_all_nu <- 1:nrow(stat_meta)
st_hig_nu <- which(stat_meta$category == "high")
st_mid_nu <- which(stat_meta$category == "middle")
st_low_nu <- which(stat_meta$category == "low")
st_hom_nu <- which(stat_meta$data_qual  == "homogenized")
st_qch_nu <- which(stat_meta$data_qual  == "quality-checked")

#Mean elevation of categories
ele_HS <- mea_na(stat_meta$alt[st_hig_nu])
ele_MS <- mea_na(stat_meta$alt[st_mid_nu])
ele_LS <- mea_na(stat_meta$alt[st_low_nu])

#Difference Tmax and Tmin trends
diff_max_min <- rep(NA,365)
stat_IDs <- colnames(temx_sl)[which(colnames(temx_sl)%in% colnames(temn_sl))]

for(i in 1:length(stat_IDs)){
  print(i)
  max_min <- temx_sl[,which(grepl(paste0(stat_IDs[i]), colnames(temx_sl)))] -
             temn_sl[,which(grepl(paste0(stat_IDs[i]), colnames(temn_sl)))]

  if(i ==1){
    diff_max_min <- max_min
  }else{
    diff_max_min <- cbind(diff_max_min, max_min=max_min)
  }

  if(i == length(stat_IDs)){
    colnames(diff_max_min) <- paste0(colnames(temx_sl))
  }
}


#WTC_CPA####

stat_data <- read.table(paste0(base_dir, "rawData/IDAweb/weastat/order58612/order_58612_data.txt"),
                        sep = ";", skip = 1, header = TRUE, na.strings = "-")

stat_data$time <- as.POSIXct(strptime(stat_data$time, "%Y%m%d", tz="UTC"))

start_day <- "1981-01-01"
end_day   <- "2017-12-31"

start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
full_date  <- seq(start_date, end_date, by="day")

data_stat <- data.frame(date = full_date,
                        value = with(stat_data, stat_data$wkcap1d0[match(full_date, time)]))

wt_1 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 1)

wl_1 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 1)

wt_2 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 2)

wl_2 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 2)

wt_3 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 3)

wl_3 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 3)

wt_4 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 4)

wl_4 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 4)

wt_5 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 5)

wl_5 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 5)

wt_6 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 6)

wl_6 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 6)

wt_7 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 7)

wl_7 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 7)

wt_8 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 8)

wl_8 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 8)

wt_9 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = start_year,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                      weather_type = 9)

wl_9 <- moving_analys(dates = data_stat$date, values = data_stat$value, start_year = 1981,
                      end_year = end_year, window_width = window_width,
                      cover_thresh= cover_thres, method_analys = "weather_likelihood",
                      weather_type = 9)

wl_data <- rbind(wl_5, #High Pressure over the Alps
                 wl_8, #High Pressure over Central Europe
                 wl_9, #Westerly flow over Southern Europe, cyclonic
                 wl_2, #West-SouthWest, cyclonic, flat pressure
                 wl_7, #West-SouthWest, cyclonic
                 wl_4, #East, indifferent
                 wl_1, #NorthEast, indifferent
                 wl_3, #Westerly flow over Northern Europe
                 wl_6  #North, cyclonic
                 )*100 #Frequency in %

wt_data <- rbind(wt_5, #High Pressure over the Alps
                 wt_8, #High Pressure over Central Europe
                 wt_9, #Westerly flow over Southern Europe, cyclonic
                 wt_2, #West-SouthWest, cyclonic, flat pressure
                 wt_7, #West-SouthWest, cyclonic
                 wt_4, #East, indifferent
                 wt_1, #NorthEast, indifferent
                 wt_3, #Westerly flow over Northern Europe
                 wt_6  #North, cyclonic
                 )*100*10 #Trend frequency in %/dec

#Smooth frequency values using loess
wl_data[1,] <- loess_NA_restore(wl_data[1, ])
wl_data[2,] <- loess_NA_restore(wl_data[2, ])
wl_data[3,] <- loess_NA_restore(wl_data[3, ])
wl_data[4,] <- loess_NA_restore(wl_data[4, ])
wl_data[5,] <- loess_NA_restore(wl_data[5, ])
wl_data[6,] <- loess_NA_restore(wl_data[6, ])
wl_data[7,] <- loess_NA_restore(wl_data[7, ])
wl_data[8,] <- loess_NA_restore(wl_data[8, ])
wl_data[9,] <- loess_NA_restore(wl_data[9, ])

Sys.time()

data_avail <- rep("", 93)
data_avail[which(stat_meta$stn %in% colnames(tem0_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(tem0_sl))], "T")

data_avail[which(stat_meta$stn %in% colnames(radi_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(radi_sl))], "-R")

data_avail[which(stat_meta$stn %in% colnames(suns_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(suns_sl))], "-S")

data_avail[which(stat_meta$stn %in% colnames(clou_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(clou_sl))], "-C")

data_avail[which(stat_meta$stn %in% colnames(ahum_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(ahum_sl))], "-H")

data_avail[which(stat_meta$stn %in% colnames(airp_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(airp_sl))], "-P")

data_avail[which(stat_meta$stn %in% colnames(snow_sl))] <-
  paste0(data_avail[which(stat_meta$stn %in% colnames(snow_sl))], "-D")

cbind(as.character(stat_meta$name), data_avail)


# rm(ahum_data, airp_data, clou_data, data_stat, out_data, radi_data,
#    snow_data, stat_data, suns_data, tem0_data, temn_data, temx_data, data_avail,
#    base_dir)


#WTC_GWT_26####
gwt26_data <- read.table(paste0(base_dir, "rawData/IDAweb/weastat/order59752/order_59752_data.txt"),
                         sep = ";", skip = 1, header = TRUE, na.strings = "-")

gwt26_data$time <- as.POSIXct(strptime(gwt26_data$time, "%Y%m%d", tz="UTC"))

start_day <- "1981-01-01"
end_day   <- "2017-12-31"

start_date <- as.POSIXct(strptime(start_day, "%Y-%m-%d", tz="UTC"))
end_date   <- as.POSIXct(strptime(end_day,   "%Y-%m-%d", tz="UTC"))
full_date  <- seq(start_date, end_date, by="day")

data_gwt26 <- data.frame(date = full_date,
                         value = with(gwt26_data, gwt26_data$wkwtg3d0[match(full_date, time)]))


gwt_med <- function(dates, clim_data, gwt_data){

  input_data <- data.frame(dates = dates,
                           clim = clim_data,
                           gwt = gwt_data)


  #Remove 29th of February
  input_data <- input_data[-which(format(input_data$dates, "%m%d") == "0229"),]

  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  days <- format(days,"%m-%d")

  #Order climate data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year

  for(i in 0:(length(start_year:end_year)-1)) {

    data_day[i+1, 2:366] <- input_data$clim[(i*365+1):((i+1)*365)]

  }

  data_day_clim <- data_day

  #Order gwt data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", days)
  data_day[ ,1] <- start_year:end_year

  for(i in 0:(length(start_year:end_year)-1)) {

    data_day[i+1, 2:366] <- input_data$gwt[(i*365+1):((i+1)*365)]

  }

  data_day_gwt <- data_day


for(k in 1:26){

  for (i in 2:ncol(data_day_clim)) {

    gwt_days <- which(data_day_gwt[, i] == k)
    gwt_days_med <- median(data_day_clim[gwt_days, i])

    if(i == 2){
      gwt_med <- gwt_days_med
    }else
      gwt_med <- c(gwt_med, gwt_days_med)
  }

  if(k ==1){
    gwt_out <- gwt_med
  }else{
    gwt_out <- cbind(gwt_out, gwt_med)
  }

}

  colnames(gwt_out) <- 1:26
  return(gwt_out)

}

#Swiss average
stat_cols_tem0 <- which(colnames(tem0_data) %in% colnames(tem0_sl))

tem0_use <- tem0_data[, stat_cols_tem0]

tem0_4_gwt <- apply(tem0_use, 1, med_na)

gwt_tem0 <- gwt_med(dates = tem0_data$date, clim_data = tem0_4_gwt, gwt_data = gwt26_data$wkwtg3d0)

#get rank out of mean values

gwt_rank_tem0 <- matrix(NA, ncol = 26, nrow = 365)

for (i in 1:365) {

  gwt_tem0_sort <- sort(gwt_tem0[i, ])

 if(length(gwt_tem0_sort) > 9){
   gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:5])
   gwt_warm <- as.numeric(names(gwt_tem0_sort)[(length(gwt_tem0_sort)-5) : length(gwt_tem0_sort)])
 }else{
   is.even <- function(x) {x %% 2 == 0}
   if(is.even(length(gwt_clim_sort))){
     gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(length(gwt_tem0_sort) / 2)])
     gwt_warm <- as.numeric(names(gwt_tem0_sort)[((length(gwt_tem0_sort) / 2) + 1) : length(gwt_tem0_sort)])
   }else{
     gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(floor(length(gwt_tem0_sort) / 2))])
     gwt_warm <- as.numeric(names(gwt_tem0_sort)[ceiling((length(gwt_tem0_sort) / 2)) : length(gwt_tem0_sort)])
   }
 }
 gwt_rank_tem0[i, gwt_cold] <-  -1
 gwt_rank_tem0[i, gwt_warm] <-   1

}

#Determine driving weather types
gwt_sums_tem0 <- apply(gwt_rank_tem0[,], 2, sum_na)

names(gwt_sums_tem0) <- 1:26

gwt_sums_tem0_sort <- sort(gwt_sums_tem0)

gwt_lows_tem0  <- 1:5
gwt_highs_tem0 <- 22:26

gwt_low_tem0   <- as.numeric(names(gwt_sums_tem0_sort)[gwt_lows_tem0])
gwt_high_tem0  <- as.numeric(names(gwt_sums_tem0_sort)[gwt_highs_tem0])

#Calculate changes in frequencies
gwt_tem0_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_tem0)*100*10# [%/dec]

gwt_tem0_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_tem0)*100*10 # [%/dec]



