###

#Data analyses

###

#Settings----

pacman::p_load(alptempr, zoo, modifiedmk, zyp, shape, foreach, 
               parallel, doParallel, pbapply, RColorBrewer)

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
load(paste0(base_dir,"pva200d0.RData")) ; wvap_data <- out_data #Water vapor pressure

start_year   <- 1981
end_year     <- 2017
window_width <- 30
cover_thres  <- 32/37

#Make cluster for parallel computing
n_cores <- 45 #define number of cores used
my_clust <- makeCluster(n_cores)
clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, alptempr))
registerDoParallel(my_clust)

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

#Meteo_Calc----

#calculate trend magnitude using Sen's Slope (per decade)
#only keep stations which had sufficient data coverage
tem0_sl <- foreach(i = 2:ncol(tem0_data), .combine = 'cbind') %dopar% {
  
  f_sl(tem0_data[, i]) * 10 #[°C/dec]
  
}
colnames(tem0_sl) <- colnames(tem0_data)[-1] ; tem0_sl <- as.data.frame(stat_coverage(tem0_sl))

temx_sl <- foreach(i = 2:ncol(temx_data), .combine = 'cbind') %dopar% {
  
  f_sl(temx_data[, i]) * 10 #[°C/dec]
  
}
colnames(temx_sl) <- colnames(temx_data)[-1] ; temx_sl <- as.data.frame(stat_coverage(temx_sl))

temn_sl <- foreach(i = 2:ncol(temn_data), .combine = 'cbind') %dopar% {
  
  f_sl(temn_data[, i]) * 10 #[°C/dec]
  
}
colnames(temn_sl) <- colnames(temn_data)[-1] ; temn_sl <- as.data.frame(stat_coverage(temn_sl))

suns_sl <- foreach(i = 2:ncol(suns_data), .combine = 'cbind') %dopar% {
  
  f_sl(suns_data[, i]) * 10 #[min/dec]
  
}
colnames(suns_sl) <- colnames(suns_data)[-1] ; suns_sl <- as.data.frame(stat_coverage(suns_sl))

radi_sl <- foreach(i = 2:ncol(radi_data), .combine = 'cbind') %dopar% {
  
  f_sl(radi_data[, i]) * 10 #[W/m²/dec]
  
}
colnames(radi_sl) <- colnames(radi_data)[-1] ; radi_sl <- as.data.frame(stat_coverage(radi_sl))

clou_sl <- foreach(i = 2:ncol(clou_data), .combine = 'cbind') %dopar% {
  
  f_sl(clou_data[, i]) * 10 #[%/dec]
  
}
colnames(clou_sl) <- colnames(clou_data)[-1] ; clou_sl <- as.data.frame(stat_coverage(clou_sl))

ahum_sl <- foreach(i = 2:ncol(ahum_data), .combine = 'cbind') %dopar% {
  
  f_sl(ahum_data[, i]) * 10 #[g/cm³/dec]
  
}
colnames(ahum_sl) <- colnames(ahum_data)[-1] ; ahum_sl <- as.data.frame(stat_coverage(ahum_sl))

airp_sl <- foreach(i = 2:ncol(airp_data), .combine = 'cbind') %dopar% {
  
  f_sl(airp_data[, i]) * 10 #[hPa/dec]
  
}
colnames(airp_sl) <- colnames(airp_data)[-1] ; airp_sl <- as.data.frame(stat_coverage(airp_sl))

snow_sl <- foreach(i = 2:ncol(snow_data), .combine = 'cbind') %dopar% {
  
  f_sl_sn(snow_data[, i]) * 10 * 100 #[%/dec]
  
}
colnames(snow_sl) <- colnames(snow_data)[-1] ; snow_sl <- as.data.frame(stat_coverage(snow_sl))

wvap_sl <- foreach(i = 2:ncol(wvap_data), .combine = 'cbind') %dopar% {
  
  f_sl(wvap_data[, i]) * 10 * 100 #[%/dec]
  
}
colnames(wvap_sl) <- colnames(wvap_data)[-1] ; wvap_sl <- as.data.frame(stat_coverage(wvap_sl))

# tem0_sl <- as.data.frame(apply(tem0_data[, -1], 2 , f_sl))*10 ; tem0_sl <- stat_coverage(tem0_sl)
# temx_sl <- as.data.frame(apply(temx_data[, -1], 2 , f_sl))*10 ; temx_sl <- stat_coverage(temx_sl)
# temn_sl <- as.data.frame(apply(temn_data[, -1], 2 , f_sl))*10 ; temn_sl <- stat_coverage(temn_sl)
# suns_sl <- as.data.frame(apply(suns_data[, -1], 2 , f_sl))*10 ; suns_sl <- stat_coverage(suns_sl)
# radi_sl <- as.data.frame(apply(radi_data[, -1], 2 , f_sl))*10 ; radi_sl <- stat_coverage(radi_sl)
# clou_sl <- as.data.frame(apply(clou_data[, -1], 2 , f_sl))*10 ; clou_sl <- stat_coverage(clou_sl)
# ahum_sl <- as.data.frame(apply(ahum_data[, -1], 2 , f_sl))*10 ; ahum_sl <- stat_coverage(ahum_sl)
# airp_sl <- as.data.frame(apply(airp_data[, -1], 2 , f_sl))*10 ; airp_sl <- stat_coverage(airp_sl)
# snow_sl <- as.data.frame(apply(snow_data[, -1], 2 , f_sl_sn))*10*100 ; snow_sl <- stat_coverage(snow_sl)

#Calculate singificance of trends using Mann Kendall
tem0_mk <- foreach(i = 2:ncol(tem0_data), .combine = 'cbind') %dopar% {
  
  f_mk(tem0_data[, i])
  
}
colnames(tem0_mk) <- colnames(tem0_data)[-1] ; tem0_mk <- as.data.frame(stat_coverage(tem0_mk))

temx_mk <- foreach(i = 2:ncol(temx_data), .combine = 'cbind') %dopar% {
  
  f_mk(temx_data[, i])
  
}
colnames(temx_mk) <- colnames(temx_data)[-1] ; temx_mk <- as.data.frame(stat_coverage(temx_mk))

temn_mk <- foreach(i = 2:ncol(temn_data), .combine = 'cbind') %dopar% {
  
  f_mk(temn_data[, i])
  
}
colnames(temn_mk) <- colnames(temn_data)[-1] ; temn_mk <- as.data.frame(stat_coverage(temn_mk))

suns_mk <- foreach(i = 2:ncol(suns_data), .combine = 'cbind') %dopar% {
  
  f_mk(suns_data[, i])
  
}
colnames(suns_mk) <- colnames(suns_data)[-1] ; suns_mk <- as.data.frame(stat_coverage(suns_mk))

radi_mk <- foreach(i = 2:ncol(radi_data), .combine = 'cbind') %dopar% {
  
  f_mk(radi_data[, i])
  
}
colnames(radi_mk) <- colnames(radi_data)[-1] ; radi_mk <- as.data.frame(stat_coverage(radi_mk))

clou_mk <- foreach(i = 2:ncol(clou_data), .combine = 'cbind') %dopar% {
  
  f_mk(clou_data[, i])
  
}
colnames(clou_mk) <- colnames(clou_data)[-1] ; clou_mk <- as.data.frame(stat_coverage(clou_mk))

ahum_mk <- foreach(i = 2:ncol(ahum_data), .combine = 'cbind') %dopar% {
  
  f_mk(ahum_data[, i])
  
}
colnames(ahum_mk) <- colnames(ahum_data)[-1] ; ahum_mk <- as.data.frame(stat_coverage(ahum_mk))

airp_mk <- foreach(i = 2:ncol(airp_data), .combine = 'cbind') %dopar% {
  
  f_mk(airp_data[, i])
  
}
colnames(airp_mk) <- colnames(airp_data)[-1] ; airp_mk <- as.data.frame(stat_coverage(airp_mk))

snow_mk <- foreach(i = 2:ncol(snow_data), .combine = 'cbind') %dopar% {
  
  f_mk_sn(snow_data[, i])
  
}
colnames(snow_mk) <- colnames(snow_data)[-1] ; snow_mk <- as.data.frame(stat_coverage(snow_mk))

# tem0_mk <- as.data.frame(apply(tem0_data[, -1], 2 , f_mk)) ; tem0_mk <- stat_coverage(tem0_mk)
# temx_mk <- as.data.frame(apply(temx_data[, -1], 2 , f_mk)) ; temx_mk <- stat_coverage(temx_mk)
# temn_mk <- as.data.frame(apply(temn_data[, -1], 2 , f_mk)) ; temn_mk <- stat_coverage(temn_mk)
# suns_mk <- as.data.frame(apply(suns_data[, -1], 2 , f_mk)) ; suns_mk <- stat_coverage(suns_mk)
# radi_mk <- as.data.frame(apply(radi_data[, -1], 2 , f_mk)) ; radi_mk <- stat_coverage(radi_mk)
# clou_mk <- as.data.frame(apply(clou_data[, -1], 2 , f_mk)) ; clou_mk <- stat_coverage(clou_mk)
# ahum_mk <- as.data.frame(apply(ahum_data[, -1], 2 , f_mk)) ; ahum_mk <- stat_coverage(ahum_mk)
# airp_mk <- as.data.frame(apply(airp_data[, -1], 2 , f_mk)) ; airp_mk <- stat_coverage(airp_mk)
# snow_mk <- as.data.frame(apply(snow_data[, -1], 2 , f_mk_sn)) ; snow_mk <- stat_coverage(snow_mk)

#Calculate mean values
tem0_me <- foreach(i = 2:ncol(tem0_data), .combine = 'cbind') %dopar% {
  
  f_me(tem0_data[, i])
  
}
colnames(tem0_me) <- colnames(tem0_data)[-1] ; tem0_me <- as.data.frame(stat_coverage(tem0_me))

temx_me <- foreach(i = 2:ncol(temx_data), .combine = 'cbind') %dopar% {
  
  f_me(temx_data[, i])
  
}
colnames(temx_me) <- colnames(temx_data)[-1] ; temx_me <- as.data.frame(stat_coverage(temx_me))

temn_me <- foreach(i = 2:ncol(temn_data), .combine = 'cbind') %dopar% {
  
  f_me(temn_data[, i])
  
}
colnames(temn_me) <- colnames(temn_data)[-1] ; temn_me <- as.data.frame(stat_coverage(temn_me))

suns_me <- foreach(i = 2:ncol(suns_data), .combine = 'cbind') %dopar% {
  
  f_me(suns_data[, i])
  
}
colnames(suns_me) <- colnames(suns_data)[-1] ; suns_me <- as.data.frame(stat_coverage(suns_me))

radi_me <- foreach(i = 2:ncol(radi_data), .combine = 'cbind') %dopar% {
  
  f_me(radi_data[, i])
  
}
colnames(radi_me) <- colnames(radi_data)[-1] ; radi_me <- as.data.frame(stat_coverage(radi_me))

clou_me <- foreach(i = 2:ncol(clou_data), .combine = 'cbind') %dopar% {
  
  f_me(clou_data[, i])
  
}
colnames(clou_me) <- colnames(clou_data)[-1] ; clou_me <- as.data.frame(stat_coverage(clou_me))

ahum_me <- foreach(i = 2:ncol(ahum_data), .combine = 'cbind') %dopar% {
  
  f_me(ahum_data[, i])
  
}
colnames(ahum_me) <- colnames(ahum_data)[-1] ; ahum_me <- as.data.frame(stat_coverage(ahum_me))

airp_me <- foreach(i = 2:ncol(airp_data), .combine = 'cbind') %dopar% {
  
  f_me(airp_data[, i])
  
}
colnames(airp_me) <- colnames(airp_data)[-1] ; airp_me <- as.data.frame(stat_coverage(airp_me))

snow_me <- foreach(i = 2:ncol(snow_data), .combine = 'cbind') %dopar% {
  
  f_me(snow_data[, i])
  
}
colnames(snow_me) <- colnames(snow_data)[-1] ; snow_me <- as.data.frame(stat_coverage(snow_me))

# tem0_me <- as.data.frame(apply(tem0_data[, -1], 2 , f_me)) ; tem0_me <- stat_coverage(tem0_me)
# temx_me <- as.data.frame(apply(temx_data[, -1], 2 , f_me)) ; temx_me <- stat_coverage(temx_me)
# temn_me <- as.data.frame(apply(temn_data[, -1], 2 , f_me)) ; temn_me <- stat_coverage(temn_me)
# suns_me <- as.data.frame(apply(suns_data[, -1], 2 , f_me)) ; suns_me <- stat_coverage(suns_me)
# radi_me <- as.data.frame(apply(radi_data[, -1], 2 , f_me)) ; radi_me <- stat_coverage(radi_me)
# clou_me <- as.data.frame(apply(clou_data[, -1], 2 , f_me)) ; clou_me <- stat_coverage(clou_me)
# ahum_me <- as.data.frame(apply(ahum_data[, -1], 2 , f_me)) ; ahum_me <- stat_coverage(ahum_me)
# airp_me <- as.data.frame(apply(airp_data[, -1], 2 , f_me)) ; airp_me <- stat_coverage(airp_me)
# snow_me <- as.data.frame(apply(snow_data[, -1], 2 , f_me)) ; snow_me <- stat_coverage(snow_me)

#Snow likelihood; probability of snow being present on e.g. 15th of February
snow_li <- foreach(i = 2:ncol(snow_data), .combine = 'cbind') %dopar% {
  
  f_li(snow_data[, i]) * 100 # [%]
  
}
colnames(snow_li) <- colnames(snow_data)[-1] ; snow_li <- as.data.frame(stat_coverage(snow_li))
# snow_li   <- as.data.frame(apply(snow_data[,-1], 2 , f_li))*100 # [%]

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

#remove stations from sta_meta that are not in data
stat_meta <- stat_meta[-which(!stat_meta$stn %in% stats_data),]


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


#WTC_GWT_26_tem0_regis####
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

#Temperature for ranking weather types

#whole Switzerland
# stat_cols_tem0 <- which(colnames(tem0_data) %in% colnames(tem0_sl))

#selected region
clim_regions <- c("Jura", "Plateau", "Alps", "S_Alps")

f_wtc_score <- function(regi_sel){
  
  stat_reg_sel <- colnames(tem0_sl)[which(colnames(tem0_sl) %in% stat_meta$stn[which(stat_meta$clim_reg == regi_sel)])]
  stat_cols_tem0 <- which(colnames(tem0_data) %in% stat_reg_sel)
  
  tem0_use <- tem0_data[, stat_cols_tem0]
  
  tem0_4_gwt <- apply(tem0_use, 1, med_na)
  
  tem0_4_gwt_slo <- f_sl(tem0_4_gwt)
  
  gwt_tem0 <- gwt_med(dates = tem0_data$date, clim_data = tem0_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_tem0 <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_tem0_sort <- sort(gwt_tem0[i, ])
    # print(length(gwt_tem0_sort))
    # gwt_tem0_sort <- gwt_tem0_sort[1:15]
    
    if(length(gwt_tem0_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_tem0_sort)[(length(gwt_tem0_sort) - num_hig_sel + 1) : length(gwt_tem0_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_tem0_sort))){
        gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(length(gwt_tem0_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_tem0_sort)[((length(gwt_tem0_sort) / 2) + 1) : length(gwt_tem0_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(floor(length(gwt_tem0_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_tem0_sort)[(ceiling((length(gwt_tem0_sort) / 2)) + 1) : length(gwt_tem0_sort)])
      }
    }
    # gwt_rank_tem0[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_tem0[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_tem0[i, gwt_cold] <-  -1
    gwt_rank_tem0[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_tem0_low <- apply(gwt_rank_tem0[ , ], 2, f_sum_neg)
  gwt_sums_tem0_hig <- apply(gwt_rank_tem0[ , ], 2, f_sum_pos)
  gwt_sums_tem0     <- apply(gwt_rank_tem0[ , ], 2, sum_na)
  
  names(gwt_sums_tem0_low) <- 1:26
  names(gwt_sums_tem0_hig) <- 1:26
  names(gwt_sums_tem0)     <- 1:26
  
  gwt_sums_tem0__low_sort <- sort(gwt_sums_tem0_low)
  gwt_sums_tem0__hig_sort <- sort(gwt_sums_tem0_hig)
  gwt_sums_tem0_sort      <- sort(gwt_sums_tem0)
  
  gwt_score_out <- cbind(gwt_sums_tem0_low, gwt_sums_tem0_hig, gwt_sums_tem0)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), regi_sel)
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_tem0 <- foreach(i = 1:length(clim_regions), .combine = 'cbind') %dopar% {
  
  f_wtc_score(clim_regions[i])
  
}

# gwt_lows_tem0  <- 1:5
# gwt_highs_tem0 <- 21:26
# gwt_low_tem0   <- as.numeric(names(gwt_sums_tem0_sort)[gwt_lows_tem0])
# gwt_high_tem0  <- as.numeric(names(gwt_sums_tem0_sort)[gwt_highs_tem0])

gwt_low_tem0 <- c(1:8, 25)
gwt_high_tem0 <- c(9:16, 26)

#Calculate changes in frequencies
gwt_tem0_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_tem0)*100*10# [%/dec]

gwt_tem0_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_tem0)*100*10 # [%/dec]


#WTC_GWT_26_ahum####
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

#Humidity for ranking weather types

#whole Switzerland
# stat_cols_ahum <- which(colnames(ahum_data) %in% colnames(ahum_sl))

#selected region
clim_regions <- c("Jura", "Plateau", "Alps", "S_Alps")

f_wtc_score <- function(regi_sel){
  
  stat_reg_sel <- colnames(ahum_sl)[which(colnames(ahum_sl) %in% stat_meta$stn[which(stat_meta$clim_reg == regi_sel)])]
  stat_cols_ahum <- which(colnames(ahum_data) %in% stat_reg_sel)
  
  ahum_use <- ahum_data[, stat_cols_ahum]
  
  ahum_4_gwt <- apply(ahum_use, 1, med_na)
  
  ahum_4_gwt_slo <- f_sl(ahum_4_gwt)
  
  gwt_ahum <- gwt_med(dates = ahum_data$date, clim_data = ahum_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_ahum <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_ahum_sort <- sort(gwt_ahum[i, ])
    # print(length(gwt_ahum_sort))
    # gwt_ahum_sort <- gwt_ahum_sort[1:15]
    
    if(length(gwt_ahum_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_ahum_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_ahum_sort)[(length(gwt_ahum_sort) - num_hig_sel + 1) : length(gwt_ahum_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_ahum_sort))){
        gwt_cold <- as.numeric(names(gwt_ahum_sort)[1:(length(gwt_ahum_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_ahum_sort)[((length(gwt_ahum_sort) / 2) + 1) : length(gwt_ahum_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_ahum_sort)[1:(floor(length(gwt_ahum_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_ahum_sort)[(ceiling((length(gwt_ahum_sort) / 2)) + 1) : length(gwt_ahum_sort)])
      }
    }
    # gwt_rank_ahum[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_ahum[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_ahum[i, gwt_cold] <-  -1
    gwt_rank_ahum[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_ahum_low <- apply(gwt_rank_ahum[ , ], 2, f_sum_neg)
  gwt_sums_ahum_hig <- apply(gwt_rank_ahum[ , ], 2, f_sum_pos)
  gwt_sums_ahum     <- apply(gwt_rank_ahum[ , ], 2, sum_na)
  
  names(gwt_sums_ahum_low) <- 1:26
  names(gwt_sums_ahum_hig) <- 1:26
  names(gwt_sums_ahum)     <- 1:26
  
  gwt_sums_ahum__low_sort <- sort(gwt_sums_ahum_low)
  gwt_sums_ahum__hig_sort <- sort(gwt_sums_ahum_hig)
  gwt_sums_ahum_sort      <- sort(gwt_sums_ahum)
  
  gwt_score_out <- cbind(gwt_sums_ahum_low, gwt_sums_ahum_hig, gwt_sums_ahum)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), regi_sel)
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_ahum <- foreach(i = 1:length(clim_regions), .combine = 'cbind') %dopar% {
  
  f_wtc_score(clim_regions[i])
  
}

# gwt_lows_ahum  <- 1:5
# gwt_highs_ahum <- 21:26
# gwt_low_ahum   <- as.numeric(names(gwt_sums_ahum_sort)[gwt_lows_ahum])
# gwt_high_ahum  <- as.numeric(names(gwt_sums_ahum_sort)[gwt_highs_ahum])

gwt_low_ahum <- c(1,2,3,4,5,6,19,20,21,25)
gwt_high_ahum <- c(9,10,11,15,16,17,18,23,24,26)

#Calculate changes in frequencies
gwt_ahum_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_ahum)*100*10# [%/dec]

gwt_ahum_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_ahum)*100*10 # [%/dec]


#WTC_GWT_26_suns####
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

#Humidity for ranking weather types

#whole Switzerland
# stat_cols_suns <- which(colnames(suns_data) %in% colnames(suns_sl))

#selected region
clim_regions <- c("Jura", "Plateau", "Alps", "S_Alps")

f_wtc_score <- function(regi_sel){
  
  stat_reg_sel <- colnames(suns_sl)[which(colnames(suns_sl) %in% stat_meta$stn[which(stat_meta$clim_reg == regi_sel)])]
  stat_cols_suns <- which(colnames(suns_data) %in% stat_reg_sel)
  
  suns_use <- suns_data[, stat_cols_suns]
  
  suns_4_gwt <- apply(suns_use, 1, med_na)
  
  suns_4_gwt_slo <- f_sl(suns_4_gwt)
  
  gwt_suns <- gwt_med(dates = suns_data$date, clim_data = suns_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_suns <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_suns_sort <- sort(gwt_suns[i, ])
    # print(length(gwt_suns_sort))
    # gwt_suns_sort <- gwt_suns_sort[1:15]
    
    if(length(gwt_suns_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_suns_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_suns_sort)[(length(gwt_suns_sort) - num_hig_sel + 1) : length(gwt_suns_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_suns_sort))){
        gwt_cold <- as.numeric(names(gwt_suns_sort)[1:(length(gwt_suns_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_suns_sort)[((length(gwt_suns_sort) / 2) + 1) : length(gwt_suns_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_suns_sort)[1:(floor(length(gwt_suns_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_suns_sort)[(ceiling((length(gwt_suns_sort) / 2)) + 1) : length(gwt_suns_sort)])
      }
    }
    # gwt_rank_suns[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_suns[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_suns[i, gwt_cold] <-  -1
    gwt_rank_suns[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_suns_low <- apply(gwt_rank_suns[ , ], 2, f_sum_neg)
  gwt_sums_suns_hig <- apply(gwt_rank_suns[ , ], 2, f_sum_pos)
  gwt_sums_suns     <- apply(gwt_rank_suns[ , ], 2, sum_na)
  
  names(gwt_sums_suns_low) <- 1:26
  names(gwt_sums_suns_hig) <- 1:26
  names(gwt_sums_suns)     <- 1:26
  
  gwt_sums_suns__low_sort <- sort(gwt_sums_suns_low)
  gwt_sums_suns__hig_sort <- sort(gwt_sums_suns_hig)
  gwt_sums_suns_sort      <- sort(gwt_sums_suns)
  
  gwt_score_out <- cbind(gwt_sums_suns_low, gwt_sums_suns_hig, gwt_sums_suns)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), regi_sel)
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_suns <- foreach(i = 1:length(clim_regions), .combine = 'cbind') %dopar% {
  
  f_wtc_score(clim_regions[i])
  
}

# gwt_lows_suns  <- 1:5
# gwt_highs_suns <- 21:26
# gwt_low_suns   <- as.numeric(names(gwt_sums_suns_sort)[gwt_lows_suns])
# gwt_high_suns  <- as.numeric(names(gwt_sums_suns_sort)[gwt_highs_suns])

gwt_low_suns <- c(1,2,3,4,5,6,19,20,21,25)
gwt_high_suns <- c(9,10,11,15,16,17,18,23,24,26)

#Calculate changes in frequencies
gwt_suns_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_suns)*100*10# [%/dec]

gwt_suns_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_suns)*100*10 # [%/dec]


#WTC_GWT_26_radi####
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

#Humidity for ranking weather types

#whole Switzerland
# stat_cols_radi <- which(colnames(radi_data) %in% colnames(radi_sl))

#selected region
clim_regions <- c("Jura", "Plateau", "Alps", "S_Alps")

f_wtc_score <- function(regi_sel){
  
  stat_reg_sel <- colnames(radi_sl)[which(colnames(radi_sl) %in% stat_meta$stn[which(stat_meta$clim_reg == regi_sel)])]
  stat_cols_radi <- which(colnames(radi_data) %in% stat_reg_sel)
  
  radi_use <- radi_data[, stat_cols_radi]
  
  radi_4_gwt <- apply(radi_use, 1, med_na)
  
  radi_4_gwt_slo <- f_sl(radi_4_gwt)
  
  gwt_radi <- gwt_med(dates = radi_data$date, clim_data = radi_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_radi <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_radi_sort <- sort(gwt_radi[i, ])
    # print(length(gwt_radi_sort))
    # gwt_radi_sort <- gwt_radi_sort[1:15]
    
    if(length(gwt_radi_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_radi_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_radi_sort)[(length(gwt_radi_sort) - num_hig_sel + 1) : length(gwt_radi_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_radi_sort))){
        gwt_cold <- as.numeric(names(gwt_radi_sort)[1:(length(gwt_radi_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_radi_sort)[((length(gwt_radi_sort) / 2) + 1) : length(gwt_radi_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_radi_sort)[1:(floor(length(gwt_radi_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_radi_sort)[(ceiling((length(gwt_radi_sort) / 2)) + 1) : length(gwt_radi_sort)])
      }
    }
    # gwt_rank_radi[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_radi[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_radi[i, gwt_cold] <-  -1
    gwt_rank_radi[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_radi_low <- apply(gwt_rank_radi[ , ], 2, f_sum_neg)
  gwt_sums_radi_hig <- apply(gwt_rank_radi[ , ], 2, f_sum_pos)
  gwt_sums_radi     <- apply(gwt_rank_radi[ , ], 2, sum_na)
  
  names(gwt_sums_radi_low) <- 1:26
  names(gwt_sums_radi_hig) <- 1:26
  names(gwt_sums_radi)     <- 1:26
  
  gwt_sums_radi__low_sort <- sort(gwt_sums_radi_low)
  gwt_sums_radi__hig_sort <- sort(gwt_sums_radi_hig)
  gwt_sums_radi_sort      <- sort(gwt_sums_radi)
  
  gwt_score_out <- cbind(gwt_sums_radi_low, gwt_sums_radi_hig, gwt_sums_radi)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), regi_sel)
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_radi <- foreach(i = 1:length(clim_regions), .combine = 'cbind') %dopar% {
  
  f_wtc_score(clim_regions[i])
  
}

# gwt_lows_radi  <- 1:5
# gwt_highs_radi <- 21:26
# gwt_low_radi   <- as.numeric(names(gwt_sums_radi_sort)[gwt_lows_radi])
# gwt_high_radi  <- as.numeric(names(gwt_sums_radi_sort)[gwt_highs_radi])

gwt_low_radi <- c(1,2, 7,8, 10, 17,18, 23,24, 25)
gwt_high_radi <- c(4,5, 11,12,13,14,15, 19,20,21,22, 26)

#Calculate changes in frequencies
gwt_radi_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_radi)*100*10# [%/dec]

gwt_radi_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_radi)*100*10 # [%/dec]


#WTC_GWT_26_wvap####
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

#Humidity for ranking weather types

#whole Switzerland
# stat_cols_wvap <- which(colnames(wvap_data) %in% colnames(wvap_sl))

#selected region
clim_regions <- c("Jura", "Plateau", "Alps", "S_Alps")

f_wtc_score <- function(regi_sel){
  
  stat_reg_sel <- colnames(wvap_sl)[which(colnames(wvap_sl) %in% stat_meta$stn[which(stat_meta$clim_reg == regi_sel)])]
  stat_cols_wvap <- which(colnames(wvap_data) %in% stat_reg_sel)
  
  wvap_use <- wvap_data[, stat_cols_wvap]
  
  wvap_4_gwt <- apply(wvap_use, 1, med_na)
  
  wvap_4_gwt_slo <- f_sl(wvap_4_gwt)
  
  gwt_wvap <- gwt_med(dates = wvap_data$date, clim_data = wvap_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_wvap <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_wvap_sort <- sort(gwt_wvap[i, ])
    # print(length(gwt_wvap_sort))
    # gwt_wvap_sort <- gwt_wvap_sort[1:15]
    
    if(length(gwt_wvap_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_wvap_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_wvap_sort)[(length(gwt_wvap_sort) - num_hig_sel + 1) : length(gwt_wvap_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_wvap_sort))){
        gwt_cold <- as.numeric(names(gwt_wvap_sort)[1:(length(gwt_wvap_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_wvap_sort)[((length(gwt_wvap_sort) / 2) + 1) : length(gwt_wvap_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_wvap_sort)[1:(floor(length(gwt_wvap_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_wvap_sort)[(ceiling((length(gwt_wvap_sort) / 2)) + 1) : length(gwt_wvap_sort)])
      }
    }
    # gwt_rank_wvap[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_wvap[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_wvap[i, gwt_cold] <-  -1
    gwt_rank_wvap[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_wvap_low <- apply(gwt_rank_wvap[ , ], 2, f_sum_neg)
  gwt_sums_wvap_hig <- apply(gwt_rank_wvap[ , ], 2, f_sum_pos)
  gwt_sums_wvap     <- apply(gwt_rank_wvap[ , ], 2, sum_na)
  
  names(gwt_sums_wvap_low) <- 1:26
  names(gwt_sums_wvap_hig) <- 1:26
  names(gwt_sums_wvap)     <- 1:26
  
  gwt_sums_wvap__low_sort <- sort(gwt_sums_wvap_low)
  gwt_sums_wvap__hig_sort <- sort(gwt_sums_wvap_hig)
  gwt_sums_wvap_sort      <- sort(gwt_sums_wvap)
  
  gwt_score_out <- cbind(gwt_sums_wvap_low, gwt_sums_wvap_hig, gwt_sums_wvap)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), regi_sel)
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_wvap <- foreach(i = 1:length(clim_regions), .combine = 'cbind') %dopar% {
  
  f_wtc_score(clim_regions[i])
  
}

# gwt_lows_wvap  <- 1:5
# gwt_highs_wvap <- 21:26
# gwt_low_wvap   <- as.numeric(names(gwt_sums_wvap_sort)[gwt_lows_wvap])
# gwt_high_wvap  <- as.numeric(names(gwt_sums_wvap_sort)[gwt_highs_wvap])

gwt_low_wvap <- c(1,2,3,4,5,6,19,20,21,25)
gwt_high_wvap <- c(9,10,11,15,16,17,18,23,24,26)

#Calculate changes in frequencies
gwt_wvap_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_wvap)*100*10# [%/dec]

gwt_wvap_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_wvap)*100*10 # [%/dec]


#WTC_GWT_26_tem0_elevs####
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

#Temperature for ranking weather types

#whole Switzerland
# stat_cols_tem0 <- which(colnames(tem0_data) %in% colnames(tem0_sl))

#selected elevation categories
eleva_catego <- matrix(c("low",     NA, NA, NA,
                         "middle",  NA, NA, NA,
                         "high",    NA, NA, NA),
                       nrow = 4,
                       ncol = 3)
# eleva_catego <- matrix(c("low",     NA,       NA, NA,
#                          "middle",  "high",   NA, NA,
#                          NA,        NA,       NA, NA),
#                        nrow = 4,
#                        ncol = 3)
regi_select <-  matrix(c("Jura", "Plateau", "Alps", "S_Alps",
                         "Jura", "Plateau", "Alps", "S_Alps",
                         "Jura", "Plateau", "Alps", "S_Alps"),
                       nrow = 4,
                       ncol = 3)
# regi_select <-  matrix(c(NA, "Plateau", "Alps", NA,
#                          NA, "Plateau", "Alps", NA,
#                          NA, "Plateau", "Alps", NA),
#                        nrow = 4,
#                        ncol = 3)

f_wtc_score <- function(col_sel){
  
  stat_ele_sel <- colnames(tem0_sl)[which(colnames(tem0_sl) %in% stat_meta$stn[which(stat_meta$category == eleva_catego[, col_sel])])]
  stat_ele_sel <- stat_ele_sel[which(stat_ele_sel %in% stat_meta$stn[which(stat_meta$clim_reg %in% regi_select[, col_sel])])]
  stat_cols_tem0 <- which(colnames(tem0_data) %in% stat_ele_sel)
  
  tem0_use <- tem0_data[, stat_cols_tem0]
  
  tem0_4_gwt <- apply(tem0_use, 1, med_na)
  
  tem0_4_gwt_slo <- f_sl(tem0_4_gwt)
  
  gwt_tem0 <- gwt_med(dates = tem0_data$date, clim_data = tem0_4_gwt, gwt_data = data_gwt26$value)
  
  #get rank out of mean values
  num_hig_sel <- 15
  num_low_sel <- 15
  gwt_rank_tem0 <- matrix(NA, ncol = 26, nrow = 365)
  
  for (i in 1:365) {
    
    gwt_tem0_sort <- sort(gwt_tem0[i, ])
    # print(length(gwt_tem0_sort))
    # gwt_tem0_sort <- gwt_tem0_sort[1:15]
    
    if(length(gwt_tem0_sort) > sum(num_hig_sel, num_low_sel)){
      gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:num_low_sel])
      gwt_warm <- as.numeric(names(gwt_tem0_sort)[(length(gwt_tem0_sort) - num_hig_sel + 1) : length(gwt_tem0_sort)])
    }else{
      is.even <- function(x) {x %% 2 == 0}
      if(is.even(length(gwt_tem0_sort))){
        gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(length(gwt_tem0_sort) / 2)])
        gwt_warm <- as.numeric(names(gwt_tem0_sort)[((length(gwt_tem0_sort) / 2) + 1) : length(gwt_tem0_sort)])
      }else{
        gwt_cold <- as.numeric(names(gwt_tem0_sort)[1:(floor(length(gwt_tem0_sort) / 2))])
        gwt_warm <- as.numeric(names(gwt_tem0_sort)[(ceiling((length(gwt_tem0_sort) / 2)) + 1) : length(gwt_tem0_sort)])
      }
    }
    # gwt_rank_tem0[i, gwt_cold] <-  (-1 * length(gwt_cold)) : -1
    # gwt_rank_tem0[i, gwt_warm] <-   1 : length(gwt_warm)
    gwt_rank_tem0[i, gwt_cold] <-  -1
    gwt_rank_tem0[i, gwt_warm] <-   1
    
  }
  
  #Determine driving weather types
  f_sum_neg <- function(data_in){
    
    data_in[which(data_in > 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  f_sum_pos <- function(data_in){
    
    data_in[which(data_in < 0)] <- NA
    score_out <- sum_na(data_in)
    
  }
  gwt_sums_tem0_low <- apply(gwt_rank_tem0[ , ], 2, f_sum_neg)
  gwt_sums_tem0_hig <- apply(gwt_rank_tem0[ , ], 2, f_sum_pos)
  gwt_sums_tem0     <- apply(gwt_rank_tem0[ , ], 2, sum_na)
  
  names(gwt_sums_tem0_low) <- 1:26
  names(gwt_sums_tem0_hig) <- 1:26
  names(gwt_sums_tem0)     <- 1:26
  
  gwt_sums_tem0__low_sort <- sort(gwt_sums_tem0_low)
  gwt_sums_tem0__hig_sort <- sort(gwt_sums_tem0_hig)
  gwt_sums_tem0_sort      <- sort(gwt_sums_tem0)
  
  gwt_score_out <- cbind(gwt_sums_tem0_low, gwt_sums_tem0_hig, gwt_sums_tem0)
  colnames(gwt_score_out) <- paste0(c("low_","hig_","net_"), "catego_sel")
  
  return(gwt_score_out)
  
  
}

wtc_score_regis_elev <- foreach(i = 1:ncol(eleva_catego), .combine = 'cbind') %dopar% {
  
  f_wtc_score(i)
  
}

# gwt_lows_tem0  <- 1:5
# gwt_highs_tem0 <- 21:26
# gwt_low_tem0   <- as.numeric(names(gwt_sums_tem0_sort)[gwt_lows_tem0])
# gwt_high_tem0  <- as.numeric(names(gwt_sums_tem0_sort)[gwt_highs_tem0])

gwt_low_elev <- c(1:8, 25)
gwt_high_elev <- c(9:16, 26)

#Calculate changes in frequencies
gwt_elev_high <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_high_elev)*100*10# [%/dec]

gwt_elev_low  <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thresh= cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = gwt_low_elev)*100*10 # [%/dec]


#WTC_number_cor_tem0----

wt_test <- cbind(c(rep(1, 1),    rep(1, 1),   rep(1, 2),   rep(1, 2),   rep(1, 3),   rep(1, 3),  
                   rep(1, 4),    rep(1, 4),   rep(1, 5),   rep(1, 5),   rep(1, 6),   rep(1, 6),  
                   rep(1, 7),    rep(1, 7),   rep(1, 8),   rep(1, 8),   rep(1, 9),   rep(1, 9),
                   rep(1, 10),   rep(1, 10),  rep(1, 11),  rep(1, 11),  rep(1, 12),  rep(1, 12),
                   rep(1, 13),   rep(1, 13)), 
                 c(rep(1, 1),    1:1,         rep(2, 2),   1:2,         rep(3, 3),   1:3,
                   rep(4, 4),    1:4,         rep(5, 5),   1:5,         rep(6, 6),   1:6,
                   rep(7, 7),    1:7,         rep(8, 8),   1:8,         rep(9, 9),   1:9,
                   rep(10, 10),  1:10,        rep(11, 11), 1:11,        rep(12, 12), 1:12,
                   rep(13, 13),  1:13),
                 c(26:26,        rep(26, 1),  25:26,       rep(25, 2),  24:26,       rep(24, 3), 
                   23:26,        rep(23, 4),  22:26,       rep(22, 5),  21:26,       rep(21, 6), 
                   20:26,        rep(20, 7),  19:26,       rep(19, 8),  18:26,       rep(18, 9),
                   17:26,        rep(17, 10), 16:26,       rep(16, 11), 15:26,       rep(15, 12),
                   14:26,        rep(14, 13)),
                 c(rep(26, 1),   rep(26, 1),  rep(26, 2),  rep(26, 2),  rep(26, 3),  rep(26, 3),
                   rep(26, 4),   rep(26, 4),  rep(26, 5),  rep(26, 5),  rep(26, 6),  rep(26, 6), 
                   rep(26, 7),   rep(26, 7),  rep(26, 8),  rep(26, 8),  rep(26, 9),  rep(26, 9),
                   rep(26, 10),  rep(26, 10), rep(26, 11), rep(26, 11), rep(26, 12), rep(26, 12),
                   rep(26, 13),  rep(26, 13))
                 )

wt_test <- cbind(c(rep(1, 13)), 
                 c(1:13),
                 c(26:14),
                 c(rep(26, 13))
)

f_wtc_tem0_index_out <- function(wt_test_in){
  
  wt_low <- wt_test[i, 1] : wt_test[i, 2] #selected low weather types for trend analysis
  wt_hig <- wt_test[i, 3] : wt_test[i, 4] #selected high weather types for trend analysis
  
  low_wts  <- as.numeric(names(gwt_sums_tem0_sort)[wt_low])
  hig_wts  <- as.numeric(names(gwt_sums_tem0_sort)[wt_hig])
  
  wt_hig_slo <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = hig_wts)*100*10# [%/dec]
  
  wt_low_slo <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = low_wts)*100*10 # [%/dec]
  
  wte_index_sing <- wt_hig_slo - wt_low_slo
  
  cor_out_sing <- cor(tem0_4_gwt_slo, wte_index_sing, method = "spearman")
  
  return(wte_index_sing)
  
}
f_wtc_tem0_corre_out <- function(wt_test_in){
  
  wt_low <- wt_test[i, 1] : wt_test[i, 2] #selected low weather types for trend analysis
  wt_hig <- wt_test[i, 3] : wt_test[i, 4] #selected high weather types for trend analysis
  
  low_wts  <- as.numeric(names(gwt_sums_tem0_sort)[wt_low])
  hig_wts  <- as.numeric(names(gwt_sums_tem0_sort)[wt_hig])
  
  wt_hig_slo <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = hig_wts)*100*10# [%/dec]
  
  wt_low_slo <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                              end_year = end_year, window_width = window_width,
                              cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                              weather_type = low_wts)*100*10 # [%/dec]
  
  wte_index_sing <- wt_hig_slo - wt_low_slo
  
  cor_out_sing <- cor(tem0_4_gwt_slo, wte_index_sing, method = "spearman")
  
  return(cor_out_sing)
  
}
f_wtc_slo <- function(wtc_in){
  
  wtc_slo_out <- moving_analys(dates = data_gwt26$date, values = data_gwt26$value, start_year = start_year,
                               end_year = end_year, window_width = window_width,
                               cover_thres = cover_thres, method_analys = "weather_type_window_likeli_sens_slope",
                               weather_type = wtc_in) * 100 * 10 #[%/dec]
  return(wtc_slo_out)
  
}

wtc_slo <- foreach(i = 1:26, .combine = 'cbind') %dopar% {
  
  f_wtc_slo(i)
  
}

for(i in 1:26){
  plot(loess_NA_restore(tem0_4_gwt_slo), type = "l")
  par(new = TRUE)
  plot(loess_NA_restore(wtc_slo[, i]), type = "l", main = i)
}

weigth_fact <- abs(gwt_sums_tem0) / sum_na(abs(gwt_sums_tem0))
weigth_fact <- c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1)
# weigth_fact[23] <- 0.0001

test <- wtc_slo %*% weigth_fact

plot(loess_NA_restore(tem0_4_gwt_slo), type = "l")
par(new = TRUE)
plot(loess_NA_restore(test), type = "l", col = "red")


wte_index <- foreach(i = 1:nrow(wt_test), .combine = 'cbind') %dopar% {
  
  f_wtc_tem0_index_out(wt_test[, i])
  
}
wte_corre <- foreach(i = 1:nrow(wt_test), .combine = 'cbind') %dopar% {
  
  f_wtc_tem0_corre_out(wt_test[, i])
  
}

wt_test[which(wte_corre == max(wte_corre)), ]
wt_test[182, ]

names(gwt_sums_tem0_sort)[25:26]
names(gwt_sums_tem0_sort)[1:3]

plot(tem0_4_gwt_slo, type = "l")
par(new = TRUE)
plot(wte_index[, 182], type = "l", col = "red3")

plot(wte_corre[1, ])
abline(v = which(wte_corre == max(wte_corre)))

test <- sort(wte_corre[1, ], decreasing = T)
test[1:20]

wt_test[c(8, 48, 24, 35, 168, 3, 143, 120, 63, 15,
          80, 169, 99, 172, 144, 121, 25, 49, 5), ]

#Intro plot catego####

#Calculate seasonal values

f_seas_vals <- function(values, dates = tem0_data$date, start_year = 1981, end_year = 2017){

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

  # #Moving average filter
  # input_data$ma <- rollapply(data = input_data$values, width = window_width,
  #                            FUN = mea_na_thres, align = "center", fill = NA)

  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  my_months <- format(days,"%m")

  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", my_months)
  data_day[ ,1] <- start_year:end_year

  for(i in 0:(length(start_year:end_year)-1)) {

    data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]

  }

  #Calculate seasonal averages
  djf_cols <- which(colnames(data_day) %in% c("12", "01", "02"))
  mam_cols <- which(colnames(data_day) %in% c("03", "04", "05"))
  jja_cols <- which(colnames(data_day) %in% c("06", "07", "08"))
  son_cols <- which(colnames(data_day) %in% c("09", "10", "11"))

  f_med_djf <- function(data_in, cols = djf_cols){
    med_na(data_in[cols])
  }
  f_med_mam <- function(data_in, cols = mam_cols){
    med_na(data_in[cols])
  }
  f_med_jja <- function(data_in, cols = jja_cols){
    med_na(data_in[cols])
  }
  f_med_son <- function(data_in, cols = son_cols){
    med_na(data_in[cols])
  }

  med_djf <- apply(data_day, 1, f_med_djf)
  med_mam <- apply(data_day, 1, f_med_mam)
  med_jja <- apply(data_day, 1, f_med_jja)
  med_son <- apply(data_day, 1, f_med_son)

  data_out <- cbind(med_djf, med_mam, med_jja, med_son)

  return(data_out)

}

stat_cols_tem0 <- which(colnames(tem0_data) %in% colnames(tem0_sl))

tem0_use <- tem0_data[, stat_cols_tem0]

for (i in 1:ncol(tem0_use)) {

  seas_out <- f_seas_vals(tem0_use[, i])

  if(i == 1){
    seas_vals <- seas_out
  }else{
    seas_vals <- cbind(seas_vals, seas_out)
    }

}

for(i in 1:ncol(tem0_use)){
  stat_cat <- as.character(stat_meta$category[which(stat_meta$stn == colnames(tem0_use)[i])])

  if(i ==1){
    stat_categos <- stat_cat
  }else{
    stat_categos <- c(stat_categos, stat_cat)
  }
}

djf_low_vals <- seas_vals[, (1 + ((which(stat_categos == "low")-1) * 4))]
djf_mid_vals <- seas_vals[, (1 + ((which(stat_categos == "middle")-1) * 4))]
djf_hig_vals <- seas_vals[, (1 + ((which(stat_categos == "high")-1) * 4))]

mam_low_vals <- seas_vals[, (2 + ((which(stat_categos == "low")-1) * 4))]
mam_mid_vals <- seas_vals[, (2 + ((which(stat_categos == "middle")-1) * 4))]
mam_hig_vals <- seas_vals[, (2 + ((which(stat_categos == "high")-1) * 4))]

jja_low_vals <- seas_vals[, (3 + ((which(stat_categos == "low")-1) * 4))]
jja_mid_vals <- seas_vals[, (3 + ((which(stat_categos == "middle")-1) * 4))]
jja_hig_vals <- seas_vals[, (3 + ((which(stat_categos == "high")-1) * 4))]

son_low_vals <- seas_vals[, (4 + ((which(stat_categos == "low")-1) * 4))]
son_mid_vals <- seas_vals[, (4 + ((which(stat_categos == "middle")-1) * 4))]
son_hig_vals <- seas_vals[, (4 + ((which(stat_categos == "high")-1) * 4))]


djf_low <- apply(djf_low_vals, 1, med_na)
djf_mid <- apply(djf_mid_vals, 1, med_na)
djf_hig <- apply(djf_hig_vals, 1, med_na)

mam_low <- apply(mam_low_vals, 1, med_na)
mam_mid <- apply(mam_mid_vals, 1, med_na)
mam_hig <- apply(mam_hig_vals, 1, med_na)

jja_low <- apply(jja_low_vals, 1, med_na)
jja_mid <- apply(jja_mid_vals, 1, med_na)
jja_hig <- apply(jja_hig_vals, 1, med_na)

son_low <- apply(son_low_vals, 1, med_na)
son_mid <- apply(son_mid_vals, 1, med_na)
son_hig <- apply(son_hig_vals, 1, med_na)

#Sesonal trends
djf_low_sl <- as.numeric(zyp.trend.vector(djf_low, method = "zhang", conf.intervals = F)[c(11,2)])
djf_mid_sl <- as.numeric(zyp.trend.vector(djf_mid, method = "zhang", conf.intervals = F)[c(11,2)])
djf_hig_sl <- as.numeric(zyp.trend.vector(djf_hig, method = "zhang", conf.intervals = F)[c(11,2)])

mam_low_sl <- as.numeric(zyp.trend.vector(mam_low, method = "zhang", conf.intervals = F)[c(11,2)])
mam_mid_sl <- as.numeric(zyp.trend.vector(mam_mid, method = "zhang", conf.intervals = F)[c(11,2)])
mam_hig_sl <- as.numeric(zyp.trend.vector(mam_hig, method = "zhang", conf.intervals = F)[c(11,2)])

jja_low_sl <- as.numeric(zyp.trend.vector(jja_low, method = "zhang", conf.intervals = F)[c(11,2)])
jja_mid_sl <- as.numeric(zyp.trend.vector(jja_mid, method = "zhang", conf.intervals = F)[c(11,2)])
jja_hig_sl <- as.numeric(zyp.trend.vector(jja_hig, method = "zhang", conf.intervals = F)[c(11,2)])

son_low_sl <- as.numeric(zyp.trend.vector(son_low, method = "zhang", conf.intervals = F)[c(11,2)])
son_mid_sl <- as.numeric(zyp.trend.vector(son_mid, method = "zhang", conf.intervals = F)[c(11,2)])
son_hig_sl <- as.numeric(zyp.trend.vector(son_hig, method = "zhang", conf.intervals = F)[c(11,2)])




#Intro plot region####

#Calculate seasonal values

f_seas_vals <- function(values, dates = tem0_data$date, start_year = 1981, end_year = 2017){

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

  # #Moving average filter
  # input_data$ma <- rollapply(data = input_data$values, width = window_width,
  #                            FUN = mea_na_thres, align = "center", fill = NA)

  #Vector with the 365 days of the year
  days <- seq(as.Date('2014-01-01'), to=as.Date('2014-12-31'), by='days')
  my_months <- format(days,"%m")

  #Order data by day
  data_day <-  matrix(NA, nrow = length(start_year:end_year), ncol = 366)
  colnames(data_day) <- c("year", my_months)
  data_day[ ,1] <- start_year:end_year

  for(i in 0:(length(start_year:end_year)-1)) {

    data_day[i+1, 2:366] <- input_data$values[(i*365+1):((i+1)*365)]

  }

  #Calculate seasonal averages
  djf_cols <- which(colnames(data_day) %in% c("12", "01", "02"))
  mam_cols <- which(colnames(data_day) %in% c("03", "04", "05"))
  jja_cols <- which(colnames(data_day) %in% c("06", "07", "08"))
  son_cols <- which(colnames(data_day) %in% c("09", "10", "11"))

  f_med_djf <- function(data_in, cols = djf_cols){
    med_na(data_in[cols])
  }
  f_med_mam <- function(data_in, cols = mam_cols){
    med_na(data_in[cols])
  }
  f_med_jja <- function(data_in, cols = jja_cols){
    med_na(data_in[cols])
  }
  f_med_son <- function(data_in, cols = son_cols){
    med_na(data_in[cols])
  }

  med_djf <- apply(data_day, 1, f_med_djf)
  med_mam <- apply(data_day, 1, f_med_mam)
  med_jja <- apply(data_day, 1, f_med_jja)
  med_son <- apply(data_day, 1, f_med_son)

  data_out <- cbind(med_djf, med_mam, med_jja, med_son)

  return(data_out)

}

stat_cols_tem0 <- which(colnames(tem0_data) %in% colnames(tem0_sl))

tem0_use <- tem0_data[, stat_cols_tem0]

for (i in 1:ncol(tem0_use)) {

  seas_out <- f_seas_vals(tem0_use[, i])

  if(i == 1){
    seas_vals <- seas_out
  }else{
    seas_vals <- cbind(seas_vals, seas_out)
  }

}

for(i in 1:ncol(tem0_use)){
  stat_reg <- as.character(stat_meta$clim_reg[which(stat_meta$stn == colnames(tem0_use)[i])])

  if(i ==1){
    stat_regions <- stat_reg
  }else{
    stat_regions <- c(stat_regions, stat_reg)
  }
}

djf_jur_vals <- seas_vals[, (1 + ((which(stat_regions == "Jura")-1) * 4))]
djf_pla_vals <- seas_vals[, (1 + ((which(stat_regions == "Plateau")-1) * 4))]
djf_alp_vals <- seas_vals[, (1 + ((which(stat_regions == "Alps")-1) * 4))]
djf_sal_vals <- seas_vals[, (1 + ((which(stat_regions == "S_Alps")-1) * 4))]

mam_jur_vals <- seas_vals[, (2 + ((which(stat_regions == "Jura")-1) * 4))]
mam_pla_vals <- seas_vals[, (2 + ((which(stat_regions == "Plateau")-1) * 4))]
mam_alp_vals <- seas_vals[, (2 + ((which(stat_regions == "Alps")-1) * 4))]
mam_sal_vals <- seas_vals[, (2 + ((which(stat_regions == "S_Alps")-1) * 4))]

jja_jur_vals <- seas_vals[, (3 + ((which(stat_regions == "Jura")-1) * 4))]
jja_pla_vals <- seas_vals[, (3 + ((which(stat_regions == "Plateau")-1) * 4))]
jja_alp_vals <- seas_vals[, (3 + ((which(stat_regions == "Alps")-1) * 4))]
jja_sal_vals <- seas_vals[, (3 + ((which(stat_regions == "S_Alps")-1) * 4))]

son_jur_vals <- seas_vals[, (4 + ((which(stat_regions == "Jura")-1) * 4))]
son_pla_vals <- seas_vals[, (4 + ((which(stat_regions == "Plateau")-1) * 4))]
son_alp_vals <- seas_vals[, (4 + ((which(stat_regions == "Alps")-1) * 4))]
son_sal_vals <- seas_vals[, (4 + ((which(stat_regions == "S_Alps")-1) * 4))]


djf_jur <- apply(djf_jur_vals, 1, med_na)
djf_pla <- apply(djf_pla_vals, 1, med_na)
djf_alp <- apply(djf_alp_vals, 1, med_na)
djf_sal <- apply(djf_sal_vals, 1, med_na)

mam_jur <- apply(mam_jur_vals, 1, med_na)
mam_pla <- apply(mam_pla_vals, 1, med_na)
mam_alp <- apply(mam_alp_vals, 1, med_na)
mam_sal <- apply(mam_sal_vals, 1, med_na)

jja_jur <- apply(jja_jur_vals, 1, med_na)
jja_pla <- apply(jja_pla_vals, 1, med_na)
jja_alp <- apply(jja_alp_vals, 1, med_na)
jja_sal <- apply(jja_sal_vals, 1, med_na)

son_jur <- apply(son_jur_vals, 1, med_na)
son_pla <- apply(son_pla_vals, 1, med_na)
son_alp <- apply(son_alp_vals, 1, med_na)
son_sal <- apply(son_sal_vals, 1, med_na)

#Sesonal trends
djf_jur_sl <- as.numeric(zyp.trend.vector(djf_jur, method = "zhang", conf.intervals = F)[c(11,2)])
djf_pla_sl <- as.numeric(zyp.trend.vector(djf_pla, method = "zhang", conf.intervals = F)[c(11,2)])
djf_alp_sl <- as.numeric(zyp.trend.vector(djf_alp, method = "zhang", conf.intervals = F)[c(11,2)])
djf_sal_sl <- as.numeric(zyp.trend.vector(djf_sal, method = "zhang", conf.intervals = F)[c(11,2)])

mam_jur_sl <- as.numeric(zyp.trend.vector(mam_jur, method = "zhang", conf.intervals = F)[c(11,2)])
mam_pla_sl <- as.numeric(zyp.trend.vector(mam_pla, method = "zhang", conf.intervals = F)[c(11,2)])
mam_alp_sl <- as.numeric(zyp.trend.vector(mam_alp, method = "zhang", conf.intervals = F)[c(11,2)])
mam_sal_sl <- as.numeric(zyp.trend.vector(mam_sal, method = "zhang", conf.intervals = F)[c(11,2)])

jja_jur_sl <- as.numeric(zyp.trend.vector(jja_jur, method = "zhang", conf.intervals = F)[c(11,2)])
jja_pla_sl <- as.numeric(zyp.trend.vector(jja_pla, method = "zhang", conf.intervals = F)[c(11,2)])
jja_alp_sl <- as.numeric(zyp.trend.vector(jja_alp, method = "zhang", conf.intervals = F)[c(11,2)])
jja_sal_sl <- as.numeric(zyp.trend.vector(jja_sal, method = "zhang", conf.intervals = F)[c(11,2)])

son_jur_sl <- as.numeric(zyp.trend.vector(son_jur, method = "zhang", conf.intervals = F)[c(11,2)])
son_pla_sl <- as.numeric(zyp.trend.vector(son_pla, method = "zhang", conf.intervals = F)[c(11,2)])
son_alp_sl <- as.numeric(zyp.trend.vector(son_alp, method = "zhang", conf.intervals = F)[c(11,2)])
son_sal_sl <- as.numeric(zyp.trend.vector(son_sal, method = "zhang", conf.intervals = F)[c(11,2)])




#Frequencies weather types for table----

f_frequ_wt <- function(dates, values, start_year, end_year, weather_type){

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

  wt_frequ <- length(which(input_data$values == weather_type)) / length(input_data$values) * 100 # Frequency in [%]

  return(wt_frequ)

}

gwt26_frequ <- rep(NA, 26)
for (i in 1:26) {

   wt_fr <- f_frequ_wt(dates  = data_gwt26$date,
                       values = data_gwt26$value,
                       start_year = start_year,
                       end_year = end_year,
                       weather_type = i)

   gwt26_frequ[i] <- wt_fr
}

wt_temp <- c(1,2,3,4,25,9,10,11,12,26)
wt_humi <- c(1,3,4,9,10,11)
sum(gwt26_frequ[wt_temp])
sum(gwt26_frequ[wt_humi])

#stop cluster####
stopCluster(my_clust)
