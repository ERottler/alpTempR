###

#Data analyses

###

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
window_width <- 60
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


t.dat <- c(0, 0.3, 1, 3, 3.4, 6)
t.dat <- c(rep(0,34), 0.01, 0.01, 0.01)
zyp.trend.vector(t.dat, method="zhang")[2]


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

statsData[which(!statsData %in% stationMeta$stn)]#station in data that are not in meta data
stationMeta$stn[which(!stationMeta$stn %in% statsData)]#stations in meta data that are not in data

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


Sys.time()



