###

#Preparation raw data for further analyses

###

base_dir <- "u:/RhineFlow/Elevation/Data/"

idaweb_data_prep(param_code = "tre200d0", order_dir = paste0(base_dir,"rawData/IDAweb/tmean"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "tre200dx", order_dir = paste0(base_dir,"rawData/IDAweb/tmaxi"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "tre200dn", order_dir = paste0(base_dir,"rawData/IDAweb/tmini"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "hto000d0", order_dir = paste0(base_dir,"rawData/IDAweb/snoHe"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "pp0qffd0", order_dir = paste0(base_dir,"rawData/IDAweb/airPre"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "nto000d0", order_dir = paste0(base_dir,"rawData/IDAweb/clouds"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "gre000d0", order_dir = paste0(base_dir,"rawData/IDAweb/gloRad"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "sre000d0", order_dir = paste0(base_dir,"rawData/IDAweb/sunsh"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")

idaweb_data_prep(param_code = "pva200d0", order_dir = paste0(base_dir,"rawData/IDAweb/vapPre"),
                 out_dir = base_dir, start_day = "1981-01-01", end_day = "2017-12-31")


#Calculation of absolute air humidity
load(paste0(base_dir,"/pva200d0.RData")) ; watpres <- out_data
load(paste0(base_dir,"/tre200d0.RData")) ; tamean  <- out_data

#stations with water vapor pressure and temperature data
watpres <- watpres[, which(colnames(watpres) %in% colnames(tamean))]

statIDs <- colnames(watpres)[-1] #station IDs as colnames without 'date'

out_data <- watpres
out_data[,] <- NA
out_data$date <- watpres$date

for(i in 1:length(statIDs)){
  vapor_pres <- watpres[, which(grepl(paste0(statIDs[i]), colnames(watpres)))]
  temp       <- tamean[,  which(grepl(paste0(statIDs[i]), colnames(tamean)))]
  k <- which(grepl(paste0(statIDs[i]), colnames(out_data)))

  out_data[,k] <- pres2ahum(vapor_pres = vapor_pres, temp = temp, Rw=461.5)
}

save(file = paste0(base_dir, "abshumid.RData"), list="out_data")


