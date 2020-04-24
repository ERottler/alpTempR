#' Determination of temperature isotherms.
#'
#' Calculation of temperature isotherms for image plot (see image_cycle_elev())
#' based on temperature data (usually after applying moving average filter).
#'
#' @param data_in Input temperature data after moving average statistics to get
#' mean yearly cycles of temperature of stations.
#' @param isotherm What isotherm to calculate (e.g. 0 for 0°C isotherm)
#' @param meta_stat Station meta information.
#' @param smoo_val Smooth values using LOESS (f = smoo_val)
#' @return Days of the year which closest to selected isotherm.
#' @export
iso_days <- function(data_in, isotherm = 0, meta_stat, smoo_val = 0.75){

  #Determination days closest to the selected temperature during ascending and
  #descending part of yearly temperature cycle
  iso_days <- matrix(nrow = 2, ncol = ncol(data_in))

  for(i in 1:ncol(data_in)){

    #Determination day with maximum temperature during year seperating yearly
    #cycle in ascending and descending part
    sep_day <- which(data_in[, i] == max_na(data_in[, i]))

    #Determination day on ascending part
    iso_day_asc <- which(abs(data_in[1:sep_day, i] - isotherm) ==
                         min_na( abs(data_in[1:sep_day, i] - isotherm)))

    #If difference found temperature and isotherm too big, put to NA
    #Temperatures never reach selected isotherm temperature
    if(min_na(abs(data_in[1:sep_day, i] - isotherm)) > 0.2){iso_day_asc <- NA}

    #Determination day on ascending part
    iso_day_des <- which(abs(data_in[(sep_day + 1):365, i] - isotherm) ==
                        min_na(abs(data_in[sep_day + 1:365, i] - isotherm) ) ) + sep_day

    #If difference found temperature and isotherm too big, put to NA
    #Temperatures never reach selected isotherm temperature
    if(min_na(abs(data_in[sep_day+1:365, i] - isotherm)) > 0.2){iso_day_des <- NA}

    iso_days[1,i] <- iso_day_asc
    iso_days[2,i] <- iso_day_des
  }

  colnames(iso_days) <- colnames(data_in)
  iso_days_ord <- order_add_stat(data_in = iso_days, meta_stat =  meta_stat)

  iso_days_ord[1, ] <- loess_NA_restore(iso_days_ord[1, ], smoo_val = smoo_val, NA_restore = F)
  iso_days_ord[2, ] <- loess_NA_restore(iso_days_ord[2, ], smoo_val = smoo_val, NA_restore = F)

  iso_days <- c(iso_days_ord[1, ], iso_days_ord[2, ])
  stat_num <- c(1:ncol(iso_days_ord), 1:ncol(iso_days_ord))

  iso_out <- data.frame(stat_num = stat_num, iso_out = iso_days)

  return(iso_out)
}
