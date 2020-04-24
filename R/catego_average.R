
#' Mean values of elevation categories.
#'
#' Function to get mean values of elevatio categories.
#'
#' @param data_in Input data frame; Station data sorted by column
#' @param stat_meta Table containing meta information on investigated stations
#' @return Mean value of elevation categores (low, middle, high)
#' @examples
#' catego_average(temperature[, -1], station_meta)
#' @export
catego_average <- function(data_in, data_meta){

  #Get station IDs
  stat_meta_sel <- data_meta[which(data_meta$stn %in% colnames(data_in)), ]
  stat_IDs <- as.character(stat_meta_sel$stn)

  #Get categories of stations
  for(i in 1:ncol(data_in)){

    if(i ==1){
      categos <- as.character(stat_meta_sel$category[which(colnames(data_in)[i] == stat_IDs)])}else{
        categos <- c(categos, as.character(stat_meta_sel$category[which(colnames(data_in)[i] == stat_IDs)]))
      }
  }


  #Group station according to elevation category / quality level
  st_all_nu <- 1:nrow(stat_meta_sel)
  st_hig_nu <- which(categos == "high")
  st_mid_nu <- which(categos == "middle")
  st_low_nu <- which(categos == "low")
  st_all   <- stat_meta_sel$stn
  st_hig   <- colnames(data_in)[st_hig_nu]
  st_mid   <- colnames(data_in)[st_mid_nu]
  st_kow   <- colnames(data_in)[st_low_nu]

  #Calculate average yearly cycles of categories
  cyc_HS <- rep(NA,365)
  cyc_HS <- apply(data_in[, st_hig_nu], 1, med_na)
  cyc_MS <- rep(NA,365)
  cyc_MS <- apply(data_in[, st_mid_nu], 1, med_na)
  cyc_LS <- rep(NA,365)
  cyc_LS <- apply(data_in[, st_low_nu], 1, med_na)

  #Calculate annual averages of cateogies
  cyc_HS_me <- med_na(cyc_HS)
  cyc_MS_me <- med_na(cyc_MS)
  cyc_LS_me <- med_na(cyc_LS)

  out_vals <- c(cyc_LS_me, cyc_MS_me, cyc_HS_me)

  return(out_vals)

}

