#' Order data set by altitude. Add missing stations.
#'
#' Orders columns according to elevatoin of stations and adds missing stations for
#' which no data available.
#'
#' @param data_in Data frame after moving average trends analysis.
#' @param meta_stat Stations meta information.
#' @return Data frame with columns ordered according to station elevation.
#' @export
order_add_stat <- function(data_in, meta_stat){

  #Order meta data according to elevation
  meta_stat <- meta_stat[order(meta_stat$alt, decreasing = FALSE), ]

  for(i in 1:length(meta_stat$stn)){
    stat_ID  <- as.character(meta_stat$stn[i])
    stat_Col <- which(colnames(data_in) == stat_ID) #which column in data

    if(length(stat_Col) > 0){
      data_sel <- data_in[, stat_Col]
    }else{
      data_sel <- rep(NA, nrow(data_in))
    }

    if(i == 1){
      data_out <- data_sel
      stat_IDs <- stat_ID
    }else{
      data_out <- cbind(data_out, data_sel)
      stat_IDs <- c(stat_IDs, stat_ID)
    }
  }
  colnames(data_out) <- stat_IDs

  return(data_out)
}
