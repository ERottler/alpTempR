#' Image plots yearly cycles of trend magnitudes.
#'
#' ...
#'
#' @param data_in ...
#' @param data_mk ...
#' @param data_in_me ...
#' @param data_meta ...
#' @param window_width ...
#' @param cover_thresh ...
#' @param method_analys ...
#' @return ...
#' @examples
#'
#' @export
image_cycle_elev <- function(data_in, stat_meta, colors, breaks, main="", unit="",
                             margins_1 = c(1.6,2.5,1.6,0), margins_2 = c(1.6,0.5,1.6,1.7),
                             isotherm_data = NULL, add_isotherms = FALSE,
                             pos_iso_text = NULL, pos_ylab_lal = c(0.27, 0.71, 0.97)){

  #Order data by altitude; add stations that are not in data file
  data_in <- order_add_stat(data_in, meta_stat = stat_meta)

  #Position of x-axis ticks and labels
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,365)-14
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)

  y <- 1:ncol(data_in)
  x <- 1:365

  par(mar= margins_1)

  image(x, y, as.matrix(data_in), col = colors, breaks = breaks, ylab = "",
        xlab = "", axes = F)

  if(add_isotherms){
    for(i in 2:ncol(isotherm_data)){
      lines(isotherm_data[, i], isotherm_data[, 1], type = "l", lwd = 2)
      text(pos_iso_text[1, i-1], pos_iso_text[2, i-1], colnames(isotherm_data)[i],  font = 2)
    }
  }

  low_stat_num <- length(which(stat_meta$category == "low"))
  mid_stat_num <- length(which(stat_meta$category == "middle"))
  hig_stat_num <- length(which(stat_meta$category == "high"))

  stat_ticks <- c(low_stat_num + 0.5, low_stat_num + mid_stat_num + 0.5,
                  low_stat_num + mid_stat_num + hig_stat_num + 0.5)

  axis(2, at = stat_ticks, labels = c("","",""), mgp = c(3, 0.3, 0))
  mtext("LS", 2, 0.1, adj = pos_ylab_lal[1], cex = 0.7)
  mtext("MS", 2, 0.1, adj = pos_ylab_lal[2], cex = 0.7)
  mtext("HS", 2, 0.1, adj = pos_ylab_lal[3], cex = 0.7)

  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  mtext("Stations (increasing altitude)", 2, 1.2, cex = 0.7)
  mtext(main, 3, 0.1, adj = 0.5, cex = 1)
  box()

  par(mar= margins_2)

  image_scale(as.matrix(data_in), col = colors, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.3, 0))
  box()

}


