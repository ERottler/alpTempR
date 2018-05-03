#' Image plots yearly cycles of trend magnitudes.
#'
#' Image plot of results of moving average analysis. Stations ordered accoring to
#' elevation along y-axis.
#'
#' @param data_in Data frame with results of moving average analysis.
#' @param data_meta Meta data file of stations.
#' @param colors Vector of colors for z-value.
#' @param breaks Color breaks in z-range.
#' @param main Plot title.
#' @param unit Unit of input data.
#' @param margins_1 Margins of left plot panel.
#' @param margins_2 Margins of right plot panel.
#' @param isotherm_data Data frame with isotherm data.
#' @param add_isotherms Plot isotherms yes or no (T or F)
#' @param pos_iso_text Position is text of isotherms in plot.
#' @param pos_ylab_lal Posistion  y-labels (HS, MS,LS)
#' @examples
#'
#' @export
image_cycle_elev <- function(data_in, stat_meta, colors, breaks, main="", unit="",
                             margins_1 = c(1.6,2.5,1.6,0), margins_2 = c(1.6,0.5,1.6,1.7),
                             isotherm_data = NULL, add_isotherms = FALSE,
                             pos_iso_text = NULL, pos_ylab_lal = c(0.27, 0.71, 0.985)){

  #Order data by altitude; add stations that are not in data file
  data_in <- order_add_stat(data_in, meta_stat = stat_meta)

  #Positions ticks and labels for x-axis
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

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

  axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
       col = "black", col.axis = "black", tck = -0.03)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col="black", col.axis="black", mgp=c(3, 0.3, 0))#plot labels
  mtext("Stations (increasing elevation)", 2, 1.2, cex = 0.7)
  mtext(main, 3, 0.1, adj = 0.5, cex = 1)
  box()

  par(mar= margins_2)

  image_scale(as.matrix(data_in), col = colors, breaks = breaks, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
  axis(4, mgp=c(3, 0.3, 0))
  box()

}


