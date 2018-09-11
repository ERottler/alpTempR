#' Plot yearly cycle and elevation dependency.
#'
#' Plot yearly cycles of moving average analysis and annual averages to visualize
#' elevation dependency for multiple stations of different elevation categories.
#'
#' @param data_in Input data frame holding results of moving average analysis.
#' @param data_mk Input analysis with results of Mann-Kendall trend test.
#' @param data_in_me Data frame with annual average values of stations.
#' @param data_meta Station meta data.
#' @param main_text Plot title
#' @param margins_1 Figure margins of left plot panel
#' @param margins_2 Figure margins of right plot panel.
#' @param no_col Display elevation categories with different colors (T or F)
#' @param snow_mk Snow days with significant trends with yellow dots (T or F)
#' @param aggr_cat_mean If TRUE, uasnge mean average instead of median for calculation af category averages
#' @param with_hom_dat Display stations with homogenized time series (T or F)
#' @param smooth_val Smoothing value for LOESS.
#' @param mk_sig_level Significance level alpha.
#' @param add_st_num Add number of stations of each category top right (T or F)
#' @return Plot with yearly cycles (lines; left panel) of moving average analysis results and
#' annual averages (right panel)
#' @export
plot_cycl_elev <- function(data_in, data_mk, data_in_me, data_meta, main_text = "",
                           margins_1 = c(1.4,1.8,1.8,0.2), margins_2 = c(1.4,0.2,1.8,3.5),
                           no_col = F, show_mk = F, aggr_cat_mean = F,with_hom_dat=F,
                           smooth_val = 0.2, mk_sig_level = 0.05, add_st_num = T){

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

  #Get data quality level
  for(i in 1:ncol(data_in)){

    if(i ==1){
      qLevel <- as.character(stat_meta_sel$data_qual[which(colnames(data_in)[i] == stat_IDs)])}else{
      qLevel <- c(qLevel, as.character(stat_meta_sel$data_qual[which(colnames(data_in)[i] == stat_IDs)]))
      }
  }

  #Group station according to elevation category / quality level
  st_all_nu <- 1:nrow(stat_meta_sel)
  st_hig_nu <- which(categos == "high")
  st_mid_nu <- which(categos == "middle")
  st_low_nu <- which(categos == "low")
  st_hom_nu <- which(qLevel  == "homogenized")
  st_qch_nu <- which(qLevel  == "quality-checked")
  st_all   <- stat_meta_sel$stn
  st_hig   <- colnames(data_in)[st_hig_nu]
  st_mid   <- colnames(data_in)[st_mid_nu]
  st_kow   <- colnames(data_in)[st_low_nu]
  st_hom   <- colnames(data_in)[st_hom_nu]
  st_qch   <- colnames(data_in)[st_qch_nu]

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

  #Using mean instead of median for calculation af category averages
  if(aggr_cat_mean){
    cyc_HS <- apply(data_in[, st_hig_nu], 1, mea_na)
    cyc_MS <- apply(data_in[, st_mid_nu], 1, mea_na)
    cyc_LS <- apply(data_in[, st_low_nu], 1, mea_na)

    cyc_HS_me <- mea_na(cyc_HS)
    cyc_MS_me <- mea_na(cyc_MS)
    cyc_LS_me <- mea_na(cyc_LS)
  }

  #Smooth yearly cycles using loess
  #Re-define loess function with smoothing values selected
  my_loess_NA_restore <- function(data_in, smoo_val, NA_restore = TRUE){
    if(length(which(is.na(data_in))) == length(data_in)){
      data_out <- data_in
    }else{
      data_out <- loess_NA_restore(data_in = data_in, smoo_val = smooth_val, NA_restore = TRUE)
    }
    return(data_out)
  }

  data_in <- apply(data_in, 2, my_loess_NA_restore)

  #Positions ticks and labels for x-axis
  x_axis_lab <- c(15,46,74,105,135,166,196,227,258,288,319,349)
  x_axis_tic <- c(15,46,74,105,135,166,196,227,258,288,319,349,380)-15

  #Plot: Yearly cycle
  par(mar = margins_1)

  col_1 <- rgb(145, 145, 145, max=255, alpha = 200) #grey
  col_2 <- rgb(255, 238, 0,   max=255, alpha = 220) #yellow

  for(i in st_all_nu){
    if(i == st_all_nu[1]){
      plot(data_in[,i], type="l", col=col_1,
           main="", ylab="", xlab="", axes=F,
           ylim=c(min_na(data_in[, ]), max_na(data_in[, ])))
    }
    #print(i)
    if(show_mk){
      #yellow if significant trend, else no color
      col_mk <- ifelse(data_mk[,which(paste0(stat_IDs[i]) == colnames(data_in))] > mk_sig_level, "#FFFFFF00", col_2)
      }else{col_mk=rep("#FFFFFF00", 365)#if no wish to display significance, color set to transparent
      }

    lines(data_in[, i], col=col_1, lwd=0.8)#plot yearly cycles of stations
    points(data_in[, i], col=col_mk, pch=18, cex=0.17)#add info on significance
  }

  col_HS <- "blue3"
  col_MS <- "black"
  col_LS <- "red3"

  if(no_col){col_HS = col_MS = col_LS = "grey30"}

  #Add yearly cycles of categories
  if(!all(is.na(cyc_HS))){lines(my_loess_NA_restore(cyc_HS), lwd = 1.6, col = col_HS)}
  if(!all(is.na(cyc_MS))){lines(my_loess_NA_restore(cyc_MS), lwd = 1.6, col = col_MS)}
  if(!all(is.na(cyc_LS))){lines(my_loess_NA_restore(cyc_LS), lwd = 1.6, col = col_LS)}
  abline(h = 0, lty = "dashed", lwd = 0.9)
  abline(v = x_axis_tic, lty = "dashed", lwd = 0.9)
  axis(2, mgp = c(3, 0.5, 0), tck = -0.04)
  box(lwd = 1)
  mtext(main_text, side = 3, line = 1, padj = 1, at = 385, cex = 1)

  axis(1, at = x_axis_tic, c("","","","","","","","","","","","",""), tick = TRUE,
       col="black", col.axis="black", tck=-0.04)#plot ticks
  axis(1, at = x_axis_lab, c("J","F","M","A","M","J","J","A","S","O","N","D"), tick = FALSE,
       col = "black", col.axis = "black", mgp = c(3, 0.3, 0))

  #Mean vs. Elevation
  par(mar = margins_2)

  plot(1, 1, type="n", ylim=c(min_na(data_meta$alt)-100, max_na(data_meta$alt)+100),
       xlim=c(min(data_in_me, na.rm=T), max(data_in_me, na.rm=T)), axes=F, xlab="", ylab="",
       main="")
  axis(1, mgp = c(3, 0.3, 0), tck = -0.04)
  axis(4, mgp = c(3, 0.4, 0), tck = -0.04)
  mtext("Elevation [m]",4, 1.8, cex=0.9, tck=-0.03)

  pardat <- par()
  #col2rgb("blue3")
  my_blu <- rgb(0, 0, 205, max=255, alpha = 80)
  #col2rgb("red3")
  my_red <- rgb(205, 0, 0, max=255, alpha = 80)
  #col2rgb("black")
  my_bla <- rgb(0, 0, 0,   max=255, alpha = 60)
  #col2rgb("grey")
  my_gre <- rgb(190, 190, 190,   max=255, alpha = 60)

  if(no_col){my_blu = my_red = my_bla = rgb(0, 0, 0,   max=255, alpha = 0)}

  #Get elevation borders of categories
  hig_mid <- mea_na(c(min_na(data_meta$alt[which(data_meta$category == "high")]),
                      max_na(data_meta$alt[which(data_meta$category == "middle")]))
                    )

  mid_low <- mea_na(c(min_na(data_meta$alt[which(data_meta$category == "middle")]),
                      max_na(data_meta$alt[which(data_meta$category == "low")]))
                    )

  rect(xleft = pardat$usr[1], ybottom = hig_mid, xright = pardat$usr[2],
       ytop = max_na(data_meta$alt)+300, col = my_blu, border = NA, lwd = 1)
  rect(xleft = pardat$usr[1], ybottom = mid_low,  xright = pardat$usr[2],
       ytop = hig_mid-0.1, col = my_bla, border = NA, lwd = 1)
  rect(xleft = pardat$usr[1], ybottom = -300,    xright = pardat$usr[2],
       ytop = mid_low-0.1,  col = my_red, border = NA, lwd = 1)

  for(i in 1:length(stat_IDs)){
    plot_points(data_in = data_in_me, data_meta = data_meta, ID = stat_IDs[i],
                no_col = no_col, is_hom = with_hom_dat)}

  #Plot annual average of category
  ele_HS <- mea_na(data_meta$alt[which(data_meta$category == "high")])
  ele_MS <- mea_na(data_meta$alt[which(data_meta$category == "middle")])
  ele_LS <- mea_na(data_meta$alt[which(data_meta$category == "low")])

  points(cyc_HS_me, ele_HS, pch = 24, cex = 1.4, col = col_HS, bg = "white")
  points(cyc_MS_me, ele_MS, pch = 24, cex = 1.4, col = col_MS, bg = "white")
  points(cyc_LS_me, ele_LS, pch = 24, cex = 1.4, col = col_LS, bg = "white")
  box(lwd=1)

  if(add_st_num){
    if(!no_col){
      hig_st_gap <- paste0(rep(" ", nchar(length(st_hig_nu)) + 3), collapse = "")
      mid_st_gap <- paste0(rep(" ", nchar(length(st_mid_nu)) + 4), collapse = "")
      low_st_gap <- paste0(rep(" ", nchar(length(st_low_nu)) + 3), collapse = "")
      
      mtext(paste0(hig_st_gap, " ", mid_st_gap, " ", length(st_low_nu)), col="red3",  side=3, line=.7, adj=1, padj=1, cex=.8)
      mtext(paste0(length(st_hig_nu), " :", mid_st_gap, " ", low_st_gap),  col="blue3", side=3, line=.7, adj=1, padj=1, cex=.8)
      mtext(paste0(hig_st_gap , " ", length(st_mid_nu), " :", low_st_gap),  col="black", side=3, line=.7, adj=1, padj=1, cex=.8)
    
      }else{
      mtext(paste0(length(st_all_nu)), col="grey30",  side=3, line=.7, adj=1, padj=1, cex=.8)
    }
  }
}
