#' Plot points representing annual averages.
#'
#' Plot points with color depending of elevation category of station and point type
#' depending on quality level of time series (see plot_cycle_elev())
#'
#' @param data_in Annual aveage values of moving average results.
#' @param data_meta Station meta data.
#' @param no_col Point color depending on elevation (FALSE) or all grey (TRUE)
#' @param is_hom If TRUE, distinction between homogenized and quality-checked data with different point type.
#' @return Point in plot.
#' @export
plot_points <- function(data_in, data_meta, ID, no_col = FALSE, is_hom = FALSE){

  #col2rgb("blue3")
  my_blu <- rgb(0, 0, 205, max=255, alpha = 160)
  #col2rgb("red3")
  my_red <- rgb(205, 0, 0, max=255, alpha = 160)
  #col2rgb("black")
  my_bla <- rgb(0, 0, 0,   max=255, alpha = 160)

if(data_meta$category[which(grepl(ID, data_meta$stn))] == "high")  {point_col <- "blue3"; backg <- my_blu}
if(data_meta$category[which(grepl(ID, data_meta$stn))] == "middle"){point_col <- "black"; backg <- my_bla}
if(data_meta$category[which(grepl(ID, data_meta$stn))] == "low")   {point_col <- "red3";  backg <- my_red}

if(data_meta$clim_reg[which(grepl(ID, data_meta$stn))] == "Jura")     {point_typ <- 21}
if(data_meta$clim_reg[which(grepl(ID, data_meta$stn))] == "Plateau")  {point_typ <- 22}
if(data_meta$clim_reg[which(grepl(ID, data_meta$stn))] == "Alps")     {point_typ <- 23}
if(data_meta$clim_reg[which(grepl(ID, data_meta$stn))] == "S_Alps")   {point_typ <- 25}

if(no_col){point_col <- "grey30"; backg = "grey30"}

# if(is_hom){
#   if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "homogenized")     {point_typ <- 8}
#   if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "quality-checked") {point_typ <- 19}
# }else{point_typ = 19}

if(is_hom){
  if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "homogenized"){
     points(data_in[which(grepl(ID, names(data_in)))], data_meta$alt[which(grepl(ID, data_meta$stn))],
            pch=point_typ, cex=1, col=point_col, bg = rgb(255, 238, 0,   max=255, alpha = 150))}#yellow
  if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "quality-checked"){
    points(data_in[which(grepl(ID, names(data_in)))], data_meta$alt[which(grepl(ID, data_meta$stn))],
           pch=point_typ, cex=1, col=point_col, bg = backg)}

}else{
  points(data_in[which(grepl(ID, names(data_in)))], data_meta$alt[which(grepl(ID, data_meta$stn))],
         pch=point_typ, cex=1, col=point_col, bg = backg)
}
}
