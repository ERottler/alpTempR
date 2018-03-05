#' Plot points.
#'
#' ...
#'
#' @param dates ...
#' @param values ...
#' @param start_year ...
#' @param end_year ...
#' @param window_width ...
#' @param cover_thresh ...
#' @param method_analys ...
#' @return ...
#' @examples
#' pres2ahum(vapor_pres = 5.5, temp = 20)
#' @export
plot_points <- function(data_in, data_meta, ID, no_col = FALSE, is_hom = FALSE){

if(data_meta$category[which(grepl(ID, data_meta$stn))] == "high")  {point_col <- "blue3"}
if(data_meta$category[which(grepl(ID, data_meta$stn))] == "middle"){point_col <- "black"}
if(data_meta$category[which(grepl(ID, data_meta$stn))] == "low")   {point_col <- "red3"}

if(no_col){point_col <- "grey30"}

if(is_hom){
  if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "homogenized")     {point_typ <- 8}
  if(data_meta$data_qual[which(grepl(ID, data_meta$stn))] == "quality-checked") {point_typ <- 19}
}else{point_typ = 19}

points(data_in[which(grepl(ID, names(data_in)))], data_meta$alt[which(grepl(ID, data_meta$stn))],
       pch=point_typ, cex=1, col=point_col)
}
