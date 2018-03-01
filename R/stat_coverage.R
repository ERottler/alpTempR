#' Remove columns with only NA.
#'
#' Function to determine sum automatically handling NAs. Calculates
#' sum disregarding NAs, except when all values are NA.
#'
#' @param data_in data frame with numeric columns...
#' @return ...
#' @examples
#' sum_na(c(2 ,3, NA))
#' sum_na(c(NA, NA, NA))
#' @export
stat_coverage <- function(data_in){
  data_cover <- data_in
  cols=1
  for(i in 1:ncol(data_in)){
    if(length(which(is.na(data_in[, i]))) == length(data_in[, i])){
      data_cover <- data_cover[, -cols]}else{
        cols <- cols + 1
      }
  }

  return(data_cover)
}
