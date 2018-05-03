#' Remove columns with only NA.
#'
#' Remove columns in data frame which onla have NA-values
#'
#' @param data_in Input data frame with numerich columns.
#' @return Data frame without only-NA-columns

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
