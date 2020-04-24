#' Factor values to numeric.
#'
#' Function to transform a factor to approximately its original numeric value.
#'
#' @param x factor value(s).
#' @return The numeric value of the factor.
#' @examples
#' f <- factor(sample(runif(5), 20, replace = TRUE))
#' fac2num(f)
#' @export
fac2num <- function(x) {as.numeric(levels(x))[x]}


