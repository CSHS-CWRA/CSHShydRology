#' Where is a specific day of the year is ?
#' 
#' Determines the location of a specific day of the year in a vector of 
#' \link{Date}.
#'
#' @param x Vector of dates
#' 
#' @param x0 The day to find. For example, '0715' standand for 07/15 July 15th.
#'
#' @export
#'
#' @examples
#' 
#' x <- seq(as.Date('2000/1/1'),as.Date('2020/12/31'), 'day')
#' yr <- which.day(x, c('0101','0715'))
#' head(x[yr])
#' 
which.day <- function(x, x0 = '0715'){
  mmdd <- paste0(format(x, '%m'), format(x, '%d'))
  return(which(mmdd %in% x0))  
}