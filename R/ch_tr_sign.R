#'@title ch_tr_sign
#'
#'@description Converts MK (or other) slopes to integers 1-2-3 (negative, 
#'  none, positive). These indices can be used to indicate trend direction.
#'  
#'  
#'@param x an array of slopes
#'@param offset the amount of shift to make values positive integers, default is 2.

#'@return {x} {an array of indices 1, 2, 3}
#'  
#'@author Paul Whitfield
#'@export
#'@examples
#'  mkin <- c( -0.23, 0.34, 0.0, .033, -0.55)
#'  mkout <- ch_tr_sign(mkin)
#'  # 1 3 2 3 1


ch_tr_sign <- function(x, offset = 2){
  x <- unlist(x)
  x <- x/abs(x)
  x <- replace(x, is.nan(x), 0)
  x <- x + offset
  x <- as.numeric(x)
  return(x)
}

