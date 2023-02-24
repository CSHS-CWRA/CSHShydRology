#'@title ch_tr_signif()
#'  
#'@description Convert pvalues to integers 1 for NS and 2 for significant 
#'  using a pvalue that can be set (default is 0.05)
#'  
#'@param x an array of pvalues from statistical test 
#'@param pvalue critical value, default is 0.05
#'  
#'@return {x} {an array of indices 1 and 2, where 1 is NS and 2 is significant}
#'  
#'@author Paul Whitfield
#'@export
#'@examples
#'  sin <- c( -0.052, 0.34, 0.012, -.033, -0.55)
#'  sout <- ch_tr_signif(sin)
#'  # 1 1 2 2 1
  

ch_tr_signif <- function(x, pvalue = 0.05){
  x <- unlist(x)
  x <- replace(x, abs(x)  > pvalue, 1)
  x <- replace(x, abs(x) <= pvalue, 2)
  x <- as.numeric(x)
  return(x)
}


