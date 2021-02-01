#' @export
print.poolmargin <- function(x, ...){
  cat('\nRegional model for pooling groups\n')
  
  cat('\nType: Margin')
  cat('\nDistribution:', x$distr)
  cat('\nNumber of sites:', ncol(x$para))
  
  cat('\nAverage parameters:\n')
  
  print(rowMeans(x$para), digits = 3)
  
  cat('\nStd parameters:\n')
  print(apply(x$para,1,sd), digits = 3)
  
}