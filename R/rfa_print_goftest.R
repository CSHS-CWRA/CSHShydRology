#' @export
print.goftest <- function(x, ...){

  if(x$method %in% c('ad','adtab'))
    methodName <- 'Anderson-Darling'
  else if(x$method == 'shapiro')
    methodName <- 'Modified Shapiro-Wilk'

  cat('\nGoodness-of-fit test\n',
        '\nTest =', methodName,
        '\nDistribution =', x$distr)

  cat('\nstatistic :', round(x$stat,4),
        '\np-value :', round(x$pvalue,4), '\n\n')
}