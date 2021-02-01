#' @export
#' @rdname FitNsAmax
print.nsamax <- function(x, ...){
  cat('\nNonstationary model for annual maximums\n')
  cat('\nMethod: LS+LMOM')
  cat('\nDistribution:',x$para$type)
  cat('\nType:', x$type )
  cat('\nParameters\n')
  print(x$para$para, digits = 4)
  cat('\nTrend:\n')
  print(x$beta, digit = 4)
}