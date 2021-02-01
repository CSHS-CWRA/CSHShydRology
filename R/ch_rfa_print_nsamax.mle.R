#' @export
#' @rdname FitNsAmaxMle
print.nsamax.mle <- function(x, ...){
  cat('\nNonstationary model for annual maximums\n')
  cat('\nMethod: MLE')
  cat('\nDistribution:',x$distr)
  cat('\nType:', x$type )
  cat('\nParameters\n')
  print(x$para, digits = 4)
  
  if(!any(is.na(x$varcov)) )
    cat('\nStd. err.\n')
    print(sqrt(diag(x$varcov)), digits = 4)
}
