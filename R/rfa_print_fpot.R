#' @export
#' @rdname FitPot
print.fpot <- function(x, ...){

  cat('\nAnalysis of peaks over threshold')

  cat('\n\nThreshold : ', x$u)
  cat('\nNumber of excess : ', x$nexcess)

  if(!is.na(x$nyear)){
    cat('\nNumber of years : ', format(round(x$nyear,2)))
    cat('\nExcess rate (yearly): ', format(round(x$nexcess/x$nyear,4)))

  } else cat('\nExcess rate: ', format(round(x$nexcess/x$ntot,4)))

  cat('\nMean residual Life : ', format(x$mrl, digits = 6))

  cat('\n\nParameters :\n')
  print(x$estimate, digit = 4)

  if(!is.null(x$varcov)){
    cat('\nstd.err :\n')
    print(sqrt(diag(x$varcov)), digit = 4)
  }
}