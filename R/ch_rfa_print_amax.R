#' @export
print.amax <- function(x, ...){

  cat('\nAt-site frequency analysis\n')

  cat('\nDistribution:', x$distr,
      '\nAIC:', format(AIC(x), digits = 4),
      '\nMethod:', x$method)

  cat('\nEstimate:\n')
  print(x$para, digit = 4)

  if(all(!is.na(x$varcov))){

    se <- sqrt(diag(vcov(x)))
    names(se) <- names(x$para)

    cat('\nStd.err:\n')
    print(se, digits = 4)
  }

  cat('\nLmoments:\n')
  print(data.frame(l1 = x$lmom[1],
                   lcv = x$lmom[2]/x$lmom[1],
                   lsk = x$lmom[3]/x$lmom[2],
                   lkt = x$lmom[4]/x$lmom[2]), digits = 4)
}