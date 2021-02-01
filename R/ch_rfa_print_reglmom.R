#' @export
#' @rdname FitRegLmom
print.reglmom <- function(x, ...){

  cat('\nRegional frequency analysis with pooling groups\n')
  
  if(x$type == 'pot')
    cat('\ntype: POT')
  
  
  cat('\nNb. site:', nrow(x$lmom))
  cat('\nStation-year:', sum(x$nrec))

  if (any(!is.null(x$stat))){
    cat('\nHomogeneity:', round(x$stat[1:3],2))

    if (x$type == 'amax'){
      cat('\n\nZ-scores (absolute):\n')
      print(round(abs(x$stat[4:8]),2))

    } else cat('\n')
  }

  cat('\nRegional L-moments:\n')
  print(x$rlmom, digits = 4)

  cat('\nDistribution:', x$distr)

  cat('\nParameter:\n')
  print(x$para, digits = 4)

}