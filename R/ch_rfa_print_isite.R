#' @export
#' @rdname Intersite
print.isite <- function(x, ...){
  cat('\nIntersite-correlation\n')
  cat('\nModel:', x$type)
  cat('\nMethod:', x$method)
  cat('\nNb. sites:', ncol(x$corr))

  if(x$type == 'exp'){
    cat('\nRMSE:', format(x$rmse, digits = 4))
    cat('\nParameter:\n')
    print(x$para, digits = 4)

  } else if(x$method == 'emp'){
    cat('\nAverage:', round(x$para[1],3))
  }
}