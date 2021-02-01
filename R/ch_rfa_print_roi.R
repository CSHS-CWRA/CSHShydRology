#' @export
#' @rdname FitRoi
print.roi <- function(x, ...){
 cat('\n\nRegion of Influence (ROI)\n')
 cat('\nNumber of sites:', x$call$nsite)
 cat('\nNumber of targets:', x$call$npred)

 cat('\n\nRegression:')
 cat('\n  Physic:', format(x$call$phy))
 cat('\n  size:', sort(unique(x$call$nk)))
 cat('\n  Similarity', format(x$call$similarity))
 cat('\n  Kernel:', format(x$call$ker))

 if(!is.null(x$model)){
   cat('\n\nKriging:')
   cat('\n  Coord:', format(x$call$kriging),'\n\n')
   print(as.data.frame(x$model)[,1:4])
 }

}