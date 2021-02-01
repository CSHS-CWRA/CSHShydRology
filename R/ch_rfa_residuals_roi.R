#' @export
#' @rdname FitRoi
residuals.roi <- function(object, x, fold = 5, ...){
  response <- eval(object$call$phy[[2]], envir = x)
  return(predict(object, x = x, fold = fold) - response)
  
}