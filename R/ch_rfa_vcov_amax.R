#' @export
vcov.amax <- function(object, ...){

  ## if exist return the covariance matrix
  if(!is.null(object$varcov))
    ans <- object$varcov
  else
    ans <- NA

  ## return
  ans
}