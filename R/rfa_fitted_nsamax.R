#' Fitted trend for the nonstationary AMAX model
#' 
#' Return the fitted trend of the nonstationary AMAX model at given dates.
#' 
#' @param object Output from \link{FitNsAmax}.
#'
#' @param newdata Covariates at the fitted dates. 
#' 
#' @param ... Other parameters.
#'
#' @seealso \link{FitNsAmax}, \link{predict.nsamax}.
#' 
#' @export
#' 
fitted.nsamax <- function(object, newdata = NULL, ...){
  
  if(is.null(newdata))
    return(object$fitted)
  
  xmat <- model.matrix(attr(object$data,'terms'), newdata)
  ans <- as.numeric(xmat %*% object$beta)
  
  if(object$type == 'mult')
    ans <- exp(ans)
  
  return(ans)
}