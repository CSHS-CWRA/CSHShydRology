#' Detrendend of the nonstationary AMAX model
#' 
#' Return the detrended values of the nonstationary AMAX model.
#'
#' @param object Output from \link{FitNsAmax}.
#' 
#' @param scale Logical. Should the residuals be transformed to a standard 
#'   normal distribution.
#' 
#' @param ... Other parameters
#'
#' @seealso \link{fitted.nsamax}, \link{FitNsAmax}.
#'
#' @importFrom lmomco plmomco
#' @export
#'
residuals.nsamax <- function(object, scale = FALSE, ...){
  
  resp <- model.response(object$data) 
  
 if(object$type == 'mult'){
    ans <- as.numeric(resp / object$fitted)
    
  } else if(object$type == 'add'){
    ans <- as.numeric(resp - object$fitted)
  }   
  
  if(scale)
    ans <- qnorm(plmomco(ans, object$para))
  
  return(ans)
}