#' AIC for nonstationary AMAX model
#' 
#' Return the AIC of the nonstationary AMAX model. 
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output
#' @param ... Other parameters
#' @param k Penalty per parameter.
#'
#' @export
#'
#' @seealso \link{AIC}.
#' 
AIC.nsamax <- function(object, ..., k = 2){
  np <- length(coef(object))-1
  return(k * np - 2*logLik(object)) 
}