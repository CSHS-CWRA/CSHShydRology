#' PLotting a nonstationary AMAX model
#' 
#' Create a graph of the fitted vs residuals.
#'
#' @param x Output from \link{FitNsAmax}.
#' @param ... Other parameters.
#' 
#' @seealso \link{FitNsAmax}.
#' 
#' @export
#'
 
#' 
plot.nsamax <- function(x, ...){
  plot(fitted.values(x), residuals(x), ...)
  
  if(x$type == 'add'){
    abline(h=0)
  } else if(x$type == 'mult'){
    abline(h=1)
  }
  
}