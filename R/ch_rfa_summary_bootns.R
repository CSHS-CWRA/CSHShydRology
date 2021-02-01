#' Summary bootstrap sample for the nonstationary AMAX model
#' 
#' Return the standard deviation and the confident interval of the parameters
#' and flood quantiles.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param object Output from \link{BootNsAmax} or \link{BootNsAmaxMle}.
#'
#' @param variable Variable to summarize. Must be one of 
#' \code{('para', 'beta', 'qua')}.
#' 
#' @param alpha Confidence level.
#' 
#' @param ... Other parameters. 
#'
#' @export
summary.bootns <- function(object, variable = 'para', alpha = 0.05, ...){
 
  if(variable == 'qua' & length(dim(object$qua)) == 3 ){
    id <- 1:2
  } else{
    id <- 2
  }
      
  vmu <- apply(object[[variable]], id, mean)
  vse <- apply(object[[variable]], id, sd)
  vlb <- apply(object[[variable]], id, quantile, alpha/2)
  vub <- apply(object[[variable]], id, quantile, 1-alpha/2)
  
  ans <- data.frame(mean = vmu, se = vse, lower = vlb, upper = vub)
  
  return(ans)
}
