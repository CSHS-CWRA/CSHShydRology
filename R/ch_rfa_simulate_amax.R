#' @export
simulate.amax <- function(object, nsim, seed = NULL, ...){
  
  if(!is.null(seed))
    set.seed(seed)
  
  return(rAmax(nsim, object$estimate, object$distr))  
}