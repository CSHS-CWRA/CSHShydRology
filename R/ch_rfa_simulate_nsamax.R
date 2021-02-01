#' Simulation of nonstationary AMAX model.
#' 
#' Return the simulation of a fitted nonstationary AMAX model.
#'
#' @param object Output from \link{FitNsAmax}.
#' @param nsim Number of simulations.
#' @param seed Seed for random number generator.
#' @param ... Other parameters.
#'
#' @importFrom lmomco rlmomco qlmomco
#' @export
#'
#' @seealso \link{FitNsAmax}.
#' 
simulate.nsamax <- function(object, nsim = 1, seed = NULL, ...){

  if(!is.null(seed))
    set.seed(seed)
  
  if(nsim == 1)
    ans <- .simulate.namax(object)
  else
    ans <- replicate(nsim, .simulate.namax(object))

  return(ans)
}

.simulate.namax <- function(object){

  ## Simulate a bootstrap sample
  sim <- rlmomco(length(object$fitted),  object$para)

  if(object$type == 'mult'){
  ans <- sim * object$fitted
  
  } else if(object$type == 'add'){
    ans <- sim + object$fitted
  }
  
  return(ans)
}


#' @rdname FitNsAmax