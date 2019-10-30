#' Simulation of a pooling group models
#' 
#' Return the simulation of multiple sites following a regional model.
#'
#' @param object Output from \link{FitPoolMle}. 
#' @param nsim Number of simulations.
#' @param seed Seed for random number generator.
#' @param ... Other parameters.
#'
#' @export
#'
#' @seealso \link{FitPoolMle}.
#' 
simulate.poolmle <- function(object, nsim = 1, seed = NULL, ...){
  
  nsite <- object$dim[2]
  ntime <- object$dim[1]
  
  o <- list(na = object$na,
              dim = object$dim,
              distr = object$distr)
  
  class(o) <- 'poolmargin'
  
  ## Create a matrix of the marginal parameters
  if(object$type == 'mean'){
    para <- replicate(nsite, object$para)
    colnames(para)<- names(object$index)
    
  } else if(object$type == 'cv'){
    loc <- object$index
    para <- rbind(loc,
                  loc * object$para[1],
                  object$para[2])
    
  } else if(object$type == 'shape'){
    para <- rbind(object$index, object$para)
  }
  
  ## Format the labels of the marginal parameters
  if(object$distr == 'pe3'){
    rownames(para) <- c('mu','sigma','gamma')
  } else if(object$distr == 'gpa'){ 
    rownames(para) <- c('alpha','kappa')
  } else {
    rownames(para) <- c('xi','alpha','kappa')
  }
  
  o$para <- para  
  ans <- simulate(o, nsim = nsim, seed = seed, ...)
  
  ## If the index-flood model is used, the simulation must be multiplied 
  ## by the scale factor.
  if(object$type == 'mean')
    ans <- lapply(ans, function(z) t(apply(z, 1,'*', object$index)))
  
  return(ans)
}
