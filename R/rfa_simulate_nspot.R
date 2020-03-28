#' Simulating a nonstationary POT model.
#' 
#' Return the simulations of a nonstationary POT model.
#' 
#' @param object Output from \link{FitNsPot}.
#'
#' @param nsim Number of simulations.
#' 
#' @param seed Seed for the random number generator.
#' 
#' @param newdata Dataset of the covariates at the predicted dates.
#' 
#' @param ... Other parameters.
#' 
#' @seealso \link{FitNsPot}, \link{predict.nspot}.
#'
#' @export
simulate.nspot <- function(object, nsim = 1, seed, newdata = NULL, ...){
  
  if(!is.null(seed))
    set.seed(seed)
  
  kap <- coef(object, 'kappa')
  
  ## Case we simulate at exactly specific time
  if(is.null(newdata)){
    
    ftd <- fitted(object)
    nr <- nrow(ftd)
    
    ans <- replicate(nsim, rgpa(nr, kap[1], kap[2]), simplify = FALSE)
    ans <- lapply(ans, '*', ftd$trend)
    ans <- sapply(ans, '+', ftd$threshold)
    
                    
  ## Case we simulate at time following a poisson process                
  } else {
  
    newdata <- as.data.frame(newdata)
    
    ## number of year for the newdata and exceedance rate
    nr <- nrow(newdata)
    ny <- 1 / object$unit * object$ppy
    
    ftd <- fitted(object, newdata = newdata)
    
    Fsim <- function(ii){
      sid <- which(runif(nr) < ny)
      
      if(length(sid) > 1){
        sim <- rgpa(length(sid), kap[1], kap[2]) 
        sim <- sim * ftd$trend[sid] + ftd$threshold[sid]
        ans <- data.frame(id = ii, time = ftd$time[sim], sim = sim)
      
      } else {
        an <- data.frame(id = ii, time = NA, sim = NA)
      }
        
      return(ans)
    }
    
    ans <- lapply(1:nsim, Fsim)
    ans <- do.call(rbind, ans)
    
  }
  
  rownames(ans) <- NULL
  
  return(ans)
}