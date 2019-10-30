#' @export
#' @rdname simulate.nsamax
simulate.nsamax.mle <- function(object, nsim = 1, seed = NULL, ...){

  if(!is.null(seed))
    set.seed(seed)
  
  if(nsim == 1)
    ans <- .simulate.nsamax.mle(object)
  else
    ans <- replicate(nsim, .simulate.nsamax.mle(object))

  return(ans)
}

.simulate.nsamax.mle <- function(object){

  ## Simulate a bootstrap sample
  para <- coef(object)
  Fqua <- getFromNamespace(paste0('q',object$distr), 'CSHShydRology')
  
  u <- runif(nrow(para))
  ans <- Fqua(u, para[,1], para[,2], para[1,3])
  
  return(as.numeric(ans))
}