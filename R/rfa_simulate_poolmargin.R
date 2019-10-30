#' @export
simulate.poolmargin <- function(object, nsim = 1, seed = NULL, corr = 0, ...){
  
  if(!is.null(seed))
    set.seed(seed)
  
  nsite <- object$dim[2]
  ntime <- object$dim[1]
  
  ## Prepare the intersite correlation matrix
  corr <- as.matrix(corr)
  
  if(any(dim(corr) != c(nsite,nsite))){
    corr <- matrix(corr[1], nsite, nsite)
    diag(corr) <- 1
  }
  
  corr.sqrt <- chol(corr)
  
  ## create a list of uniform random variables
  u <- replicate(nsim, 
                 mnormt::rmnorm(ntime, mean = 0, sqrt = corr.sqrt),
                 simplify = FALSE)
  
  u <- lapply(u, pnorm)
  
  ## Reformat the output to be a data.frame with proper columns names
  u <- lapply(u, as.data.frame)
  u <- lapply(u, 'colnames<-', names(object$index))
  
  ## Create a function that returns the quantiles of a site
  if(object$distr == 'gpa'){
    Fz <- function(z,p) qgpa(z, p[1], p[2])
  } else {
    qfun <- getFromNamespace(paste0('q',object$distr),'CSHShydRology')
    Fz <- function(z,p) qfun(z, p[1], p[2], p[3]) 
  }
  
  ## Evaluate all the quantiles
  pp <- as.data.frame(object$para)
  ans <- lapply(u, function(z) mapply(Fz, z, pp))
  
  ## Add the missing values
  ans <- lapply(ans, '[<-', object$na, NA)
  
  ans <- lapply(ans, 'colnames<-', colnames(object$para))
  
  if(nsim == 1){
    ans <- ans[[1]]
  }  
    
  return(ans)
}