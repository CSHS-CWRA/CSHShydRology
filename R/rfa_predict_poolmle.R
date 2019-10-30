#' Prediction of the flood quantile for regional model
#' 
#' Return the flood quantiles of given probabilities.
#'
#' @param object Output from \link{FitPoolMle}.
#' 
#' @param p Probabilities.
#' 
#' @param index Site specific parameters.
#' 
#' @param ... Other parameters.
#'
#' @export
#'
#' @examples
#' 
#' xd <- flowAtlantic$ams 
#' ## transpose by year
#' xd$year <- format(xd$date, '%Y')
#' xw <- DataWide(ams ~ id + year, xd)
#' 
#' ## Fitting a regional shape parameter
#' fit.gev <- FitPoolMle(xw, 'gev', type = 'mean')
#' 
#' ## A quick illustration on how to do bootstrap for inference
#' boot <- simulate(fit.gev, nsim = 30)
#' fboot <- lapply(boot, function(z) FitPoolMle(z, 'gev', type = 'mean') ) 
#' pboot <- sapply(fboot, getElement, 'para')
#' summary(pboot)
#' 
#' ## Inference for flood quantiles at target site
#' Fz <- function(z) predict(z, c(.9,.99), index = z$index[1])
#' qboot <- sapply(fboot, Fz)
#' summary(t(qboot))
#' 
#' 
predict.poolmle <- 
  function(object, 
          p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99), 
          index = NULL, ...){
  
  if(is.null(index)){
    index <- object$index
  } 
    
  ## Compute the flood quantile
  if(object$type == 'mean'){
    ans <- .predict.poolmle.mean(index, object$para, p, object$distr)
  
  } else if(object$type == 'cv'){
    ans <- .predict.poolmle.cv(index, object$para, p, object$distr)
  
  } else if(object$type == 'shape' & object$distr == 'gpa'){
    ans <- .predict.poolmle.pot(index, object$para, p)
  
  } else if(object$type == 'shape'){
    ans <- .predict.poolmle.shape(index, object$para, p, object$distr)
  }
  
  ## format the output
  if(nrow(ans) == 1){
    ans <- as.vector(ans)
    names(ans) <- round(p,2)
    
  } else if(ncol(ans) == 1){
    ans <- as.vector(ans)
    names(ans) <- names(index)
    
  } else{
    colnames(ans) <- round(p,3)
    
    if(is.vector(index))
      rownames(ans) <- names(index)
    else
      rownames(ans) <- colnames(index)
  }
  
  return(ans) 
}

.predict.poolmle.mean <- function(index, para, p, distr){
  
  if(distr == 'gpa'){
    qua <- qgpa(p, para[1], para[2]) 
  } else {
    ## extract
    qfun <- getFromNamespace(paste0('q',distr),'CSHShydRology')
    qua <- qfun(p, para[1], para[2], para[3]) 
  }
  
  return(outer(index, qua))
  
}

.predict.poolmle.pot <- function(index, para, p){
  
  ans <- sapply(p, qgpa, index, kap = para) 
  
  if(is.matrix(ans)){
    colnames(ans) <- p
  } else{
    names(ans) <- p
    ans <- t(ans)
  }
  
  return(ans)
}

.predict.poolmle.cv <- function(index, para, p, distr){
  
  qfun <- getFromNamespace(paste0('q',distr),'CSHShydRology')
  
  ans <- sapply(p, qfun, index, index * para[1], para[2])
  
  if(is.matrix(ans)){
    colnames(ans) <- p
  } else{
    names(ans) <- p
    ans <- t(ans)
  }
  
  return(ans)
  
}

.predict.poolmle.shape <- function(index, para, p, distr){
  
  qfun <- getFromNamespace(paste0('q',distr), 'CSHShydRology')
  
  index <- as.matrix(index)
  
  ans <- sapply(p, qfun, index[1,], index[2,], para)
  
  if(is.matrix(ans)){
    colnames(ans) <- p
  } else{
    names(ans) <- p
    ans <- t(ans)
  }
  
  return(ans)
  
}

