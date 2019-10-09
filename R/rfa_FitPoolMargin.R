#' Fitting marginal distributions of a pooling group
#' 
#' Returns an object containing the parameter estimates of each site sites
#' inside a pooling groups. Estimation can be performed by L-moments or 
#' maximum likelihood for the five distributions: GEV, GLO, GNO, PE3, GPA. 
#'
#' @param x Data with time in rows and sites in columns.
#' @param distr Fitting distribution
#' @param method Estimation method.
#' @param method.optim, ... Further arguments pass to \link{optim} when 
#'   maximum likelihood method is used 
#'
#' @return
#' 
#' \item{para}{Matrix of the estimated parameters.}
#' \item{distr}{Fitting distribution.}
#' \item{na}{Indices of the missing values.}
#' \item{dim}{Dimension of the input data.}
#'
#' @seealso \link{FitPoolMle}, \link{Amax}, \link{GPA}.
#'
#' @export
#' 
#' @examples
#' 
#' x <- replicate(3, rgev(100, 100, 30, 0))
#' 
#' fit <- FitPoolMargin(x, 'gev', method = 'mle')
#' 
#' print(fit)
#' predict(fit, p = c(.9, .99))
#' 
#' xx <- simulate(fit, nsim = 2, corr = .2)
#' 
#' 
FitPoolMargin <- function(x, distr, method = 'lmom', 
                          method.optim = 'BFGS', ...){
 
  x <- as.data.frame(x)
  
  if(ncol(x) < 2)
    stop('Must have more than one site')
  
  ## Verify the choice of the selected distribution
  if(!(distr %in% c('gpa', 'gev', 'glo', 'gno', 'pe3')))
    stop('Must provide a valid distribution.')
  
  ## Transform data into list
  xl <- lapply(x, na.omit)  
  
  ans <- list(para = NULL,
              distr = distr,
              na = which(is.na(x)),
              dim = dim(x))
  
  ## Compute the model parameters
  if(distr == 'gpa' & method == 'lmom'){
    ans$para <- sapply(xl, fgpaLmom)
  } else if(method == 'lmom'){
    ans$para <- sapply(xl, fAmax, distr)
  } else if(distr == 'gpa' & method == 'mle'){
    ans$para <- sapply(xl, fgpa1d)
  } else if(distr %in% c('gev','glo','gno','pe3') & method == 'mle'){
    ffun <- getFromNamespace(paste0('f',distr),'CSHShydRology')
    ans$para <- sapply(xl, ffun, method = method.optim, ...)
  } else {
    stop('Must choose a valid distribution and estimation method.') 
  }
  
  class(ans) <- 'poolmargin' 

  return(ans)
}

print.poolmargin <- function(x){
  cat('\nRegional model for pooling groups\n')
  
  cat('\nType: Margin')
  cat('\nDistribution:', x$distr)
  cat('\nNumber of sites:', ncol(x$para))
  
  cat('\nAverage parameters:\n')
  
  print(rowMeans(x$para), digits = 3)
  
  cat('\nStd parameters:\n')
  print(apply(x$para,1,sd), digits = 3)
  
}

#' @export
predict.poolmargin <-function(object, p = c(.5, .8, .9, .95, .98, .99)){
  
  if(object$distr == 'gpa'){
    ans <- apply(object$para,2, function(z) qgpa(p, z[1],z[2]))  
  
  } else {
    qfun <- getFromNamespace(paste0('q',object$distr), 'CSHShydRology')
    ans <- apply(object$para, 2, function(z) qfun(p, z[1],z[2], z[3]))
  }

  if(is.matrix(ans))
    rownames(ans) <- round(p,3)
  
  return(ans)
}

#' @export
simulate.poolmargin <- function(object, nsim = 1, corr = 0){
  
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

#' @export
coef.poolmargin <- function(object) object$para
