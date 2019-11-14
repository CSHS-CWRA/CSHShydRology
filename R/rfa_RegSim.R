#############################################################################
#' Simulation of a regional dataset with intersite correlation.
#'
#' Returns a dataset containing multiple time series in the form of
#' a matrix where the sites are in columns.
#' Different record lengths can be specified for each site
#' and missing values are filled accordingly at the beginning.
#' The rows (time) are independent and the intersite correlation model
#' is based on a multivariate Normal distribution.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Matrix (in rows) of parameters or L-moments for all sites to 
#'   simulate.
#'   Can also be an output form \link{FitRegLmom}.
#'
#' @param distr Marginal distribution of each site.
#'   If only one value is passed as argument, the same is used for all sites.
#'
#' @param nrec Record lengths of the sites.
#'   If only one value is passed in argument, the same is used for all sites.
#'
#' @param corr Correlation matrix for the dependence between site.
#'   If only one value is passed, the correlation is assumed
#'   the for every pair of sites.
#'
#' @param corr.sqrt Logical. The correlation matrix `corr` is the squared of 
#'   version. Can be passed to speed up multiple calls.
#'
#' @param lmom Logical. Is the argument `x` a matrix of L-moments or
#' distribution parameters
#'
#' @param lscale Logical. Is the second L-moments the scale (`TRUE`)
#' or the LCV (`FALSE`).
#'
#' @param long  Logical. Should the output be returned in a long format.
#' 
#' @param n Number of simulations.
#' 
#' @param margin Which marginal distribution should be used. Either
#'   based on the at-site (\code{'atsite'}) or regional (\code{'reg'}) 
#'   distribution.
#' 
#' @param ... Other parameters.
#'
#' @export
#'
#' @examples
#'
#' ## Extract data
#' data(flowUngauged)
#' lmom <- flowUngauged[1:5, c('l1','lcv','lsk')]
#'
#' ## Simulate data base on at-site L-moments
#' sim <- RegSim(lmom, distr = 'gev', nrec = 11:15, corr = .4)
#'
#' head(sim)
#'
#' sim <- RegSim(lmom,
#'               distr = c(rep('gev',4),'gno'),
#'               nrec = 10:15,
#'               corr = .4, long = TRUE)
#' head(sim)
#'
#'  
RegSim <- function(x, ...) UseMethod('RegSim', x)

#' @export
RegSim.data.frame <- function(x, ...)
  RegSim.matrix(as.matrix(x), ...)

#' @export
#' @rdname RegSim
RegSim.matrix <-
  function(
    x,
    distr,
    nrec,
    corr = 0,
    corr.sqrt = FALSE,
    lmom = TRUE,
    lscale = FALSE,
    long = FALSE, 
    ...)
{

  ## Extract info
  nsite <- nrow(x)
  nrec.max <- max(nrec)

  ## Expend distr if constant
  if(length(distr) == 1)
    distr <- rep(distr, nsite)

  ## Create a matrix if only a scalar is passed as correlation
  if (length(corr) == 1){
    corr <- matrix(corr, nsite, nsite)
    diag(corr) <- 1
  }

    ## Create a matrix if only a scalar is passed as correlation
  if (length(nrec) == 1){
    nrec <- rep(nrec,nsite)
  }

  ## Decomposed the correlation matrix if necessary
  if( any(is.na(corr)))
    stop('Missing value in the correlation matrix')

  if (corr.sqrt){
    corr.L <- corr
  } else {
    corr.L <- chol(corr)
  }

  ## Simulate a normal copula
  u <- matrix(rnorm(nrec.max * nsite), nrec.max, nsite)
  u <- pnorm(u %*% corr.L)

  ## ------------------------------------------ ##
  ## Convert L-moment to parameter if necessary
  ## ------------------------------------------ ##

  if (lmom){
    ## Extract list of functions that convert L-moments to parameter
    ffunc <- lapply(distr,
                  function(d) getFromNamespace(paste0('pel',d),'lmom'))

    ## If LCV is passed correct the 2nd L-moments
    if(lscale == FALSE)
      x[,2] <- x[,2] * x[,1]

    ## Estimate parameters for every site
    para <- lapply(1:nrow(x), function(kk) ffunc[[kk]](x[kk, ]))

  } else {

    ## If parameter were passed as matrix, transform to list
    para <- lapply(split(x,1:nrow(x)), na.omit)
  }

  ## Extract a list of quantile functions for each site
  qfunc <- lapply(distr, function(d){
             getFromNamespace(paste0('qua',d),'lmom')
           })

  ## Transform uniform data to proper marginal distribution
  ans <- sapply(1:nsite, function(kk){
            qfunc[[kk]](u[,kk], para[[kk]])
          })

  ## Adjust record length by adding NA to early data
  Rlen <- function(x,n){
    nrm <- length(x)-n

    if(nrm > 0)
      x[1:nrm] <- NA

    return(x)
  }

  ans <- sapply(1:nsite, function(kk) Rlen(ans[,kk], nrec[kk]))

  ## ------------------------------------- ##
  ## Transform the data in the long format
  ## ------------------------------------- ##

  if(long){

   ans <- cbind(expand.grid(time = 1:nrow(ans), site = 1:ncol(ans)),
                   value = as.numeric(ans))

   ans <- ans[!is.na(ans$value), ]

  }

  return(ans)
}

#' @export
#' @rdname RegSim
RegSim.reglmom <- function(x, n = 1, corr = 0, margin = 'atsite', ...)
{

  nsite <- nrow(x$lmom)
  nmax <- max(x$nrec)

  corr <- as.matrix(corr)

  if(length(corr) == 1){
    corr <- matrix(corr[1], nsite, nsite)
    diag(corr) <- 1
  }

  corr.L <- chol(corr)

  ## ---------------------------------------------------------- ##
  ## Perform simulation
  ## ---------------------------------------------------------- ##

  if(margin == 'atsite'){
    lmom0 <- x$lmom
  } else if(margin == 'reg'){
    lmom0 <- t(replicate(nsite,x$rlmom))
  }

  Sim1 <- function(){
    out <- RegSim(lmom0, distr = x$distr, nrec = x$nrec,
                  corr = corr.L, corr.sqrt = TRUE, long = FALSE)

    return(mapply('*', as.data.frame(out), x$lmom[,1]))
  }

  ans <- replicate(n, Sim1())
  colnames(ans) <- x$station

  ## Case there is one simulation
  if(n == 1)
    ans <- ans[ , , 1]

  return(ans)
}

