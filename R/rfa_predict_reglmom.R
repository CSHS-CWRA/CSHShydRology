############################################################################
#' Flood quantiles estimates
#'
#' Predict the flood quantile of index-flood model for a specific scale
#' factor. By default the flood quantile of the first site (target) is returned.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object An output from \link{FitRegLmom}.
#'
#' @param p Probability associated to the flood quantiles.
#'
#' @param ci Logical. Should the confident intervals and the standard deviation
#' be evaluated?
#'
#' @param corr Intersite correlation. Either a matrix or a constant coefficient
#'   for all pairs
#'
#' @param nsim Number of simulations used for approximating
#'   the confident intervals.
#'
#' @param alpha Probability outside the confidence intervals.
#' 
#' @param out.matrix Logical. Should the bootstrap samples be returned.
#' 
#' @param ... Other parameters.
#' 
#' @seealso \link{FitRegLmom}, \link{Intersite}.
#'
#' @references 
#'
#' Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis:
#'   an approach based on L-moments. Cambridge Univ Pr.
#'
#' @import lmomRFA lmom
#'
#' @export
#'
#' @examples
#'
#' data(flowAtlantic)
#'
#' h <- GeoDist(flowAtlantic$info[,c('lon','lat')])
#' ams <- flowAtlantic$ams
#' ams$year <- format(ams$date, '%Y')
#' xd <- DataWide(ams ~ id + year, ams)
#'
#' xd <- FindNearest(xd, h[1,], 25)
#' h  <- FindNearest(h , h[1,], 25, row = TRUE)
#'
#' ## Fit the regional model
#' fit <- FitRegLmom(xd)
#'
#' ## estimate flood quantiles
#' predict(fit, c(.3,.7))
#'
#' ## Evaluate intersite correlation
#' isite <- Intersite(xd)
#'
#' predict(fit, ci = TRUE, corr = isite$model)
#'
predict.reglmom <-
  function(object,
           p = c(.5, .8, .9, .95, .98 , .99),
           ci = FALSE,
           corr = 0,
           nsim = 500,
           alpha = 0.05,
           out.matrix = FALSE,
           ...){

  ## The index-flood is the L1 of the target site
  indx <- object$lmom[1,1]

  ## Extract quantile function
  qfunc <- getFromNamespace(paste0('qua',object$distr), 'lmom')
  
  ## Evaluate flood quantiles
  hat <- indx * qfunc(p, object$para)

  if(ci | out.matrix){

    para <- coef(object)
    
    if(object$type == 'amax'){
      
      ## Extract the parameters of each site individually
      pfunc <- getFromNamespace(paste0('pel',object$distr), 'lmom')
        
      sim <- .SimulateAmaxQ(nsim = nsim, p = p, corr = corr,
                           nr = object$nrec, 
                           para = para, 
                           pfunc = pfunc, qfunc = qfunc)
    
      pid <- seq(ncol(para)+1)
      cname <- c('IF', colnames(para))
    
    } else if(object$type == 'pot'){
      
    ## Compute a bootstrap sample of the return levels
      sim <- .SimulatePotQ(nsim, 
                            p = p, 
                            nr = object$nrec, 
                            indx = object$lmom[,1],
                            k = para[,3])
      
      pid <- 1:2
      cname <- c('IF','kap')

    }
    
    simp <- sim[ , pid]
    colnames(simp) <- cname
    simq <- as.matrix(sim[ , -pid])
  
    
    ## Evaluate RMSE
    if(ncol(simq) == 1){
      res <- simq - hat
    } else{
      res <- t(apply(simq, 1, '-', hat))
    }
    
    rmse <- sqrt(colSums(res^2)/nrow(res))  
      
    ## Summarize the bootstrap sample
    ans <- data.frame(
        pred = hat,
        rmse = rmse,
        lower = apply(simq, 2, quantile, alpha/2),
        upper = apply(simq, 2, quantile, 1-alpha/2))

    
    rownames(ans) <- format(p, digits = 3)
    

  } else {
    ans <- hat
  }
  
  if(out.matrix){
    ans <- list(pred = ans, qua = simq, para = simp)
  }
  
  return(ans)
}

## Function that simulate a regional AMAX model and return the 
## flood quantiles of the target site using the L-moments
## nsim: number of simulation
## p : Exceeding probability
## nr: record length
## para: matrix of the at-site parameter (in rows)
## pfunc: Function that estimate parameter based on L-moments
## qfunc: Function that estimate flood quantiles
.SimulateAmaxQ <- function(nsim, p, nr, para, corr, qfunc, pfunc){

  nsite <- nrow(para)
  nmax <- max(nr)
  npara <- ncol(para)
  para.lst <- lapply(1:nsite, function(jj) para[jj,])
  
  ## Decompose the correlation matrix if necessary
  if(all(corr == 0)){
    nocorr <- TRUE
    
  } else {
    
    if(length(corr) == 1){
      corr <- matrix(corr, nsite, nsite)
      diag(corr) <- 1
    }

    nocorr <- FALSE
    cc <- chol(corr)  
  }
  
  ## Simulate the flood quantiles
  out <- replicate(nsim, {
    
    if(nocorr){
      ## Simulate directly uniform variable
      u <- lapply(nr, runif)
    } else {
      ## Simulate from multivariate normal
      z <- matrix(rnorm(nsite * nmax), nsite, nmax)
      z <- pnorm(crossprod(cc, z))
      u <- lapply(1:nsite, function(j) z[j, 1:nr[j]])
    }
    
    ## Transform marginal, compute the index and scale the data
    xlist <- mapply(qfunc, u, para.lst, SIMPLIFY = FALSE)
    mu <- vapply(xlist, function(z) sum(z)/length(z), FUN.VALUE = double(1))
    xlist <- mapply("/", xlist, mu, SIMPLIFY = FALSE)
       
    ## Evaluate the flood quantiles 
    xmom <- vapply(xlist, .samlmu, nmom = npara, FUN.VALUE = double(npara))
    rmom <- apply(xmom, 1, weighted.mean, w = nr)
    pp <- pfunc(rmom)
    qq <- qfunc(p, pp) * mu[1]
    c(mu[1],pp,qq)
    
  })

  return(t(out))
}



## Function that simulate a regional POT model and return the 
## regional flood quantiles using the L-moments
## nsim: number of simulations
## p : exceeding probability
## nr: record length
## indx: target average
## k: at-site shape parameter
.SimulatePotQ <- function(nsim, p, nr, indx, k){
  
  ## Function that simulate one site and return the LCV
  SimLcv <- function(znr, zindx, zk) {
    s <- replicate(nsim, zindx * rgpa(n = rpois(n=1, lambda = znr), 1 + zk, zk))
    
    mu <- vapply(s, function(z) sum(z)/length(z), double(1))
    s <- mapply('/', s, mu)
    
    n <- vapply(s,length, double(1))
    l <- vapply(s,lmom::samlmu, nmom = 2, FUN.VALUE = double(2))
    lcv <- l[2,]/l[1,]
    return(cbind(n, mu, lcv))
  }
  
  ## Simulate LCV for each site
  lcv <- mapply(SimLcv, nr, indx, k, SIMPLIFY = FALSE)

  ## extract weight based on record length, scale factor and LCV
  indx <- lcv[[1]][,2]
  w <- vapply(lcv, '[', , 1, FUN.VALUE = double(nsim))
  lcv <- vapply(lcv,'[', , 3, FUN.VALUE = double(nsim))
  
  ## Compute the regional parameter
  kap <- rowSums(w) / rowSums(w * lcv) - 2

  ## Compute the return levels
  q <- vapply(kap, function(z) qgpa(p, 1+z, z), double(length(p)))
  
  if(is.vector(q)){
    q <- as.matrix(indx * q)
  } else{
    q <- apply(q, 1, '*', indx)
  }
 
  ans <- cbind(indx, kap, q)
  
  return(ans)
  
}


