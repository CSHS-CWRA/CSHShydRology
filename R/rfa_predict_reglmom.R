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
           ...){

  ## The index-flood is the L1 of the target site
  indx <- object$lmom[1,1]


  ## Extract quantile function
  qfunc <- getFromNamespace(paste0('qua',object$distr), 'lmom')

  if(ci){

    ## extract the parameters of each site individually
    para <- coef(object)

    if(object$type == 'amax'){
      ## Create a regfit object
      dd <- as.regdata(cbind(1:nrow(object$lmom),object$nrec,object$lmom),FALSE)
      rfit <- regfit(dd,object$distr)

      ## simulate flood quantile
      simq <- regsimq(qfunc,
                   para = para,
                   cor = corr,
                   index = indx,
                   nrec = object$nrec,
                   nrep = nsim,
                   fit = object$distr,
                   f = p,
                   boundprob = c(alpha/2, 1-alpha/2))

      ## extract confident interval and standard deviation
      ans <- sitequantbounds(simq, rfit, sitenames = 1)[,-1]
      
      colnames(ans) <- c('pred', 'se', 'lower','upper')
      class(ans) <- 'data.frame'
      attr(ans,'boundprob') <- NULL
    
    } else if(object$type == 'pot'){
      
      ## Compute a bootstrap sample of the return levels
      simq <- .SimulatePotQ(nsim, 
                            p = p, 
                            nr = object$nrec, 
                            l1 = object$lmom[,1],
                            k = para[,3])
      
      ## Summarize the bootstrap sample
      ans <- data.frame(
        pred = indx * qfunc(p, object$para),
        se = apply(simq, 2, sd),
        lower = apply(simq, 2, quantile, alpha/2),
        upper = apply(simq, 2, quantile, 1-alpha/2))
      
      
    }
    
    rownames(ans) <- format(p, digits = 3)
    

  } else {
    ## Compute the flood quantiles
    ans <- indx * qfunc(p, object$para)
  }
  
  return(ans)
}


## Function that simulate a regional POT model and return the 
## regional shape parameter estimated using the L-moments
## nsim: number of simulation
## p : matrix with 3 cols: nrec, scale factor, shape
.SimulatePotQ <- function(nsim, p, nr, l1, k){
  
  ## Function that simulate one site and return the LCV
  SimLcv <- function(znr, zl1, zk) {
    s <- replicate(nsim, zl1 * rgpa(n = rpois(n=1, lambda = znr), 1 + zk, zk))
    
    mu <- vapply(s, function(z) sum(z)/length(z), double(1))
    s <- mapply('/', s, mu)
    
    n <- vapply(s,length, double(1))
    l <- vapply(s,lmom::samlmu, nmom = 2, FUN.VALUE = double(2))
    lcv <- l[2,]/l[1,]
    return(cbind(n, mu, lcv))
  }
  
  ## Simulate LCV for each site
  lcv <- mapply(SimLcv, nr, l1, k, SIMPLIFY = FALSE)

  ## extract weight based on record length, scale factor and LCV
  indx <- lcv[[1]][,2]
  w <- vapply(lcv, '[', ,1, FUN.VALUE = double(nsim))
  lcv <- vapply(lcv,'[', ,3, FUN.VALUE = double(nsim))
  
  ## Compute the regional parameter
  kap <- rowSums(w) / rowSums(w * lcv) - 2

  ## Compute the return levels
  q <- vapply(kap, function(z) qgpa(p, 1+z, z), double(length(p)))
  
  if(is.vector(q)){
    q <- as.matrix(indx * q)
  } else{
    q <- apply(q, 1, '*', indx)
  }
 
  return(q)
  
}

