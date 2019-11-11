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
           nsim = 1000,
           alpha = 0.05,
           ...){

  ## The index-flood is the L1 of the target site
  indx <- object$lmom[1,1]


  ## Extract quantile function
  qfunc <- getFromNamespace(paste0('qua',object$distr), 'lmom')

  if(ci){

    ## extract the parameter of each site individually
    para <- coef(object)

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
    ans <- sitequantbounds(simq, rfit, sitenames = 1)
    colnames(ans) <- c('','pred', 'se', 'lower','upper')
    rownames(ans) <- format(p, digits = 3)
    class(ans) <- 'data.frame'
    attr(ans,'boundprob') <- NULL
    
    ans <- ans[,-1]

  } else {
    ## Compute the flood quantiles
    ans <- indx * qfunc(p, object$para)
  }
  
  return(ans)
}


