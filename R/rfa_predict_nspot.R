##############################################################################
#' Flood quantiles for the nonstationary index-flood model at one site
#' 
#' Predict the return levels or design level of 
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output form \cite{FitNsPot}
#' 
#' @param rt Vector of return period to estimate.
#' 
#' @param newdata Data when to estimate flood quantiles.
#' 
#' @param reliability Logical. Should the reliability be returned instead of the
#'   flood quantiles.  
#'   
#' @param nsim Size of the bootstrap sample.
#'
#' @references 
#' 
#' Durocher, M., Burn, D. H., & Ashkar, F. (2019). Comparison of estimation 
#'   methods for a nonstationary index-flood model in flood frequency analysis 
#'   using peaks over threshold [Preprint]. 
#'   https://doi.org/10.31223/osf.io/rnepc
#'   
#' @seealso \link{predict.nspot}, \link{FitNsAmax}. 
#'
#' @export
#'
#' @examples
#' 
#' data(flowStJohn)
#'  
#' ## Fit the data using the ns-index-flood model 
#' fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95,
#'                 trend = ~ date, thresh = ~ date, declust = 'wrc', r = 14)
#' 
#' plot(fit)
#' 
#' ## Add the return levels to the graphics 
#' qhat <- predict(fit, rt = c(10, 100))
#' for(ii in 1:6) lines(flowStJohn$date, qhat[,ii], col = 'magenta')
#' 
#' ## Add the design level of the last 30 years
#' 
#' xref <- flowStJohn[flowStJohn$date > as.Date('1980-01-01'), ]
#' qhat <- predict(fit, rt = c(10, 50), newdata = xref, reliability = TRUE)
#' for(ii in 1:6) 
#'   arrows(min(xref$date), qhat[ii], max(xref$date), lwd = 2, col = 'cyan',
#'          code = 3, angle = 90, length = .05)
#' 
#' ## Assuming that a regional estimate of of the shape was available by other 
#' ## methods.        
#' predict(fit, rt = c(10, 100), reg.kappa = .3)
#' 
predict.nspot <- 
  function(object, 
           rt = c(2, 5, 10, 20, 50, 100), 
           newdata = NULL, 
           reliability = FALSE,
           reg.kappa = NULL){
  
  if(is.null(newdata))
    newdata <- object$data
  
  if(is.null(reg.kappa)){
    para <- coef(object, 'kappa')
  
  } else{
    para <- reg.kappa + c(1,0)
  }
  
  
  ftd <- fitted(object, newdata)
  p <- 1 - 1 / (object$ppy * rt)
  
    
  ############################
  ## quantile without trend ##
  ############################
  qua <- qgpa(p, para[1], para[2])
  
  if(object$trend == formula('~1') & object$threshold == formula('~1')){
    ans <- qua * object$trend.beta + object$threshold.beta
    names(ans) <- paste0('q',round(rt,2))
    return(ans)
  }
  ###########################
  ## quantile with trend   ##
  ###########################
  
  quas <- outer(ftd$trend, qua)
  quas <- apply(quas, 2, '+', ftd$thresh)
  
  if(!reliability){
    colnames(quas) <- paste0('q',round(rt,2))
    return(quas)
  }
  #################
  ## reliability ##
  #################
  
  lp <- log(p) * nrow(newdata)
  
  Frel <- function(z, jj){
    u <- (z-ftd$thresh)/ftd$trend
    lp[jj]-sum(log(pgpa(u, para[1], para[2])))
  }
    
  ## Determine boundary for searching design level
  bnd <- apply(quas, 2, range)
  bnd[1,] <- bnd[1,] - sqrt(.Machine$double.eps)
  bnd[2,] <- bnd[2,] + sqrt(.Machine$double.eps)
  
  ## Evaluate all design levels
  rel <- rep(0,length(lp))
  for(jj in seq_along(lp))
    rel[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  names(rel) <- paste0('q',round(rt,2))
  return(rel)
}

#' @export
#' @rdname predict.nspot
BootNsPot <- function(object, rt, newdata = NULL, reliability = FALSE,
                      nsim = 1000, verbose = TRUE, by.year = FALSE){
  
  if(is.null(newdata))
    newdata <- object$data
  
  ## allocate memory
  paras <- matrix(0, nsim, length(object$trend.beta)+1)
  colnames(paras) <- c('kappa',names(object$trend.beta))
  
  if(reliability){
    quas <- matrix(0, nsim, length(rt))
  } else {
    quas <- array(0, dim = c(nrow(newdata), length(rt), nsim))
  }
  
  colnames(quas) <- paste0('Q',rt)
  
  if(verbose)
    bar <- txtProgressBar()
  
  ## create a copy of the object
  copy <- object
  kap <- coef(object, 'kappa')
  
  for(ii in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, ii /nsim)
    
    nn <- rpois(1,length(object$peak))
    sid <- sort(sample.int(nrow(object$data),nn))
    
    ## Evaluate the trend at the sampled time
    xd <- object$data[sid,]
    ftd <- fitted(object, xd)
    trend.xmat <- model.matrix(object$trend, xd)
      
    ## Create a bootstrap sample of exceedances
    y.boot<- rgpa(nrow(xd), kap[1], kap[2]) * ftd$trend
    
    ############################################
    ## Fit the trend of the boostrap sample
    ############################################
    if(object$method %in% c('reg-mle','reg-lmom')){
      
      trend.fit <- .FitNsPotGlm(trend.xmat, 
                              y.boot, 
                              object$trend.beta, 
                              object$trend.link)
    
      copy$trend.beta <- paras[ii,-1] <- coef(trend.fit)
      trend.fitted <- fitted(trend.fit)
  
      if(object$method == 'reg-mle'){
        copy$kappa <- paras[ii,1] <- .FitNsPotKappaMle(y.boot / trend.fitted)
    
      } else if(object$method == 'reg-lmom'){
        copy$kappa <- paras[ii,1] <- .FitNsPotKappa(y.boot / trend.fitted)
      }
    
    } else{
      
       sol <- .FitNsPotMle(x = trend.xmat, y = y.boot, l = object$trend.link, 
                        s = c(-.1, object$trend.beta[-1]), 
                        object$trend.method, object$trend.control)
    
       copy$kappa <- sol$kappa
       copy$trend.beta <- sol$beta
      
    }
    
    ## compute flood quantile
    if(reliability){
      quas[ii,] <- predict(copy,  rt, newdata, TRUE)
    } else {
      quas[,,ii] <- predict(copy,  rt, newdata, FALSE)
    }
    
  }
  
  ans <- list(para = paras, qua = quas)
  class(ans)<- c('bootns', 'bootnspot')
  
  return(ans)
}

#' @export
simulate.nspot <- function(object, nsim = 1, newdata = NULL, ...){
  
  kap <- coef(object, 'kappa')
  
  ## Case we simulate at exactly specific time
  if(!is.null(newdata)){
    
    fit <- fitted(object)
    nc <- nrow(fit)
    
    ans <- apply(ans, 2, qgpa, kap[1], kap[2])
    ans <- apply(ans, 2, '*', fit$trend)
    ans <- apply(ans, 2, '+', fit$threshold)
    
                    
  ## Case we simulate at time following a poisson process                
  } else {
  
    Fsim <- function(ii){
      nn <- rpois(1,length(object$peak))
      sid <- sort(sample.int(nrow(object$data),nn))
      fit <- fitted(object, newdata = object$data[sid,])
      
      sim <- rgpa(nn, kap[1], kap[2]) * fit$trend + fit$threshold
      return(data.frame(id = ii, time = fit$time, sim = sim))
    }
    
    ans <- lapply(1:nsim, Fsim)
    ans <- do.call(rbind, ans)
    
  }
  
  rownames(ans) <- NULL
  
}