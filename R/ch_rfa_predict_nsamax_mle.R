################################################################################
#' Prediction of flood quantiles for the nonstationary model using MLE
#' 
#' Return the flood quantiles associatied to specifc level of 
#' exceeding probability or reliability level.
#' The function \code{ch_rfa_boot_nsamaxMle} is used to obtain bootstrap samples of 
#' the parameters and flood quantiles.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output of \link{FitNsAmaxMle}.
#'  
#' @param p Exceeding probability associated to return period or reliability level.
#'   The reliability is \code{p^s} where \code{s} is the size of \code{newdata}. 
#' 
#' @param newdata Covariates. If \code{NULL} the input data that served to fit 
#'   the model is used
#'   
#' @param type Type of output. Must be one of 
#'   \code{'location','quantile', 'reliability'}. 
#'   
#' @param reliability  Logical. Should the flood quantile associated with 
#'   reliability level be returned. See \link{FitNsAmax}.
#'   
#' @param nsim Sample size.
#' 
#' @param alpha Confidence level.
#' 
#' @param verbose Logical. Should a progress bar be displayed.
#' 
#' @param ... Other parameters.
#' 
#' @seealso \link{FitNsAmax}, \link{predict.nsamax.mle}.
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' data(flowStJohn)
#' 
#' amax <- ExtractAmax(flow~date, flowStJohn, tol = 365)
#' 
#' ## Recenter year to start at 1
#' amax$year <- as.integer(amax$yy) - 1926
#' 
#' ## Add a trend
#' amax$flow <- amax$flow + amax$year * 10
#' 
#' fit <- FitNsAmaxMle(flow~year, amax, 'gno', type = 'add')
#' 
#' ## Last 30 years
#' ref.period <- 59:88
#'
#' ## trend ##
#' loc <- predict(fit, c(.5, .9), type = 'loc')
#' hat <- predict(fit, c(.5, .9))
#' rel <- predict(fit, c(.5, .9), newdata = amax[ref.period,], 
#'                type = 'rel')
#' 
#' ## Data cloud
#' plot(flow~year, amax)
#' 
#' ## location parameter
#' lines(loc, col = 'red')
#' 
#' ## Flood quantiles
#' for(ii in 1:2) lines(hat[,ii], col = 'red', lty = 2)
#' for(ii in 1:2) lines(cbind(ref.period, rel[ii]), lwd = 3, col = 'blue')
#' 
#' ## Using boostrap to evaluate the model uncertainty
#' bs <- ch_rfa_boot_nsamaxMle(fit, c(.5, .9), nsim = 50, reliability = TRUE,
#'                  method = 'Nelder-Mead', control = list(maxit = 2000))
#' 
#' out <- summary(bs)
#' print(out)
#' 
predict.nsamax.mle <- 
  function(object, 
           p = c(.5, .8, .9, .95, .98, .99), 
           newdata = NULL, 
           type = 'quantile',
           ...){
  
  if(is.null(newdata))
    newdata <- object$data
  
  newdata <- as.data.frame(newdata)

  ## Return trend or location component ##
  xmat <- model.matrix(attr(object$data,'term'), newdata)
  np <- ncol(xmat)
  
  loc <- as.numeric(xmat %*% object$para[1:np])

  if(type %in% c('location','loc'))
    return(loc)
  
  ## Return the scale component ##
  
  scl <- rep(object$para[np+1], nrow(xmat))
  names(scl) <- NULL

  if(object$type == 'mult')
    scl <- scl * abs(loc)
  
  if(type == 'scale')
    return(scl)
  
  shp <- object$para[np+2]
  
  ## return the quantile ##
  
  Fqua <- getFromNamespace(paste0('q',object$distr),'CSHShydRology')
  qua <- sapply(p, Fqua, loc, scl, shp)
  colnames(qua) <- as.character(round(p,3))
  rownames(qua) <- rownames(xmat)
  
  if(type %in% c('quantile','qua'))
    return(qua)
  
  ## reliability ##
  
  if(!(type %in% c('rel', 'reliability')))
    stop('Must select a valid type.')
  
  Fcdf <- getFromNamespace(paste0('p',object$distr),'CSHShydRology')
  
  lp <- log(p) * nrow(newdata)
  
  Frel <- function(z, jj) lp[jj]-sum(log(Fcdf(z, loc, scl, shp)))

  bnd <- apply(qua, 2, range)
  bnd[2,] <- bnd[2,] + sqrt(.Machine$double.eps)
  
  rel <- rep(0,length(lp))
  for(jj in seq_along(lp))
    rel[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  names(rel) <- as.character(round(p,3))
  
  return(rel)
}


#' @export
#' @rdname predict.nsamax.mle
ch_rfa_boot_nsamaxMle <- 
  function(object,
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE,
           nsim = 1000, 
           alpha = 0.95, 
           verbose = TRUE, ...){
  
  if(is.null(newdata))
    newdata <- object$data  
  
  ## allocate memory
  paras <- matrix(0, nsim, length(object$para))
  
  if(reliability){
    quas <- matrix(0, nsim, length(p))
  } else {
    quas <- array(0, dim = c(nrow(newdata), length(p), nsim))
  }
  
  ## Create a formula and data.frame for fitting simulation
  xjj <- model.matrix(attr(object$data,'term'), object$data)
  xjj <- data.frame(0, xjj)
  newdata0 <- model.matrix(attr(object$data,'term'), newdata)
  newdata0 <- data.frame(0, newdata0)
  form0 <- as.formula(paste0(colnames(xjj)[1],'~ . -1'))
  
  
  if(verbose)
    bar <- txtProgressBar()
  
  for(jj in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, jj / nsim)
    
    ## Simulate a bootstrap sample
    xjj[,1] <- simulate(object)
    
    ## Fit the model
    fit <- try(FitNsAmaxMle(form = form0, x = xjj, varcov = FALSE,
                     distr = object$distr, type = object$type, ...))
    
    ## Return missing if fails
    if(class(fit) == 'try-error'){
      paras[jj,] <- NA
          
      if(reliability){
        quas[jj,] <- NA
      } else{
        quas[,,jj] <- NA
      }
      
      next  
    }
    
    ## save results
    paras[jj,] <- fit$para
    
    if(reliability){
      quas[jj,] <- predict(fit, p, newdata0, 'reliability')
    } else{
      quas[,,jj] <- predict(fit, p, newdata0, 'quantile')
    }
  }    
  
  colnames(paras) <- names(object$para)
  colnames(quas) <- as.character(round(p,3))
  
  ans <- list(para = paras,
              qua = quas)
  
  class(ans) <- 'bootns'
  
  return(ans)
}


