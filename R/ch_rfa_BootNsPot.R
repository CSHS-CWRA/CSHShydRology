#' Bootstrap sample of the nonstationary POT model
#' 
#' @return Returns a list of the bootstrap samples of the parameters and flood quantiles for the 
#' nonstationary POT model.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param object Output form \link{FitNsPot}.
#' 
#' @param rt Vector of return period to estimate.
#' 
#' @param x Dataset used to fit the nonstationary POT model.
#'    
#' @param newdata Covariates. If \code{NULL} the input data that served to fit 
#'   the model is used
#'   
#' @param reliability Logical. Should the flood quantile associated with 
#'   reliability level be returned. In that case, \code{newdata} must represent
#'   the period of interest. See Durocher et al. (2019) for more details.
#'   
#' @param nsim Number of simulations.
#' 
#' @param verbose Logical. Should a progress bar be displayed.
#'
#' @seealso \link{FitNsPot}, \link{predict.nspot}.
#' @export
#'
BootNsPot <- 
  function(object, x,
           rt = c(2,5,10,20,50,100),
           newdata = NULL, 
           reliability = FALSE,
           nsim = 1000, 
           verbose = TRUE){
  
  if(is.null(newdata))
    newdata = x
  
  newdata <- as.data.frame(newdata)
    
  ## allocate memory
  paras <- matrix(0, nsim, length(object$trend$beta)+1)
  colnames(paras) <- c('kappa',names(object$trend$beta))
  
  if(reliability){
    quas <- matrix(0, nsim, length(rt))
  } else {
    quas <- array(0, dim = c(nrow(newdata), length(rt), nsim))
  }
  
  colnames(quas) <- paste0('Q',rt)
  
  if(verbose)
    bar <- txtProgressBar()
  
  ## create a copy of the object to be modified
  copy <- object
  kap <- coef(object, 'kappa')
  
  ## daily exceedance rate
  nr <- nrow(object$data)
  ny <- 1 / object$unit * object$ppy
  
  ## extract the design matrix for all data
  trend.xmat <- model.matrix(attr(object$trend$data, 'term'), x)
  trend.all <- trend.xmat %*% object$trend$beta
    
  for(ii in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, ii /nsim)
    
    ## Create a bootstrap sample of exceedances
    sid <- which(runif(nr) < ny)
    yboot <- rgpa(length(sid), kap[1], kap[2]) * trend.all[sid]
    
   
    ##---- Fit the trend of the boostrap sample ----##
    
    if(object$method %in% c('reg-mle','reg-lmom', 'reg-mom')){
      
      trend.fit <- .FitNsPotGlm(trend.xmat[sid,], 
                              yboot, 
                              object$trend$beta, 
                              object$trend$link)
    
      copy$trend$beta <- paras[ii,-1] <- coef(trend.fit)
      trend.fitted <- fitted(trend.fit)
  
      if(object$method == 'reg-mle'){
        copy$kappa <- paras[ii,1] <- .FitNsPotKappaMle(yboot / trend.fitted)
    
      } else if(object$method == 'reg-lmom'){
        copy$kappa <- paras[ii,1] <- fgpaLmom(yboot / trend.fitted)[2]
      
      } else if(object$method == 'reg-mom'){
        copy$kappa <- paras[ii,1] <- fgpaMom(yboot / trend.fitted)[2]
      }
    
    } else{
      
       sol <- .FitNsPotMle(x = trend.xmat, y = yboot, l = object$trend$link, 
                        s = c(-.1, object$trend$beta[-1]), 
                        object$trend$method, object$trend$control)
    
       copy$kappa <- sol$kappa
       copy$trend$beta <- sol$beta
      
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