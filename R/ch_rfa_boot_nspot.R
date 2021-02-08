#' Bootstrap sample of the nonstationary POT model
#' 
#' @return Returns a list of the bootstrap samples of the parameters and flood quantiles for the 
#' nonstationary POT model.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#' 
#' @param object Output form \link{FitNsPot}.
#' 
#' @param x Dataset used to fit the nonstationary POT model.
#' 
#' @param rt Vector of return period to estimate. Default is \code{rt = c(2,5,10,20,50,100}
#' 
#' @param newdata Covariates. If \code{NULL} (the default), the input data that served to fit 
#'   the model is used.
#'   
#' @param reliability Logical. Should the flood quantile associated with 
#'   reliability level be returned? In that case, \code{newdata} must represent
#'   the period of interest. See Durocher et al. (2019) for more details. Default is \code{FALSE}.
#'   
#' @param nsim Number of simulations. Default is \code{1000}.
#' 
#' @param verbose Logical. Should a progress bar be displayed? Default is \code{TRUE}.
#'
#' @seealso \link{FitNsPot}, \link{predict.nspot}.
#' @export
#' 
#' @example \dontrun{
#' # this example is explained in the vignette "At-site flood frequency analysis using peaks over threshold"
#' hat <- ch_rfa_boot_nspot(fit, x = flowStJohn, newdata = flowStJohn[yr,], nsim = 50, 
#' reliability = TRUE, verbose = FALSE)}
#'
ch_rfa_boot_nspot <- 
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
  trend_xmat <- model.matrix(attr(object$trend$data, 'term'), x)
  trend_all <- trend_xmat %*% object$trend$beta
    
  for(ii in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, ii /nsim)
    
    ## Create a bootstrap sample of exceedances
    sid <- which(runif(nr) < ny)
    yboot <- rgpa(length(sid), kap[1], kap[2]) * trend_all[sid]
    
   
    ##---- Fit the trend of the boostrap sample ----##
    
    if(object$method %in% c('reg-mle','reg-lmom', 'reg-mom')){
      
      trend_fit <- .FitNsPotGlm(trend_xmat[sid,], 
                              yboot, 
                              object$trend$beta, 
                              object$trend$link)
    
      copy$trend$beta <- paras[ii,-1] <- coef(trend_fit)
      trend_fitted <- fitted(trend_fit)
  
      if(object$method == 'reg-mle'){
        copy$kappa <- paras[ii,1] <- .FitNsPotKappaMle(yboot / trend_fitted)
    
      } else if(object$method == 'reg-lmom'){
        copy$kappa <- paras[ii,1] <- fgpaLmom(yboot / trend_fitted)[2]
      
      } else if(object$method == 'reg-mom'){
        copy$kappa <- paras[ii,1] <- fgpaMom(yboot / trend_fitted)[2]
      }
    
    } else{
      
       sol <- .FitNsPotMle(x = trend_xmat, y = yboot, l = object$trend$link, 
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
  class(ans)<- c('bootns', 'ch_rfa_boot_nspot')
  
  return(ans)
}
