#' Bootstrap sample of the nonstationary AMAX model
#' 
#' @description Calculates bootstrap samples of the parameters and flood quantiles for the 
#' nonstationary AMAX model.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param object Output from \link{FitNsAmax}.
#' 
#' @param p Exceeding probability associated to a quantile or reliability level. Default 
#' value is \code{c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99)}.
#'    
#' @param newdata Covariates. If \code{NULL} (the default) the input data that served to fit 
#'   the model is used.
#'   
#' @param reliability Logical. Should the flood quantile associated with 
#'   reliability level be returned. In that case, \code{newdata} must represent
#'   the period of interest. See Durocher et al. (2019) for more details. Default is \code{FALSE}
#' 
#' @param nsim Number of simulations. Default is \code{1000}.
#' 
#' @param alpha Confidence level. Default is \code{0.95}.
#' 
#' @param verbose Logical. Should a progress bar be displayed? Default is \code{TRUE}.
#'
#' 
#' @return 
#' 
#' \item{para}{Parameter of the detrended distribution.}
#' \item{beta}{Parameter of the trend.}
#' \item{qua}{flood quantiles.}
#' 
#' @seealso \link{FitNsAmax}, \link{predict.nsamax}.
#'
#' @export
#' 
#' @example \dontrun{
#' # this example is explained in the vignette "At-site flood frequency analysis using annual maxima"
#' hat <- ch_rfa_boot_nsamax(fit.lin, p = prob, reliability = TRUE, nsim = 500, verbose = FALSE)}
#'
ch_rfa_boot_nsamax <- 
  function(object,
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE,
           nsim = 1000, 
           alpha = 0.95, 
           verbose = TRUE){
  
  if(is.null(newdata))
    newdata <- object$data 
  
  paras <- matrix(0, nsim, length(object$para$para))
  betas <- matrix(0, nsim, length(object$beta))
  
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
    fit <- FitNsAmax(form = form0, x = xjj,
                     distr = object$para$type, type = object$type) 
    
    paras[jj,] <- fit$para$para
    betas[jj,] <- fit$beta
    
    if(reliability){
      quas[jj,] <- predict(fit, p, newdata0, TRUE)
    } else{
      quas[,,jj] <- predict(fit, p, newdata0, FALSE)
    }
  }    
  
  colnames(paras) <- names(object$para$para)
  colnames(betas) <- names(object$beta)
  colnames(quas) <- as.character(round(p,3))
  
  ans <- list(para = paras,
              beta = betas,
              qua = quas)
  
  class(ans) <- 'bootns'
  
  return(ans)

}

