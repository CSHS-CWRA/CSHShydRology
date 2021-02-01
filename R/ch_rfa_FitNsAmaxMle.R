##############################################################################
#' Nonstationary model for annual maxima fitted by maximum likelihood
#' 
#' Return the fitting of a nonstationary model for annual
#' maxima fitted by maximum likelihood. A trend is set for the location parameter.
#' The scale parameter can be constant or proportional to the location parameter.  
#' 
#' @param form Formula describing the trend in the location parameter.
#'
#' @param x Data.
#' 
#' @param distr Fitted distribution. Must be one of 
#'   \code{('gev','glo','gno','pe3')}.
#' 
#' @param type Type of model defining the relation between the location
#'   and the scale parameter. Must be one of \code{('add','mult')}.
#'   
#' @param varcov Logical. Should the covariance matrix be returned. 
#' 
#' @param para0 Starting parameter estimates.
#' 
#' @param method,control,... Argument pass to \link{optim} 
#' 
#' @param quiet Logical. Should the message and warnings be displayed.
#'
#' @return
#' 
#' \item{formula}{Formula for the trend in the location parameter.}
#' \item{data}{Input data.}
#' \item{distr}{Distribution.}
#' \item{type}{Type of model.}
#' \item{para}{Model parameters. The last two parameters characterize the
#'   scale and shape parameters.}
#' \item{varcov}{Covariance matrix.}
#' \item{convergence,message}{Output form \link{optim} describing the 
#'   convergence of the algorithm.}
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
#' fit <- FitNsAmaxMle(flow~year, amax, distr = 'gev', type = 'add')
#' print(fit)
#' 
#' ## Function to extract the parameter for every year.
#' cc <- coef(fit)
#' 
#' ## Covariance matrix
#' vcov(fit)
#' 
#' ## Graphics of the location parameter +/- the scale parameters
#' plot(flow~year, amax)
#' lines(cc[,1], col = 'red')
#' lines(cc[,1] + cc[,2], col = 'red', lty = 2)
#' lines(cc[,1] - cc[,2], col = 'red', lty = 2)
#' 
#' ## Selecting a model based on AIC
#' fit2 <- FitNsAmaxMle(flow~year, amax, distr = 'gno', type = 'mult')
#' 
#' AIC(fit)
#' AIC(fit2)
#' 
#' ## Simulate the model
#' simulate(fit2)
#' 
FitNsAmaxMle <- 
  function(form, 
           x, 
           distr, 
           type = 'add', 
           varcov = TRUE,
           method = 'BFGS',
           control = NULL,
           para0 = NULL, 
           quiet = FALSE,
           ...){
  
  ## Extract the response variable and design matrix
  xd <- model.frame(form, x)
  resp <- model.response(xd)
  xmat <- model.matrix(attr(xd,'term'), xd)
  np <- ncol(xmat)
  
  ## Extract the pdf function
  if(distr %in% c('gev','glo','gno','pe3')){
    Fpdf <- getFromNamespace(paste0('d',distr), 'CSHShydRology')
  } else {
    stop('The selected distribution is not supported')
  }
    
  ## Function that evaluate the negative log-likelihood 
  nllik <- function(p, transf = TRUE){
    loc <- as.numeric(xmat %*% p[1:np])
    shp <- p[np+2] 
    
    if(transf)
      scl <- exp(p[np+1])
    else
      scl <- p[np+1]
    
    if(type == 'mult')
      scl <- scl * abs(loc)
    
    -sum(Fpdf(resp, loc, scl, shp, log = TRUE))
  }
  
  ## initial values if not provided
  if(is.null(para0)){
    beta0 <- lsfit(xmat, resp, intercept = F)$coefficients
    
    if(type == 'add'){
      alf0 <- sd(resp - xmat %*% beta0)
      
    } else if (type == 'mult'){
      alf0 <- sd(resp / (xmat %*% beta0))
    }
    
    para0 <- c(beta0, log(alf0),-.1)
      
  } else {
    para0[np+1] <- log(para0[np+1])
  }
  
  ## Estimation of the model
  sol <- optim(para0, nllik, method = method, control = control)
  sol$par[np+1] <- exp(sol$par[np+1])
  
  ## Name the scale and shape parameters
  if(distr == 'pe3'){
    names(sol$par)[np + 1:2] <- c('sig', 'g')
  } else {
    names(sol$par)[np + 1:2] <- c('alf', 'kap')
  }
  
  ## Warn from possible problem of convergence
  if(!quiet & sol$convergence != 0)
    warning('Solution have not converged')
  
  ## Should the variance-covariance be returned
  if(varcov){
    
    ## Evaluate the covariance matrix from the hessian
    h <- try(optimHess(sol$par, nllik, transf = FALSE, control = control))
    h <- try(as.matrix(Matrix::nearPD(h)$mat))
    varcov <- try(chol2inv(chol(h)))
  
    if(class(varcov) == 'try-error'){
      varcov <- NA
    } else {
      colnames(varcov) <- rownames(varcov) <- names(sol$par)
    }
  }
  
  ans <- list(formula = form,
              data = xd,
              distr = distr,
              type = type,
              para = sol$par,
              varcov = varcov,
              logLik = -sol$value,
              convergence = sol$convergence,
              message = sol$message)
  
  class(ans) <- "nsamax.mle"
  
  return(ans)
}
