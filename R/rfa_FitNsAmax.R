##############################################################################
#' Nonstationary model for annual maxima fitted by least-squares and L-moments
#' 
#' Return the fitting of a nonstationary model for annual
#' maxima that fit first a trend using regression and then estimates the
#' residual distribution with L-moments. 
#' If a list of distribution is provided, the best distribution is selected by AIC.    
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param form Formula of the trend. 
#' 
#' @param x Data including covariates.
#' 
#' @param distr Distribution.
#' 
#' @param type Type of model. Either additive (\code{"add"}) 
#'   or multiplicative (\code{"mult"}).
#'   
#' @param k The penalty per parameter to be used in AIC. See \link{AIC}.
#' 
#' @param tol.gev Accepted difference between the AIC of the GEV and the best 
#'   best distribution. If the difference is inferior to \code{tol.gev}, the 
#'   GEV distribution is prefered.
#' 
#' @param ... Other parameters.
#' 
#' @return
#'
#' \item{formula}{Formula of the trend.}
#' \item{data}{Input data.}
#' \item{beta}{Parameters of the trend.}
#' \item{fitted}{Fitted value of the trend.}
#' \item{type}{Type of model.}
#' \item{para}{Parameters of the distribution. See \link{lmom2par}}
#' 
#' 
#' @seealso \link{FitAmax}, \link{predict.nsamax}.
#' 
#' @importFrom lmomco lmoms lmom2par pwm pwm2lmom
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
#' fit <- FitNsAmax(flow~year, amax, 'gev', type = 'mult')
#' 
#' ## Examination of the residuals
#' print(fit)
#' plot(fitted(fit), residuals(fit))
#' 
#' ## Visualization of the trend
#' plot(flow~year, amax)
#' lines(amax$year, fitted(fit), col = 'red')
#' 
#' ## Using alternative distribution and descriptors
#' fit1 <- FitNsAmax(flow~year, amax, 'ln3', type = 'mult')
#' fit2 <- FitNsAmax(flow~poly(year,2), amax, 'ln3', type = 'mult')
#' 
#' ## Evaluation of the AIC for model selection
#' AIC(fit1)
#' AIC(fit2)
#' 
#' ## Automatic selection of the distribution
#' fit <- FitNsAmax(flow~year, amax, c('gev','ln3'), type = 'mult')
#' print(fit)
#' 
#' ## Simulate the model
#' simulate(fit)
#' 
FitNsAmax <- 
  function(form, 
           x, 
           distr = 'gev',
           type = 'add',
           k = 2,
           tol.gev = 0){
  
  ## Fit a the model with the given distribution
  if(length(distr) == 1)
    return(.FitNsAmax(form, x, distr, type))
  
  ## Or Loop true all the distribution
  Fun <- function(d) try(.FitNsAmax(form, x, d, type))
  
  fits <- lapply(distr, Fun)
  crit <- lapply(fits, function(z) try(AIC(z, k = k)))
  
  ## If the fitting fails put AIC to Infinity
  crit.err <- sapply(crit, function(z) class(z) != 'numeric')
  
  if(any(crit.err))
    crit[[crit.err]] <- Inf
  
  crit <- unlist(crit)
  
  ## It is assumed for small difference in AIC, the fitting is similar
  ## Therefore GEV could selected as default due to its theoritical
  ## justification. The tolerate difference is tol.gev
  gid <- which(distr == 'gev')
  crit[gid] <- crit[gid] - tol.gev
  
  return(fits[[which.min(crit)]])
}

## Function that perform fitting of given distribution
.FitNsAmax <- 
  function(form, 
           x, 
           distr = 'gev',
           type = 'add'){
  
  ## extract the response variable and design matrix
  xd <- model.frame(form, x)
  resp <- model.response(xd)
  xmat <- model.matrix(attr(xd, 'terms'), xd)
  
  ## Fit a linear model and transform the residuals depending of an
  ## additive or multiplicative model.
  if(type == 'mult'){
    trend <- lm.fit(xmat, log(resp))
    trend.fit <- exp(xmat %*% trend$coef)
    z <- as.numeric(resp/trend.fit)
    
  } else if(type == 'add'){
    trend <- lm.fit(xmat, resp)
    trend.fit <- xmat %*% trend$coef
    z <- as.numeric(resp-trend.fit)
    
  } else{ 
    stop('The type must be "mult" or "add"')
    
  }
    
  ## Fit the distribution of the standardized observation
  para <- lmom2par(pwm2lmom(pwm(z)), distr)

  ## return a nsamax object
  ans <- list(formula = form,
              data = xd,
              beta = trend$coef,
              fitted = as.numeric(trend.fit),
              type = type,
              para = para)

  class(ans) <- 'nsamax'

  return(ans)
}
