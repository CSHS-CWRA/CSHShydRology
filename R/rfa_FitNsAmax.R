##############################################################################
#' Nonstationary model for annual maxima fitted by least-squares and L-moments
#' 
#' Return the estimation of a nonstationary model for the distribution of annual
#' maximums with a trend in the mean.   
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param form Formula of the trend. 
#' @param x Data.
#' @param distr Distribution
#' @param type Type of model. Either additive (\code{"add"}) 
#'   or multiplicative (\code{"mult"}).
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
#' fit <- FitNsAmax(flow~date, amax, 'gev', type = 'mult')
#' 
#' print(fit)
#' plot(fitted(fit), residuals(fit))
#' 
#' plot(flow~date, amax)
#' lines(amax$date, fitted(fit))
#' 
#' fit.ln3 <- FitNsAmax(flow~date, amax, 'ln3', type = 'mult')
#' 
#' AIC(fit)
#' AIC(fit.ln3)
#' 
#' FitNsAmax(flow~date, amax, c('gev','ln3'), type = 'mult')
#' 
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
  xd <- get_all_vars(form, x)
  resp <- eval(form[[2]], env = xd)
  xmat <- model.matrix(form, xd)
  
  ## Fit a linear model and transform the residuals depending of an
  ## additive or multiplicative model.
  if(type == 'mult'){
    trend <- lsfit(xmat, log(resp), intercept = FALSE)
    trend.fit <- exp(xmat %*% trend$coef)
    z <- as.numeric(resp/trend.fit)
    
  } else if(type == 'add'){
    trend <- lsfit(xmat, resp, intercept = FALSE)
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




#' @export
print.nsamax <- function(obj){
  cat('\nNonstationary model for annual maximums\n')
  cat('\nMethod: LS+LMOM')
  cat('\nDistribution:',obj$para$type)
  cat('\nType:', obj$type )
  cat('\nParameters\n')
  print(obj$para$para, digits = 4)
  cat('\nTrend:\n')
  print(obj$beta, digit = 4)
}

#' @importFrom lmomco plmomco
#' @export
residuals.nsamax <- function(object, scale = FALSE){
  
  resp <- eval(object$formula[[2]],object$data)
  
 if(object$type == 'mult'){
    ans <- as.numeric(resp / object$fitted)
    
  } else if(object$type == 'add'){
    ans <- as.numeric(resp - object$fitted)
  }   
  
  if(scale)
    ans <- qnorm(plmomco(ans, object$para))
  
  return(ans)
}

#' @export
fitted.nsamax <- function(object, newdata = NULL){
  
  if(is.null(newdata))
    return(object$fitted)
  
  xmat <- model.matrix(object$formula, model.frame(object$formula, newdata))
  ans <- as.numeric(xmat %*% object$beta)
  
  if(object$type == 'mult')
    ans <- exp(ans)
  
  return(ans)
}

#' @importFrom lmomco dlmomco
#' @export
logLik.nsamax <- function(object)
  sum(log(dlmomco(residuals(object), object$para)))  

#' @export
#' @rdname FitNsAmax
AIC.nsamax <- function(object, k = 2){
  np <- length(coef(object))-1
  return(k * np - 2*logLik(object)) 
}
  
#' @export
coef.nsamax <- function(object) c(object$beta, object$para$para)

#' @importFrom lmomco rlmomco qlmomco
#' @export
#' @rdname FitNsAmax
simulate.nsamax <- function(object, u = NULL){

  ## Simulate a bootstrap sample
  
  if(is.null(u)){
    sim <- rlmomco(length(object$fitted),  object$para)
  
  } else{
    sim <- qlmomco(u, object$para)
  }
    
  if(object$type == 'mult'){
  ans <- sim * object$fitted
  
  } else if(object$type == 'add'){
    ans <- sim + object$fitted
  }
  
  return(ans)
}

#' @export
plot.nsamax <- function(x, ...){
  plot(fitted.values(x), residuals(x), ...)
  
  if(x$type == 'add'){
    abline(h=0)
  } else if(x$type == 'mult'){
    abline(h=1)
  }
  
}

#' @export
is.nsamax <- function(object) any(class(object) == 'nsamax')
