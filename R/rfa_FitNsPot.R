###############################################################################
#' Estimation of a nonstationary index-flood model for peaks over threshold for
#' a single site.
#' 
#' Estimate the parameter of a nonstationary index-flood model by a stepwise
#' method. First quantile regression is used to estimate 
#' a time-varying threshold. Second, a trend in the exceedances is determined
#' using quasi-likelihood and finally, a L-moments are used for estimating 
#' the shape parameter of the a Generalized Pareto Distribution.
#' 
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param form Formula describing the date and the response variable. 
#'   Must be of the form \code{y ~ date}.  
#' 
#' @param x Data.
#' 
#' @param tau Percentage of daily data above the threshold. 
#' 
#' @param thresh Formula determining the threshold model. 
#'   Must be of the form \code{~ x1 + x2 + ...}.
#'   
#' @param trend  Formula determining the trend of the mean excess. 
#'   Same as \code{thresh}.
#' 
#' @param trend.link Link function characterizing the mean excess.
#' 
#' @param trend.start If necessary, starting values for the estimation of 
#'   the mean excess.
#' 
#' @param declust Type of declustering techniques used to extract peaks over 
#'   threshold. See \link{which.floodpeaks}
#' 
#' @param r,rlow Parameter of the declustering technique. 
#' 
#' @seealso \link{FitPot}, \link{SearchThresh}.
#'
#' @export
#'
#' @examples
#' 
#' fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95,
#'                 trend = ~ date, thresh = ~ date, declust = 'wrc', r = 14)
#' 
#' print(fit)
#' plot(fit)
#' 
#' GofTest(fit)
#' 
FitNsPot <- function(
  form,
  x,
  tau,
  method = 'reg-lmom',
  thresh = ~ 1, 
  trend = ~ 1,
  trend.link = 'identity',
  trend.start = NULL,
  trend.method = 'BFGS',
  trend.control = list(),
  declust = 'none',
  r = 1,
  rlow = 0.75,
  unit = 365.25,
  sorted = FALSE,
  start = NULL,
  quiet = FALSE,
  ...){
  
  ## extract model element from formulas
  x <- as.data.frame(x)
  dd <- eval(form[[3]], envir = x)
  
  ## If necessary make sure that the data are sorted according with
  ## the time variable
  if(!sorted)
    x <- x[order(dd),]
  
  nyear <- sum(is.finite(dd)) / unit
  
  y <- eval(form[[2]], envir = x)
  thresh.xmat <- model.matrix(thresh, x)
  
  ## Fit threshold using quantile regression and return corrected data
  thresh.fit <- quantreg::rq.fit(thresh.xmat, y, tau)
  thresh.beta <- coef(thresh.fit)
  thresh.fitted <- fitted(thresh.fit)
  thresh.avg <- mean(thresh.fitted)
  thresh.y <- y - thresh.fitted
  
  ## Extract peak information
  if(declust == 'none'){
    peak.id <- which(thresh.y > 0)
  } else if(declust == 'wrc'){
    peak.id <- which.floodPeaks(thresh.y+thresh.avg, dd, 
                                u = thresh.avg, r = r , rlow = rlow)
  } else if(declust == 'run'){
    peak.id <- which.floodPeaks(thresh.y, dd, u = 0, r = r)
  } else{
    stop('Must select a valid declustering technique.')
  }
      
  peak <- y[peak.id]
  trend.y <- thresh.y[peak.id]
  trend.xmat <- model.matrix(trend, x[peak.id,])
  
  ## Exceedance rate
  ppy <- length(peak)/ nyear
  
  if(method %in% c('reg-lmom', 'reg-mle')){
    
    ## Fit a mean excess model
    trend.fit <- .FitNsPotGlm(trend.xmat, trend.y, trend.start, trend.link) 
     
    trend.beta <- coef(trend.fit)
    trend.fitted <- fitted(trend.fit)
  
    ## Fit the kappa parameter
    if(method == 'reg-mle'){
      kappa <- .FitNsPotKappaMle(trend.y / trend.fitted)
    } else{
      kappa <- .FitNsPotKappa(trend.y / trend.fitted)
    }  
      
  
  } else if(method == 'mle'){
    
    sol <- .FitNsPotMle(x = trend.xmat, y = trend.y, l = trend.link, 
                        s = trend.start, trend.method, trend.control)
    
    kappa <- sol$kappa
    trend.beta <- sol$beta
    trend.fitted <- sol$fitted
    
    ## If required, provide feedback on solution
    if(!quiet){
      if(sol$convergence != 0)
        warning('The solution may not have converged')
      
      if(!is.null(sol$message))
        warning(sol$message)
    }
  
  } else{
    stop('Must pass a valid estimation method') 
    
  }
  
  
  ## Return object
  allvars <- unique(c(all.vars(form),all.vars(thresh),all.vars(trend)))
  
  ans <- list(data = x[,allvars],
              peak = peak.id,
              nyear = nyear,
              ppy = ppy,
              unit = unit,
              formula = form,
              method = method,
              threshold = thresh,
              threshold.beta = thresh.beta,
              trend = trend,
              trend.beta = trend.beta,
              trend.link = trend.link,
              trend.method = trend.method,
              trend.control = trend.control,
              kappa = kappa)
  
  class(ans) <- 'nspot'
    
  return(ans)
}

########################################################
## Function to compute the kappa parameter assuming that
## the mean is known and == 1. Use the probability weigthed
## moment.
########################################################
.FitNsPotKappa <- function(x){
  beta <- lmomco::pwm(x, nmom = 2)$betas
  return(beta[1]/(2*beta[2]-beta[1]) - 2)
}

########################################################
## Function to estimate the mean excess by MLE 
########################################################

## same using mle
.FitNsPotKappaMle <- function(x){
  
  ## negative log-likelihood
  nllik <- function(k) -sum(dgpa(x, 1+k, k, log = TRUE))
  
  ## find optimum
  suppressWarnings(sol <- optimize(nllik, c(-.5,1)))
  return(sol$minimum)
}

########################################################
########################################################
.FitNsPotMle <- function(x, y, s = NULL, l = 'identity',
                         method, control){
  
  mlink <- make.link(l)
  
  ## Find a starting value.
  if(is.null(s)){
    ini <- lm.fit(x, mlink$linkfun(y))
    s <- c(-.1, coef(ini))
  
  } else{
    
    if(length(s) != ncol(x)+1)
      stop('Must provide a valid starting value')
    
  }
  
  ## negative log-likelihood
  nllik <- function(para){
    mu <- as.numeric(x %*% para[-1])
    alf <- mlink$linkinv(mu) * (1 + para[1])
    
    ans <- try(-sum(dgpa(y, alf, para[1], log = TRUE)), 
               silent = TRUE)
    
    if(class(ans) == 'try-error')
      ans <- Inf
      
    return(ans)
  }
  
  ans <- optim(s, nllik, method = method, control = control)
  
  ## Add the specific output
  ans$kappa <- ans$par[1]
  ans$beta <- ans$par[-1]
  
  mu <- as.numeric(x %*% ans$beta)
  ans$fitted <- mlink$linkinv(mu)
  
  return(ans)
}

########################################################
## Robust function to estimate the mean excess by GLM 
## If GLM fail LS is used instead
########################################################
.FitNsPotGlm <- function(x, y, s = NULL, l = 'identity'){
  
  ## Fit a mean excess model
  Fun <- function(z) 
    glm.fit(x, y, intercept = FALSE, start = z,
            family = quasi(link = l, variance = 'mu^2'))
  
  suppressWarnings(fit <- try(Fun(s), silent = TRUE))
  
  ## If GLM does not work with default or passed argument use the LS solution
  ## as starting point
  if(class(fit) == 'try-error'){
    mlink <- make.link(l)
    ini <- lm.fit(x, mlink$linkfun(y))

    suppressWarnings(fit <- try(Fun(coef(ini)), silent = TRUE))
  }
  
  if(class(fit) == 'try-error'){
    warning('GLM fitting did not completed correctly')
    fit <- ini
  }
  
  return(fit)
}

#' @export
coef.nspot <- function(object, type = 'all'){
  
  if(type == 'kappa')
    ans <- object$kappa + c(1,0)
  else if(type == 'all')
    ans <- c(object$kappa, object$threshold.beta, object$trend.beta)

  return(ans)
}

#' @export
fitted.nspot <- function(object, newdata = NULL){
  
  if(is.null(newdata))
    newdata <- object$data[object$peak,]
  
  ## Compute the threshold
  thresh.xmat <- model.matrix(object$threshold, newdata)
  thresh.y <- thresh.xmat %*% object$threshold.beta
  
  ## Compute the mean excess
  trend.xmat <- model.matrix(object$trend, newdata)
  trend.y <- trend.xmat %*% object$trend.beta
  
  mlink <- make.link(object$trend.link)
  trend.y <- mlink$linkinv(trend.y)
  
  ## Compute the variable
  y <- eval(object$formula[[2]], envir = newdata)
  dd <- eval(object$formula[[3]], envir = newdata)
  z <- (y - thresh.y)/trend.y
  
  return(data.frame(time = dd, 
                    original = y, 
                    threshold = thresh.y, 
                    trend = trend.y, 
                    scale = z))
}

#' @export
residuals.nspot <- function(object, type = 'thresh'){
  
  cc <- fitted(object, object$data[object$peak,])
  
  if(type == 'scale'){
    ans <- cc$scale
  
  } else if(type %in% c('thresh','threshold')){
    ans <- cc$original - cc$threshold
  
  } else {
    stop("The type of residuals must be : 'thresh' or 'scale'") 
  }
  
  return(ans)
}

#' @export
is.nspot <- function(object, ...) any(class(object) == 'nspot')


#' @export
plot.nspot <- function(x, ...){
  
  ## plot original data
  plot(x$formula, x$data, type = 'l', ...)
  
  ## plotted fitted trend and threshold model
  yhat <- fitted(x)
  points(original~time, yhat)
  lines(threshold~time, yhat, col = 'red', lwd = 2)
  lines(trend+threshold~time, yhat, col = 'blue', lwd = 2)
}

#' @export
print.nspot <- function(x, ...){
  cat('Nonstationary index-flood model\n')
  
  cat('\nNumber of excess:', format(length(x$peak)))
  cat('\nNumber of PPY:', format(length(x$peak)/x$nyear))
  
  cat('\n\nShape parameter:', round(x$kappa,3))
  
  cat('\n\nThreshold parameters:\n')
  print(x$threshold.beta, digit = 4)
  
  cat('\nTrend link:', x$trend.link)
  cat('\nTrend parameters:\n')
  print(x$trend.beta, digit = 4)
  
}