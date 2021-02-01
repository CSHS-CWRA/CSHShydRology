###############################################################################
#' Estimation of a nonstationary POT model.
#' 
#' Return the estimate the parameter of a nonstationary POT model. 
#' First quantile regression is used to estimate 
#' a time-varying threshold. Second, a trend in the exceedances is determined
#' using quasi-likelihood and finally, a L-moments are used for estimating 
#' the shape parameter of the a Generalized Pareto Distribution.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param form Formula defining the response variable and date. 
#'   Must be of the form \code{y ~ date}.  
#' 
#' @param x Data.
#' 
#' @param tau Percentage of daily data above the threshold.
#' 
#' @param method Estimation method. See the detail section. 
#' 
#' @param thresh Formula determining the threshold model. 
#'   Must be of the form \code{~ x1 + x2 + ...}.
#'   
#' @param trend  Formula determining the trend of the mean excess. 
#'   Same as \code{thresh}.
#' 
#' @param trend.link Link function characterizing the mean excess.
#' 
#' @param trend.method,trend.control,trend.start Parameters pass to \link{optim}.
#'   In particular, \code{trend.start} is user provided starting values for the
#'   trend model when likelihood techniques are used.
#' 
#' @param declust,r,rlow Parameter for the declustering techniques. 
#'   See \link{which.floodPeaks}
#'   
#' @param unit Determine the period of reference for the risk. By default
#'   it uses 365.25 for daily data. 
#'   
#' @param quiet Logical. Should the warning message be removed.
#' 
#' @details
#' 
#' The \code{AIC} function of the nonstationary POT model assumed that the 
#' threshold and the peaks are known. It can be used only to select the best 
#' model for the mean excess (trend). 
#' 
#' @seealso \link{FitPot}, \link{SearchThresh}.
#'
#' @export
#'
#' @examples
#' 
#' fit <- FitNsPot(flow~date, x = flowStJohn, tau = .95,
#'                 trend = ~ poly(date,3), thresh = ~ date, declust = 'wrc', r = 14)
#' 
#' print(fit)
#' plot(fit)
#' 
#' ## Perform the Goodness-of-fit on the standardized data.
#' ## note that 
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
  quiet = FALSE){
  
  ## extract model element from formulas
  x <- na.omit(as.data.frame(x))
  dd <- model.frame(form, x)
  nyear <- nrow(dd) / unit
  
  ## If necessary make sure that the data are sorted in respect with
  ## the date variable
  if(is.unsorted(dd[,2])){
    sid <- order(dd[,2])
    x <- x[sid,]
    dd <- dd[sid,]
  }
  
  ## Extract response variable and design matrix for threshold model
  y <- model.response(dd)  
  thresh.xd <- model.frame(thresh, x)
  thresh.xmat <- model.matrix(attr(thresh.xd, 'term'), thresh.xd)
  
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
    peak.id <- which.floodPeaks(thresh.y + thresh.avg, dd[,2], 
                                u = thresh.avg, r = r , rlow = rlow)
  } else if(declust == 'run'){
    peak.id <- which.floodPeaks(thresh.y, dd[,2], u = 0, r = r)
  
  } else{
    stop('Must select a valid declustering technique.')
  }
      
  peak <- y[peak.id]
  trend.y <- thresh.y[peak.id]
  trend.xd <- model.frame(trend, x[peak.id, , drop = FALSE])
  trend.xmat <- model.matrix(attr(trend.xd,'term'), trend.xd)
  
  ## Exceedance rate
  ppy <- length(peak)/ nyear
  
  if(method %in% c('reg-lmom', 'reg-mle', 'reg-mom')){
    
    ## Fit a mean excess model
    trend.fit <- .FitNsPotGlm(trend.xmat, trend.y, trend.start, trend.link) 
     
    trend.beta <- coef(trend.fit)
    trend.fitted <- fitted(trend.fit)
  
    ## Fit the kappa parameter
    if(method == 'reg-mle'){
      kappa <- .FitNsPotKappaMle(trend.y / trend.fitted)
    
    } else if(method == 'reg-lmom'){
      kappa <- fgpaLmom(trend.y / trend.fitted)[2]
    
    } else if(method == 'reg-mmom'){
      kappa <- fgpaMom(trend.y / trend.fitted)[2]
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
  
  threshold <- list(data = thresh.xd, beta = thresh.beta)
  
  trend <- list(data = trend.xd, beta = trend.beta, link = trend.link, 
                method = trend.method, control = trend.control)
  
  ans <- list(data = dd,
              peak = peak.id,
              nyear = nyear,
              ppy = ppy,
              unit = unit,
              method = method,
              threshold = threshold,
              trend = trend,
              kappa = kappa)
  
  class(ans) <- 'nspot'
    
  return(ans)
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
print.nspot <- function(x, ...){
  cat('Nonstationary index-flood model\n')
  
  cat('\nNumber of excess:', format(length(x$peak)))
  cat('\nNumber of PPY:', format(length(x$peak)/x$nyear))
  
  cat('\n\nShape parameter:', round(x$kappa,3))
  
  cat('\n\nThreshold parameters:\n')
  print(x$threshold$beta, digit = 4)
  
  cat('\nTrend link:', x$trend$link)
  cat('\nTrend parameters:\n')
  print(x$trend$beta, digit = 4)
  
}

#' @export
logLik.nspot <- function(object, ...){
  z <- residuals(object,'scale')
  kap <- coef(object, 'kappa')
  return(sum(dgpa(z, kap[1], kap[2], log = TRUE)))
}

#' @export
AIC.nspot <- function(object, ..., k = 2){
 np <- length(coef(object, 'trend'))
 return(k * np - 2*logLik(object)) 
}
