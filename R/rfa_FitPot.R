######################################################################
#' Peak over threshold (POT)
#'
#' Fit the parameters of a thresholding model using Generalized Pareto
#' distribution (GPA). Include declustering techniques.
#'
#' @param x,form Dataset. If a matrix or data.frame is passed as argument the
#'   first column must be the time and the second the values. A formula can be
#'   used to specify which variables of a data.frame to use. In this case, it
#'   must have the form \code{value~time}.
#'
#' @param dt Date or time of observation.
#'
#' @param u Threshold.
#'
#' @param method Estimation method. Either \code{'lmom'}, \code{'mle'} or
#' \code{'mle2'}.
#'
#' @param declust If necessary, declustering method.
#'   Either \code{'run'} or \code{'wrc'}.
#'
#' @param r Lag parameter for declustering. Either the running length between clusters
#'   or the minimum separating time between two flood Peaks.
#'   The scale must coincide with the observation date \code{dt}.
#'
#' @param rlow For WRC, recession level between two flood peaks in percentages.
#'
#' @param nsim Number of bootstrap samples.
#' 
#' @param varcov Logical. Should the covariance matrix of the estimated 
#'   parameters be evaluated. 
#' 
#' @param unit Length of cycle. Data are normaly years and \code{unit} = 365.25.
#'   Can be change to 12 or 52 for daily and weekly data.
#'
#' @param object Output of \code{FitPot}.   
#'   
#' @param ci For \code{'coef'} should confidence interval be returned.
#' 
#' @param alpha Probability outside the confidence interval.
#' 
#' @param rate Logical. Should the estimated probability of exeedance must be
#'   included in the covariance matrix. 
#'   
#' @param k The penalty per parameter to be used.
#'   
#' @param ... Other parameters.
#'
#' @details
#'
#' The access functions \code{coef} and \code{vcov} return respectively the
#' parameters and the variance-covariance matrix of the POT model. For the L-moment
#' method the covariance matrix is using bootstraps.
#' The access function \code{predict} evaluates flood quantiles.
#' If \code{dt} is a Date the return period is computed in years using the range
#' of observation.
#'
#'
#' @seealso \link{which.floodPeaks}, \link{which.clusters}, \link{PlotMrl}.
#'
#' @section References:
#'
#' Coles S. (2001) An introduction to statistical modeling of extreme values.
#'   Springer Verlag.
#'
#' Davison AC, Smith RL. (1990) Models for Exceedances over High Thresholds.
#'   Journal of the Royal Statistical Society Series B (Methodological).
#'   52(3), 393-442.
#'   
#' @export
#'
#' @examples
#'
#' xd <- rgpa(100, 1, -.2)
#' fit <- FitPot(xd, u = 0)
#'
#' print(fit)
#' vcov(fit)
#' predict(fit)
#' coef(fit, ci = TRUE)
#'
#' fit <- FitPot(flow~date, flowStJohn, u = 1000,
#'                declust = 'wrc', r = 14)
#'
#' print(fit)
#' plot(flow~date,flowStJohn, type = 'l')
#' points(fit$time,fit$excess+fit$u, col = 2, pch = 16)
#' abline(h=1000, col = 3, lwd = 2)
#'
#' predict(fit, se = TRUE, ci = 'delta')
#'

FitPot <- function(x, ...) UseMethod('FitPot',x)

#' @export
#' @rdname FitPot
FitPot.data.frame <- function(x,  ...)
  FitPot.numeric(x = as.numeric(x[,2]), dt = as.numeric(x[,1]), ...)

#' @export
#' @rdname FitPot
FitPot.matrix <- function(x,  ...)
  FitPot.numeric(x = as.numeric(x[,2]), dt = as.numeric(x[,1]), ...)

#' @export
#' @rdname FitPot
FitPot.formula <- function(form, x, ...){
  xd <- model.frame(form, as.data.frame(x))
  return(FitPot.numeric(x = xd[,1], dt = xd[,2], ...))
}

#' @export
#' @rdname FitPot
FitPot.numeric <- 
  function(x, 
           dt = NULL, 
           u = 0, 
           method = 'mle',
           declust = 'none', 
           r = 1, 
           rlow = .75, 
           nsim = 1000,
           varcov = TRUE, 
           unit = 365.25,
           ...){

  ## Supply a date variable if not presented
  if(is.null(dt))
    dt <- seq_along(x)

  nyear <- sum(is.finite(dt)) / unit

  ## If necessary, extract peaks using a declustering method
  if(declust == 'run')
    cid <- which.clusters(x, dt, u, r)
  else if(declust == 'wrc')
    cid <- which.floodPeaks(x, dt, u, r = r, rlow = rlow, ini = 'wrc')
  else
    cid <- x > u

  ans <- list(excess = x[cid] - u, time = dt[cid])

  ## select the fitting function
  if(method == 'lmom')
    f <- fgpaLmom
  else if(method == 'mom')
    f <- fgpaMom
  else if(method == 'mle')
    f <- fgpa1d
  else if(method == 'mle2')
    f <- fgpa2d
  
  ## Estimate the parameters
  if(method %in% c('lmom', 'mom')){

    ## Estimatate the parameters
    ans$estimate <- f(ans$excess)

    ## Covariance matrix estimated by boostraps
    if(varcov){
      ans$varcov <- matrix(NA,2,2)

      xboot <- replicate(nsim,
                         sample(ans$excess, length(ans$excess), replace = TRUE))
      pboot <- t(apply(xboot,2,f))

      ## Correct
      vc <- Matrix::nearPD(cov(pboot))$mat
      ans$varcov <- as.matrix(vc)
    }

  } else if(method %in% c('mle','mle2')){

    if(varcov){
      sol <- f(ans$excess, sol = TRUE)
      ans$estimate <- sol$par
      ans$varcov <- sol$varcov
      colnames(ans$varcov) <- rownames(ans$varcov) <- names(ans$estimate)
    
    } else{
      ans$estimate <- f(ans$excess, sol = FALSE)
    }
    
    if( (ans$estimate[2] < -.5 + 1e-4) | (ans$estimate[2] > 1-1e-4) )
      warning(paste('Shape parameter have reach boundary. Normal',
                    ' approximation of the likelihood may be unreliable'))
  }

  ## Complete object
  ans$u <- u
  ans$method <- method
  ans$mrl <-  as.numeric(ans$estimate[1]/ (1+ans$estimate[2]))
  ans$nyear <- nyear
  ans$unit <- unit
  ans$ntot <- length(x)
  ans$nexcess <- length(ans$excess)

  class(ans) <- 'fpot'
  return(ans)
}


#' @export
#' @rdname FitPot
coef.fpot <- 
  function(object, 
           rate = FALSE, 
           ci = FALSE, 
           alpha = 0.05, 
           ...){

  if(rate)
    ans <- c(object$nexcess/object$ntot,object$estimate)
  else
    ans <- object$estimate

  if(ci){

    ## Full likelihood
    llikFull <- logLik(object)

    ## Initiate searching intervals for parameters
    ase <- sqrt(object$varcov[1,1])

    abnd0 <- c(pmax(1e-8, object$estimate[1] - 10 * ase),
               object$estimate[1] + 10* ase)

    kbnd0 <- c(-1,2)


    ## Bound for the deviance
    khi <- qchisq(1-alpha,1)

    ## Deviance for the profile likelihood of the scale parameter
    Dscl <- function(z){

      nllik <- function(p) -sum(dgpa(object$excess, z, p, log = TRUE))

      ## Compute the profile likelihood
      suppressWarnings(llik0 <- -optimize(nllik, kbnd0)$objective)

      return(2*(llikFull-llik0))

    }

    ## Deviance for the profile likelihood of the shape parameter
    Dshp <- function(z){

      nllik <- function(p) -sum(dgpa(object$excess, p, z, log = TRUE))

      suppressWarnings(llik0 <- -optimize(nllik, abnd0)$objective)

      return(2*(llikFull-llik0))
    }

    ## Find the boundary

    suppressWarnings(lbScl <-
        optimize(function(z) abs(Dscl(z)-khi),
                 c(abnd0[1],object$estimate[1]))$minimum)

    suppressWarnings(ubScl <-
                       optimize(function(z) abs(Dscl(z)-khi),
                                c(object$estimate[1],abnd0[2]))$minimum)

    suppressWarnings(lbShp <-
                       optimize(function(z) abs(Dshp(z)-khi),
                                c(kbnd0[1],object$estimate[2]))$minimum)

    suppressWarnings(ubShp <-
                       optimize(function(z) abs(Dshp(z)-khi),
                                c(object$estimate[2],kbnd0[2]))$minimum)

    bnd <- data.frame(lower = c(NA,lbScl,lbShp),
                      upper = c(NA,ubScl,ubShp))

    rownames(bnd) <- c('rate','alpha','kappa')

    if(!rate) bnd <- bnd[-1,]

    ans <- data.frame(estimate = ans, bnd)

  }

  return(ans)
}

#' @export
#' @rdname FitPot
vcov.fpot <- function(object, rate = FALSE, ...){

  if(rate){
    ans <- matrix(0,3,3)

    kn <- object$nexcess/object$ntot

    ans[1,1] <- kn*(1-kn)/object$ntot
    ans[2:3,2:3] <- object$varcov

  } else
    ans <- object$varcov

  return(ans)
}


#' @export
#' @rdname FitPot
print.fpot <- function(x, ...){

  cat('\nAnalysis of peaks over threshold')

  cat('\n\nThreshold : ', x$u)
  cat('\nNumber of excess : ', x$nexcess)

  if(!is.na(x$nyear)){
    cat('\nNumber of years : ', format(round(x$nyear,2)))
    cat('\nExcess rate (yearly): ', format(round(x$nexcess/x$nyear,4)))

  } else cat('\nExcess rate: ', format(round(x$nexcess/x$ntot,4)))

  cat('\nMean residual Life : ', format(x$mrl, digits = 6))

  cat('\n\nParameters :\n')
  print(x$estimate, digit = 4)

  if(!is.null(x$varcov)){
    cat('\nstd.err :\n')
    print(sqrt(diag(x$varcov)), digit = 4)
  }
}


#' @export
#' @rdname FitPot
logLik.fpot <- function(object, ...)
  sum(dgpa(object$excess, object$estimate[1], object$estimate[2], log = TRUE))

#' @export
#' @rdname FitPot
AIC.fpot <- function(object, k = 2, ...)
  -2 * logLik(object) + 2 * k


