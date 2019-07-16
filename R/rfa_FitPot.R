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
#' @param se Should the standard error or the confidence interval be returned.
#'
#' @param ci For \code{'coef'} should confidence interval be returned. 
#'   For \code{'predict'}, method used to compute confidence intervals. 
#'   Either \code{'profile'}, \code{'delta'} or \code{'boot'}.
#'
#' @param nsim Number of bootstrap samples.
#' 
#' @param unit Length of cycle. Data are normaly years and \code{unit} = 365.25.
#'   Can be change to 12 or 52 for daily and weekly data.
#'
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
FitPot.data.frame <- function(obj,  ...)
  FitPot.numeric(x = as.numeric(obj[,2]), dt = as.numeric(obj[,1]), ...)

#' @export
#' @rdname FitPot
FitPot.matrix <- function(obj,  ...)
  FitPot.numeric(x = as.numeric(obj[,2]), dt = as.numeric(obj[,1]), ...)

#' @export
#' @rdname FitPot
FitPot.formula <- function(form, x, ...){
  xd <- model.frame(form,as.data.frame(x))
  return(FitPot.numeric(x = xd[,1], dt = xd[,2], ...))
}

#' @export
#' @rdname FitPot
FitPot.numeric <- function(x, dt = NULL, u = 0, method = 'mle',
                    declust = 'none', r = 1, rlow = .75, nsim = 1000,
                    varcov = TRUE, unit = 365.25){

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

  ## Estimatate the parameters
  if(method == 'lmom'){

    ## Estimatate the parameters
    ans$estimate <- fgpaLmom(ans$excess)

    ## Covariance matrix estimated by boostraps
    if(varcov){
      ans$varcov <- matrix(NA,2,2)

      xboot <- replicate(nsim,
                         sample(ans$excess, length(ans$excess), replace = TRUE))
      pboot <- t(apply(xboot,2,fgpaLmom))

      ## Correct
      vc <- Matrix::nearPD(cov(pboot))$mat
      ans$varcov <- as.matrix(vc)
    }

  } else if(method == 'mle'){
    sol <- fgpa1d(ans$excess, sol = TRUE)
    ans$estimate <- sol$par

    if(varcov){
      ans$varcov <- sol$varcov
      colnames(ans$varcov) <- rownames(ans$varcov) <- names(ans$estimate)
    }

  } else if(method == 'mle2'){
    sol <- fgpa2d(ans$excess, sol = TRUE)

    ans$estimate <- sol$par

    if(varcov){
      ans$varcov <- sol$varcov
      colnames(ans$varcov) <- rownames(ans$varcov) <- names(ans$estimate)
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
coef.fpot <- function(obj, rate = FALSE, ci = FALSE, alpha = 0.05){

  if(rate)
    ans <- c(obj$nexcess/obj$ntot,obj$estimate)
  else
    ans <- obj$estimate

  if(ci){

    ## Full likelihood
    llikFull <- logLik(obj)

    ## Initiate searching intervals for parameters
    ase <- sqrt(obj$varcov[1,1])

    abnd0 <- c(pmax(1e-8, obj$estimate[1] - 10 * ase),
               obj$estimate[1] + 10* ase)

    kbnd0 <- c(-1,2)


    ## Bound for the deviance
    khi <- qchisq(1-alpha,1)

    ## Deviance for the profile likelihood of the scale parameter
    Dscl <- function(z){

      nllik <- function(p) -sum(dgpa(obj$excess, z, p, log = TRUE))

      ## Compute the profile likelihood
      llik0 <- -goptimize(nllik, kbnd0)$objective

      return(2*(llikFull-llik0))

    }

    ## Deviance for the profile likelihood of the shape parameter
    Dshp <- function(z){

      nllik <- function(p) -sum(dgpa(obj$excess, p, z, log = TRUE))

      llik0 <- -optimize(nllik, abnd0)$objective

      return(2*(llikFull-llik0))
    }

    ## Find the boundary

    suppressWarnings(lbScl <-
        goptimize(function(z) abs(Dscl(z)-khi),
                 c(abnd0[1],obj$estimate[1]))$minimum)

    suppressWarnings(ubScl <-
                       goptimize(function(z) abs(Dscl(z)-khi),
                                c(obj$estimate[1],abnd0[2]))$minimum)

    suppressWarnings(lbShp <-
                       goptimize(function(z) abs(Dshp(z)-khi),
                                c(kbnd0[1],obj$estimate[2]))$minimum)

    suppressWarnings(ubShp <-
                       goptimize(function(z) abs(Dshp(z)-khi),
                                c(obj$estimate[2],kbnd0[2]))$minimum)

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
vcov.fpot <- function(obj, rate = FALSE){

  if(rate){
    ans <- matrix(0,3,3)

    kn <- obj$nexcess/obj$ntot

    ans[1,1] <- kn*(1-kn)/obj$ntot
    ans[2:3,2:3] <- obj$varcov

  } else
    ans <- obj$varcov

  return(ans)
}


#' @export
#' @rdname FitPot
print.fpot <- function(obj){

  cat('\nAnalysis of peaks over threshold')

  cat('\n\nThreshold : ', obj$u)
  cat('\nNumber of excess : ', obj$nexcess)

  if(!is.na(obj$nyear)){
    cat('\nNumber of years : ', format(round(obj$nyear,2)))
    cat('\nExcess rate (yearly): ', format(round(obj$nexcess/obj$nyear,4)))

  } else cat('\nExcess rate: ', format(round(obj$nexcess/obj$ntot,4)))

  cat('\nMean residual Life : ', format(obj$mrl, digits = 6))

  cat('\n\nParameters :\n')
  print(obj$estimate, digit = 4)

  if(!is.null(obj$varcov)){
    cat('\nstd.err :\n')
    print(sqrt(diag(obj$varcov)), digit = 4)
  }
}


#' @export
#' @rdname FitPot
logLik.fpot <- function(obj)
  sum(dgpa(obj$excess, obj$estimate[1], obj$estimate[2], log = TRUE))

#' @export
#' @rdname FitPot
AIC.fpot <- function(obj, k = 2)
  -2 * logLik(obj) + 2 * k


