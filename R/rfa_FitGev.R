######################################################################
#' Fit Generalized Extreme Value (GEV) distribution using Generalized
#' maximum likelihood
#'
#' Fit a GEV distribution on annual maxima using the generalized
#' maximum likelihood method with Beta prior.
#' The output is of the class \code{amax}. See \link{FitAmax}.
#' Asymptotic result are computed like the maximum likelihood approach.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Data.
#' 
#' @param varcov Logical. Should the covariance matrix be returned.
#' 
#' @param mu,sig2 Mean and variance of the Beta prior.
#' 
#' @param method.optim Optimisation method used by \code{\link{optim}}
#' 
#' @param ... Other parameter pass to \code{\link{optim}}
#'
#' @section References:
#'
#' Martins, E.S., Stedinger, J.R., 2000. Generalized maximum-likelihood
#' generalized extreme-value quantile estimators for hydrologic data.
#' Water Resour. Res. 36, 737–744. https://doi.org/10.1029/1999WR900330
#'
#' @export
#'
#' @examples
#'
#' x <- ExtractAmax(flow~date, flowStJohn)$flow
#'
#' ## Using the default physiographic prior.
#' fit <- FitGev(x)
#' print(fit)
#' coef(fit)
#' vcov(fit)
#' predict(fit, ci = 'delta')
#'
#' ## A uniform prior on interlval -0.5 to 0.5 can be used to approximate
#' ## the maximum likelihood estimate.
#'
#' AIC(FitAmax(x, distr = 'gev', method ='mle'))
#' AIC(FitGev(x, mu = 0, sig2 = 1/12))
#'
#' ## A regional study can be performed by using an empirical prior
#' ## Here 20 sites with the same GEV distribution are simulated
#' ## without priors.
#' ## Then a regional estimate is obtained using an empirical prior
#'
#' xmat <- replicate(20, rAmax(20, c(100,3, -.1), 'gev') )
#' flist <- apply(xmat,2, FitAmax, distr = 'gev', varcov = FALSE)
#' pmat <- sapply(flist, getElement,'para')
#'
#' kap0 <- pmin(.5,pmax(-.5,pmat[3,]))
#'
#' FitGev(xmat[,1], mu = mean(kap0), sig2 = var(kap0))
#' FitAmax(xmat[,1], distr = 'gev', method = 'mle')
#'
FitGev <- function(x, varcov = TRUE, mu = -.1, sig2 = 0.015,
                   method.optim = 'BFGS',...){


   ## Verify that all values are finite values
  if(!all(is.finite(x)))
    stop('There is one (or more) non finite values')

  ## Compute the L-moments and associated parameters as starting value
  lmm <- lmomco::lmoms(x)
  startp <- lmomco::lmom2par(lmm,'gev')$para

  ## apply transformation to parameter to ensure positivness of scale parameter
  ## and modify the sign of the shape parameter to agree with the parametrization
  startp[2] <- log(startp[2])

  ## Find the parameter of the prior Beta distribution based on
  ## mean and variance passed in arguments
  mu <- mu + .5
  a <- mu * ( (mu * (1-mu) / sig2) - 1)
	b <- a * (1 - mu) / mu

	if(a <= 0 | b <=0)
	  stop('The parameters of the Beta distribution are not correct')

	## Function returning Negative log-likelihood of the model
  Nllik <- function(p, w = 1){
    -sum(dgev(x,p[1],exp(p[2]), p[3] ,log=TRUE)) -
      w * dbeta(p[3]+.5, a, b, log = TRUE)
  }

  ## optimize the Nllik to obtain the estimate
  out <- optim(startp, Nllik, hessian = varcov, method = method.optim, ...)

  ## Compute the covariance matrix if necessary by iversion of the hessian
  ## matrix
  if(varcov)
    varcov <- chol2inv(chol(out$hessian))
  else
    varcov <- NA

  ## Create the output object
  pout <- out$par
  pout[2] <- exp(pout[2])
  names(pout) <- c('xi','alpha','kappa')

  ans <- list(para = pout,
              distr = 'gev',
              llik  = -Nllik(out$par, w = 0),
              varcov = varcov,
              method = 'gml',
              lmom = lmm$lambdas,
              data = x,
              prior = c(mu - 0.5, sig2))

  class(ans) <- 'amax'

  return(ans)
}

## Copy from R-package evd with modification of the parametrization
dgev <- function (x, xi = 0, alpha = 1, kappa = 0, log = FALSE){

  ## Verify if the parameter are valid
  if (min(alpha) <= 0)
    stop("invalid alpha")

  if (length(kappa) != 1)
    stop("invalid -kappa")

  ## center and alpha
  x <- (x - xi)/alpha


  if (kappa == 0){

    ## Case of Gumbel distribution
    d <- log(1/alpha) - x - exp(-x)

  } else {

    nn <- length(x)
    xx <- 1 - kappa * x
    xxpos <- xx[xx > 0 | is.na(xx)]
    alpha <- rep(alpha, length.out = nn)[xx > 0 | is.na(xx)]
    d <- numeric(nn)
    d[xx > 0 | is.na(xx)] <- log(1/alpha) - xxpos^(1/kappa) -
      (1-1/kappa) * log(xxpos)
    d[xx <= 0 & !is.na(xx)] <- -Inf
  }

  if (!log)
    d <- exp(d)

  return(d)
}