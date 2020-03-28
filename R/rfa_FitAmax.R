###############################################################################
#' At-site frequency analysis using annual maximums
#'
#' Return a fitting of a distribution, normally representing annual maximums.
#' Both maximum likelihood and L-moments estimation methods are available.
#' If the maximum likelihood is used and fails, the L-moments solution
#' will be returned with a warning message is issued.
#' When more than one distribution is passed. The
#' best distribution is selected automatically according to the AIC criteria.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @param x Data.
#'
#' @param distr Distribution to fit. See \code{\link{Amax}} for the list
#'   of available distribution.
#'
#' @param method Estimation method.
#'   Either maximum likelihood ('mle') or L-moments ('lmom').
#'
#' @param varcov Should the variance-covariance matrix of the parameters
#'    be computed. For \code{mle} the covariance matrix is derived from the
#'    hessian matrix. For L-moments, non-parametric bootstrap is used.
#'
#' @param nsim Number of simulations used to evaluate the covariance matrix
#'   when using L-moment estimator.
#'   
#' @param k Penality terms for the AIC.
#'   
#' @param tol.gev Accepted difference between the AIC of the GEV and the best 
#'   best distribution. If the difference is inferior to \code{tol.gev}, the 
#'   GEV distribution is prefered.
#'   
#' @param ... Other parameters.
#'
#' @return
#'
#' \item{data}{Data Values.}
#' \item{lmom}{L-moments.}
#' \item{para}{Parameter estimates.}
#' \item{varcov}{Covariance matrix of the parameter}
#' \item{llik}{Value of the log-likelihood}
#'
#'
#' @section References:
#'
#' Coles, S. (2001). An introduction to statistical
#'   modeling of extreme values. Springer Verlag.
#'
#' Hosking, J. R. M., & Wallis, J. R. (1997). Regional frequency analysis:
#'   an approach based on L-moments. Cambridge Univ Pr.
#'
#' @seealso \link{predict.amax}, \link{GofTest}, \link{plot.amax}.
#'
#' @importFrom lmomco lmoms
#' @export
#'
#' @examples
#'
#' ## Extract a time series of annual maxima
#' x <- ExtractAmax(flow~date, flowStJohn)$flow
#'
#' ## Fitting of GEV distribution using L-moments
#'
#' fit <- FitAmax(x,'gev')
#' print(fit)
#' coef(fit)
#' AIC(fit)
#' fit$lmom
#'
#' ## The evaluation of the variance-covariance matrix can be turn down
#' fit <- FitAmax(x,'gev', varcov = FALSE)
#'
#' ## Using Maximum likelihood
#' fit <- FitAmax(x,'gev', method ='mle')
#' print(fit)
#' vcov(fit)
#'
#' ## Standard deviation of the parameter
#' sqrt(diag(vcov(fit)))
#'
#' ## Chose the best distribution according to AIC
#' FitAmax(x, distr = c('gev','glo','gno','pe3'), method = 'mle')
#'
FitAmax <-
  function(x, 
           distr = c('gev','gno', 'pe3', 'glo'),
           method = 'lmom',
           varcov = TRUE,
           nsim = 500,
           k = 2,
           ...,
           tol.gev = 0){

  ## compute lmoments once
  lmm <- lmoms(x)
  
  #################################################
  ## Verify that all values are finite values
  if(!all(is.finite(x)))
    stop('There is one (or more) non finite values')

  if(varcov & method == 'lmom' & nsim < 2)
    stop('There is not enough simulations (nsim) to perform boostrap')
  #################################################

  ## If there is only one distribution passed
  if(length(distr) == 1)
    return(.FitAmax(x, distr = distr, lmm = lmm, 
                    method = method, varcov = varcov, nsim = nsim, ...))

  ## Fit data and compute the AIC for each distribution
  if(method == 'lmom')
    varcov.search <- FALSE
  else
    varcov.search <- varcov
  
  Fun <- function(z) try(.FitAmax(x, distr = z, lmm = lmm, 
                                  method = method, varcov = varcov.search, 
                                  nsim = nsim, ...))
  fits <- lapply(distr, Fun)
  crit <- lapply(fits, function(z) try(AIC(z)))

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

  ans <- fits[[which.min(crit)]]
  
  ## Compute the varcov if needed 
  if(method == 'lmom' & varcov)
    ans$varcov <- .VcovLmom(ans$para, ans$distr, length(x), nsim)
  
  ## return
  return(ans)
}

## see FitAmax for more info
## lmm is an output of lmomco::lmom that can be passed to speed up
## multiple evaluations
#' @importFrom lmomco lmom2par mle2par dlmomco rlmomco
.FitAmax <-
  function(x, distr = 'gev', method = 'lmom',
           varcov = TRUE, lmm = NULL, nsim = 1000){

  ## Compute the L-moments and associated parameters
  if(is.null(lmm))
    lmm <- lmoms(x)

  f <- lmom2par(lmm,distr)

  if(method == 'mle'){

    ## Fit the distribution by maximum likelihood
    suppressWarnings(
      para <- try(mle2par(x, distr, para.init = f, silent = TRUE,
                                  hessian = varcov)))

    ## If mle return an error, use the L-moment estimate
    if(class(para) == 'try-error'){
      para <- f
      varcov <- NA
      method = 'lmom'

      ## compute the log-likelihood
      llik <- sum(log(dlmomco(x,para)))

      warning('Maximum likelihood has fail')

    } else {

      ## Verify and warn the user if optim does not converge
      if(para$optim$convergence != 0)
        warning('Maximum likelihood may have not coverge')

      ## (If necessary)
      ## Compute the covariance matrix by inversing the hessian matrix
      if(varcov)
        varcov <- chol2inv(chol(para$optim$hessian))
      else
        varcov <- NA

      ## compute the log-likelihood value
      llik <- -para$optim$value

      ## Remove the optim element from the list for a cleaner output
      para <- para[1:3]
    }

  } else if(method == 'lmom'){
    para <- f

    ## Compute the covariance matrix by bootstrap if required
    if(varcov){
      varcov <- .VcovLmom(f$para, distr, length(x), nsim)
    } else {
      varcov <- NA
    }

    ## compute the log-likelihood
    llik <- sum(log(dlmomco(x,para)))
  }

  if(!all(is.na(varcov)))
    colnames(varcov) <- rownames(varcov) <- names(para$para)

  ## Return an amax object
  ans <- list(lmom = lmm$lambdas,
              method = method,
              para = para$para,
              distr = distr,
              varcov = varcov,
              llik = llik,
              data = x)

  class(ans) <- 'amax'

  return(ans)
}

.VcovLmom <- function(para, distr, n, nsim){
  ## Bootstrap estimate
  xboot <- replicate(nsim, rAmax(n, para, distr))
  pboot <- apply(xboot, 2, fAmax, distr)
  cc <- cov(t(pboot))
  
  ## Correct it to obtain a positive-definite matrix
  ans <- Matrix::nearPD(cc)
  
  return(as.matrix(ans$mat))
}
