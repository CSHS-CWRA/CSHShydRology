######################################################################
#' Predict return levels
#'
#' Return the flood quantile of annual maximum distribution and
#' Confident intervals are provided by bootstrap.
#'
#' @param object Output from  \code{\link{FitAmax}}
#'
#' @param p Probabilities associated to the return level. For example,
#'   a 100 years return period is equivalent to \code{p = 0.99}.
#'
#' @param se Return the standard deviation of the return level using
#'   the delta method. The fitted model must
#'
#' @param ci Method to compute the confident interval. One of \code{'delta'}
#' for the delta method, \code{'boot'} for parametric boostrap and \code{'norm'}
#' for Monte-Carlo approximation assuming normality of the parameters.
#'
#' @param alpha Probability outside the confident interval.
#'
#' @param nsim Number of simulation use for resampling.
#'
#' @param out.matrix Logical. Should the resampling be returned. If true,
#'   a list is returned containing the prediction table (\code{pred}),
#'   the parameters (\code{para}) and the return levels (\code{qua}).
#'   
#' @param ... Other parameters.
#'
#' @importFrom lmomco qlmomco rlmomco vec2par 
#' @export
#'
#' @examples
#'
#' #' ## Extract an time series of annual maxima
#' x <- ExtractAmax(flow~date, flowStJohn)$flow
#'
#' ## Fitting of GEV distribution using L-moments
#' fit <- FitAmax(x,'gev', method = 'mle')
#'
#' ## Get the estimated quantile of 10 and 100 years return period
#' rp <- 1-1/c(10,100)
#' predict(fit, rp)
#' predict(fit, se = TRUE, ci = 'delta')
#'
#' ## The bootstrap sample used for CI are returned
#' fit <- FitAmax(x,'gev', varcov = FALSE)
#' boot <- predict(fit, rp, se = FALSE, ci = 'boot',
#'                 nsim = 500, out.matrix = TRUE)
#'
predict.amax <- 
  function(object, 
           p = c(.5, .8, .9, .95, .98, .99),
           se = FALSE, 
           ci = 'none',
           alpha = .05, 
           nsim = 1000,
           out.matrix = FALSE, ...){

  n <- length(object$data)

  ## Function that compute the return level for a given vector of parameter
  qfun <- function(z, p0){
    qlmomco(p0, vec2par(z, object$distr))
  }

  ## Compute Return level
  ans <- qfun(object$para, p)

  ## Compute confident intervals by resampling techniques
  if(ci == 'boot'){

    ## function that fit the data
    ffun <- function(b){ 
       tryCatch(FitAmax(b, distr = object$distr, method = object$method, 
                           varcov = FALSE)$para,
                error =  function(e) NULL)
    }

    p0 <- vec2par(object$para, object$distr)
    xboot <- replicate(nsim, rlmomco(n,p0), simplify = FALSE)
    
    pboot <- lapply(xboot, ffun)
    pboot <- pboot[!sapply(pboot, is.null)]

    pboot <- do.call(rbind, pboot)

  } else if(ci == 'norm'){

    ## Verify requirements for using the delta method
    if(any(is.na(object$varcov)))
      stop('Covariance matrix was not estimated')

    pboot <- mnormt::rmnorm(nsim, object$para, object$varcov)

  } else
    pboot <- NA



  if(!any(is.na(pboot))){
    ## Compute the return level
    qboot <- t(apply(pboot,1, qfun, p))

    ## if only one return period was passed in argument
    ## pivot the matrix
    if(all(dim(qboot) == c(1,nsim)))
      qboot <- t(qboot)

    ## Compute the confident interval
    bnd <- t(apply(qboot, 2, quantile, c(alpha/2,1 - alpha/2)))

    colnames(bnd) <- c('lower','upper')
    colnames(qboot) <- format(p, digits = 3)
  }


  ## Compute the standard deviation via the delta method
  if(se | ci == 'delta'){

    ## Verify requirements for using the delta method
    if(any(is.na(object$varcov)))
      stop('Covariance matrix was not estimated')

    ## could eventually be replaced by analogic formulas
    g <- sapply(p, function(z) numDeriv::grad(qfun, object$para, p0 = z))
    sig <- sqrt(diag(crossprod(g, object$varcov %*% g)))

    if(ci == 'delta'){
      nq <- abs(qnorm(alpha/2))
      bnd <- cbind(lower = ans - sig * nq, upper = ans + sig * nq)
    }

  }

  ## Built the final output
  ans <- data.frame(pred = ans)

  if(se)
    ans <- cbind(ans, se = sig)

  if(ci %in% c('boot','delta','norm'))
    ans <- cbind(ans, bnd)

  if(ncol(ans) == 1){
    ans <- ans[,1]

  } else {
    rownames(ans) <- format(p, digits = 3)
  }

  if(out.matrix)
    ans <- list(pred = ans, para = pboot, qua = qboot)

  return(ans)
}