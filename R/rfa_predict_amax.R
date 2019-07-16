######################################################################
#' Predict return levels
#'
#' Return the flood quantile of annual maximum distribution and
#' Confident intervals are provided by bootstrap.
#'
#' @param obj Output from  \code{\link{FitAmax}}
#'
#' @param q Probabilities associated to the return level. For example,
#'   a 100 years return period is equivalent to \code{q = 0.99}.
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
predict.amax <- function(obj, q = c(.5, .8, .9, .95, .98, .99),
                           se = FALSE, ci = 'none',
                           alpha = .05, nsim = 1000,
                           out.matrix = FALSE){

  n <- length(obj$data)

  ## Function that compute the return level for a given vector of parameter
  FunQ <- function(z, q0){
    lmomco::qlmomco(q0, lmomco::vec2par(z, obj$distr))
  }

  ## Compute Return level
  ans <- FunQ(obj$para, q)

  ## Compute confident intervals by resampling techniques
  if(ci == 'boot'){

    ## using Parametric bootstrap
    bootp <- matrix(NA,nsim,length(obj$para))
    p0 <- lmomco::vec2par(obj$para, obj$distr)

    for(ii in 1:nsim){

      repeat{
        ## simulate
        b <- lmomco::rlmomco(n,p0)

        ## estimate
        if(obj$method == 'gml'){
          suppressWarnings(p <- try(
            FitGev(b, varcov = FALSE, mu = obj$prior[1], sig2 = obj$prior[2]),
            silent = TRUE))

        } else {
          suppressWarnings(p <- try(
            FitAmax(b, distr = obj$distr, method = obj$method, varcov = FALSE),
            silent = TRUE))
        }

        ## Sample only feasible set
        if(any(class(p) != 'try-error'))
          if(p$method == obj$method)
            break

      }# end repeat


      bootp[ii,] <- p$para

    }# end for

  } else if(ci == 'norm'){

    ## Verify requirements for using the delta method
    if(any(is.na(obj$varcov)))
      stop('Covariance matrix was not estimated')

    bootp <- mnormt::rmnorm(nsim, obj$para, obj$varcov)

  } else
    bootp <- NA



  if(!any(is.na(bootp))){
    ## Compute the return level
    bootq <- t(apply(bootp,1, FunQ, q))

    ## if only one return period was passed in argument
    ## pivot the matrix
    if(all(dim(bootq) == c(1,nsim)))
      bootq <- t(bootq)

    ## Compute the confident interval
    bnd <- t(apply(bootq, 2, quantile, c(alpha/2,1 - alpha/2)))

    colnames(bnd) <- c('lower','upper')
  }


  ## Compute the standard deviation via the delta method
  if(se | ci == 'delta'){

    ## Verify requirements for using the delta method
    if(any(is.na(obj$varcov)))
      stop('Covariance matrix was not estimated')

    ## could eventually be replaced by analogic formulas
    g <- sapply(q, function(z) numDeriv::grad(FunQ, obj$para, q0 = z))
    sig <- sqrt(diag(crossprod(g, obj$varcov %*% g)))

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
    rownames(ans) <- format(q, digits = 3)
  }

  if(out.matrix)
    ans <- list(pred = ans, para = bootp, qua = bootq)

  return(ans)
}