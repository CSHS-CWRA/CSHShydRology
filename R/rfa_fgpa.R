###############################################################
#' Estimation of the Generalized Pareto distribution.
#'
#' Low level functions for estimating of the generalized Pareto
#' distribution(GPA) with two parameters. Can use
#' either maximum likelihood or the method of L-moments.
#' The algorithm of \code{fgpa2d} is using
#' \code{optim} to directly optimize the log-likelihood (bivariate), while
#' the algorithm of \code{fgpa1d} is using a transformation to use
#' a univariate optimization routine. Additionally, \code{fgpa2d} constraint
#' the shape parameter between -.5 and 1.
#' 
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @name fgpa
#' 
#' @param x Sample.
#'
#' @param sol Does solution from \code{optim} be returned.
#'   In case of \code{fgpa1d}, it returns the variance covariance matrix.
#'
#' @param par0 Initial parameter.
#'
#' @param ... aditional arguments to pass to \code{\link{optim}}
#'
#' @section Reference:
#'
#' Davison AC, Smith RL. (1990) Models for Exceedances over High Thresholds.
#'   Journal of the Royal Statistical Society Series B (Methodological).
#'   52(3):393-442.
#'
#' Hosking JRM (1990). L-Moments: Analysis and Estimation of Distributions Using
#'   Linear Combinations of Order Statistics. Journal of the Royal Statistical
#'   Society Series B (Methodological). 52(1):105-24.
#'
#' @export
#'
#' @examples
#'
#' x <- rgpa(1000, 1, -.2)
#' fgpa1d(x)
#' fgpa2d(x)
#' fgpaLmom(x)
#'
fgpa1d <- function(x, sol = FALSE){

  fk <- function(sigma) -mean(log(1-x/sigma))

  fpara <- function(sigma){
    length(x) * (-log(fk(sigma) * sigma) + fk(sigma) - 1)
  }

  mx <- max(x)

  sigma <- NA
  suppressWarnings(try(
    sigma <- optimize(fpara, interval = c(-10000,10000) * mx,
                      maximum = TRUE)$maximum))

  para <- rep(NA,2)
  names(para) <- c('alpha','kappa')
  try(para[2] <- fk(sigma))
  try(para[1] <- para[2]*sigma)

  if(sol){
    ans <- list(par = para, varcov = matrix(NA,2,2))
    
    ## evaluate the covariance matrix
    ans$varcov[2,2] <- 1-para[2]
    ans$varcov[1,1] <- 2 * para[1]^2
    ans$varcov[1,2] <- ans$varcov[2,1] <- para[1]
    ans$varcov <- ans$varcov * ans$varcov[2,2] / length(x)

  } else {
    ans <- para
  }

  return(ans)

}

#' @export
#' @rdname fgpa
fgpa2d <- function(x, sol = FALSE, par0 = NULL, ...){

  logit0 <- function(z) logit((z+.5)/1.5)
  expit0 <- function(z) expit(z)*1.5 -.5

  ## negative loglikelihood
  nllik <- function(para) -sum(dgpa(x,exp(para[1]),
                                    expit0(para[2]), log = TRUE))

  if(!is.null(par0)){
    par0[1] <- log(par0[1])
    par0[2] <- logit0(par0[2])
  } else
    par0 <- c(log(mean(x)),-.6931472)

  ## estimate the parameter
  para <- optim(par0, nllik, ...)$par
  para[1] <- exp(para[1])
  para[2] <- expit0(para[2])

  names(para) <- c('alpha','kappa')

  if(sol){

    ## compute the covariance matrix using the Hessian
    nllik <- function(para) -sum(dgpa(x,para[1], para[2], log = TRUE))
    hs <- optimHess(para, nllik)
    vc <- try(chol2inv(chol(hs)))
    
    ## correct for positive-definite if necessary
    if(class(vc) == 'try-error'){
      hs <- as.matrix(Matrix::nearPD(hs, doSym = TRUE)$mat)
      vc <- chol2inv(chol(hs))
    }
      
    ans <- list(par = para, varcov = vc)

  } else {
    ans <- para
  }

  return(ans)
}

#' @export
#' @rdname fgpa
fgpaLmom <- function(x){
  beta <- lmomco::pwm(x, nmom = 2)$betas
  k <- beta[1]/(2*beta[2]-beta[1]) - 2
  a <- beta[1] * (1 + k)
  
  return(c(alpha = a, kappa = k))
}

#' @export
#' @rdname fgpa
fgpaMom <- function(x){
  m <- mean(x)
  s2 <- var(x)
  k <- 0.5 * (m^2 / s2 - 1)
  a <- m * (1+k)
  
  return(c(alpha = a, kappa = k))
}


########################################################################
# Logit function
#
# Return the logit of a functon or its inverse.
#
logit <- function(x) 
  log(x/(1-x))

expit <- function(x) 
  exp(x)/(1 + exp(x))
