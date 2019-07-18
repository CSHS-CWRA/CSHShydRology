############################################################################
#' Basic probability functions for distribution of annual maximums
#'
#' Density, distribution function, quantile function and random generation
#' for various distribution used in the modeling annual maximums.
#'
#' @author Martin Durocher <mduroche@@uwaterloo.ca>
#'
#' @name Amax
#' 
#' @param x,q Vector of quantiles.
#' 
#' @param p Vector of probabilities.
#' 
#' @param n	Number of observations.
#' 
#' @param l L-moments.
#' 
#' @param para Vector of parameters for the given distribution.
#' 
#' @param distr Distribution family. See \link{lmom2par}.
#' 
#' @param log Logical. If TRUE, probabilities \code{p} are given as
#'   \code{log(p)}.
#'   
#' @param ... Other parameters.
#' 
#' @export
#'
#' @examples
#'
#' u <- runif(5)
#' u
#' x <- qAmax(u, c(100,3,.001), 'gno')
#' pAmax(x, c(100,3,.001), 'gno')
#'
#' x <- rAmax(100, c(100,30,0), 'gev')
#' sum(dAmax(x, c(100,30, 0), 'gev', log = TRUE))
#'
#' lAmax(c(100, .3, .2), 'gev', lscale = FALSE)
#'
#'

#' @export
#' @rdname Amax
dAmax <- function(x, para, distr, log = FALSE){
  ans <- lmomco::dlmomco(x, lmomco::vec2par(para,distr))

  if(log)
    ans <- log(ans)

  return(ans)
}

#' @export
#' @rdname Amax
pAmax <- function(q, para, distr)
  lmomco::plmomco(q, lmomco::vec2par(para,distr))

#' @export
#' @rdname Amax
qAmax <- function(p, para, distr)
  lmomco::qlmomco(p, lmomco::vec2par(para,distr))

#' @export
#' @rdname Amax
rAmax <- function(n, para, distr)
  lmomco::rlmomco(n, lmomco::vec2par(para,distr))

#' @export
#' @rdname Amax
lAmax <- function(l, distr, ...)
  lmomco::lmom2par(lmomco::vec2lmom(l, ...), distr)$para
