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
#' @param xi,alf,kap,mu,sig,g Location, scale and shape parameters.
#' 
#' @param p0 Starting parameter for estimation using maximum likelihood.
#'   
#' @param ... Other parameters passed to \link{optim} or \link{lmom2par}.
#' 
#' @export
#' 
#' @seealso \link{FitAmax}, \link{optim}.
#' 
#' @details 
#' 
#' The familly of \code{Amax} functions wraps the routines available in 
#' \link{lmomco-package} in order to simplify their usage within the family of apply
#' functions. 
#' 
#' The other functions provide a R-like interface to the Generalized Extreme Value
#' (GEV), Generalized Logistic (GLO), Generalized normal (GNO) and Pearson type
#' III (PE3) distributions.  
#' The f-functions such as \code{fgev} are low-level functions that 
#' carries out parameters estimation by maximum likelihood. 
#' The likelihood is optimized by the function \link{optim} 
#' to control the optimization process.
#'
#' @examples
#'
#' ## Create a list of 5 uniform sample
#' u <- replicate(5, runif(50), simplify = FALSE)
#' 
#' x <- lapply(u, qAmax, c(100,3,.001), 'gno')
#' p <- lapply(x, pAmax, c(100,3,.001), 'gno')
#' d <- lapply(x, dAmax, c(100,3,.001), 'gno')
#'
#' ## Estimation using L-moments
#' ll <- lapply(x, lmom::samlmu)
#' para <- lapply(ll, lAmax, 'gno')
#' para <- lapply(x, fAmax, 'gno')
#'
#' ## R-like Interface
#' x <- rgev(1000,100, 30, -.05)
#' p <- pgev(x, 100, 30, -0.05)
#' x <- qgev((1:1000)/1001, 100, 30, -0.05)
#' d <- dgev(x, 100, 30, -0.05)
#' f <- fgev(x)
#' f <- fpe3(x, method = 'BFGS', control = list(maxit = 1000))
#' 


#' @importFrom lmomco dlmomco vec2par
#' @export
#' @rdname Amax
dAmax <- function(x, para, distr, log = FALSE){
  ans <- dlmomco(x, vec2par(para,distr))

  if(log)
    ans <- log(ans)

  return(ans)
}

#' @importFrom lmomco lmom2par pwm2lmom pwm
#' @export
#' @rdname Amax
fAmax <- function(x, distr){
  pw <- pwm(x, npAmax(distr))
  return(lmom2par(pwm2lmom(pw), distr)$para)
}

#' @importFrom lmomco lmom2par vec2lmom
#' @export
#' @rdname Amax
lAmax <- function(l, distr, ...)
  lmom2par(vec2lmom(l, ...), distr)$para


#' @export
#' @rdname Amax
npAmax <- function(distr){

  if(distr %in% c('emu','exp','gam','gum','kmu','kur','lap','lmrq','nor',
                  'ray','revgum','rice','sla')){
    ans <- 2 
  } else if(distr %in% c('gep', 'gev','glo','gno', 'gov', 'gpa','ln3','pe3',
                       'st3','texp','wei')){
    ans <- 3
  } else if(distr %in% c('aep4','gld','kap')){
    ans <- 4  
  } else{
    ans <- 5
  }
    
  return(ans)
} 

#' @importFrom lmomco plmomco vec2par
#' @export
#' @rdname Amax
pAmax <- function(q, para, distr)
  plmomco(q, vec2par(para,distr))

#' @importFrom lmomco qlmomco vec2par
#' @export
#' @rdname Amax
qAmax <- function(p, para, distr)
  qlmomco(p, vec2par(para,distr))

#' @importFrom lmomco rlmomco vec2par
#' @export
#' @rdname Amax
rAmax <- function(n, para, distr)
  rlmomco(n, vec2par(para,distr))
