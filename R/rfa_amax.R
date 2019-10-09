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
#' @seealso \link{FitAmax}, \link{optim}.
#' 
#' @details 
#' 
#' The `f-functions` such as `fgev` a low-level functions that provide the 
#' parameters estimated by the technique of maximum likelihood. 
#' The likelihood is optimized by the function `optim` where the other arguments
#' `...` are used to control the optimization process.
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
#' x <- rgev(1000,100, 30, -.05)
#' p <- pgev(x, 100, 30, -0.05)
#' x <- qgev((1:1000)/1001, 100, 30, -0.05)
#' d <- dgev(x, 100, 30, -0.05)
#' f <- fgev(x)
#' f <- fpe3(x, method = 'BFGS', control = list(maxit = 1000))

#' @importFrom lmomco dlmomco vec2par
#' @export
#' @rdname Amax
dAmax <- function(x, para, distr, log = FALSE){
  ans <- dlmomco(x, vec2par(para,distr))

  if(log)
    ans <- log(ans)

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

#' @importFrom lmomco lmom2par vec2lmom
#' @export
#' @rdname Amax
lAmax <- function(l, distr, ...)
  lmom2par(vec2lmom(l, ...), distr)$para

#' @importFrom lmomco lmom2par pwm2lmom pwm
#' @export
#' @rdname Amax
fAmax <- function(x, distr, nmax = 3){
  pw <- pwm(x, nmax)
  return(lmom2par(pwm2lmom(pw), distr)$para)
}
  

##############################################################################
## GEV ##
##############################################################################

#' @export
#' @rdname Amax
pgev <- function (x, xi, alf, kap){
    
  p <- vector(mode = "numeric", length = length(x))
  y <- (x - xi)/alf
    
  if (kap == 0) {
    p <- exp(-exp(-y))
  
  } else {
    y <- suppressWarnings(-log(1 - kap * y)/kap)
    p <- exp(-exp(-y))
  }
  
  if (kap < 0) {
    p[!is.finite(p)] <- 0
  
  } else if (kap > 0) {
    p[!is.finite(p)] <- 1
  }
    
  names(p) <- NULL
  return(p)
}

#' @export
#' @rdname Amax
qgev <- function (p, xi, alf, kap){
  
  y <- suppressWarnings(-log(-log(p)))

  if (abs(kap) > sqrt(.Machine$double.eps)) 
    y <- (1 - exp(-kap * y))/kap
    
  x <- xi + alf * y
  z <- xi + alf/kap

  id0 <- (p == 0 & kap < 0)
  id1 <- (p == 1 & kap > 0)
  
  if(length(z) == 1){
    x[id0] <- z
    x[id1] <- z
  
  } else{
    x[id0] <- z[id0]
    x[id1] <- z[id1]
  }
    
  names(x) <- NULL
  return(x)
}

#' @export
#' @rdname Amax
dgev <- function(x, xi, alf, kap, log = FALSE){
  y <- (x - xi)/alf
  
  if (kap != 0) {
        z <- 1 - kap * y
        y <- suppressWarnings(-log(z)/kap)
  }
  
  d <- exp(-(1 - kap) * y - exp(-y)) / alf
  
  names(d) <- NULL
  d[!is.finite(d)] <- NA
  d[is.na(d)] <- 0
  
  if(log)
    d <- log(d)
  
  return(d)
}

#' @export
#' @rdname Amax
rgev <- function(n, xi, alf, kap)
  qgev(runif(n), xi, alf, kap)

#' @export
#' @rdname Amax
fgev <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dgev(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0)){
    m <- mean(x)
    s <- sd(x)
    p0 <- c(m + .58 * s , log(.52*s), -.1)
  }
    
  out <- try(optim(p0, nllik, ...), silent = TRUE)
  
  if(class(out) == 'try-error'){
    p0[3] <- -p0[3]
    out <- optim(p0, nllik, ...)  
  }
    
  if(out$convergence != 0)
    warning('The solution may not have converged.')
  
  ans <- out$par
  ans[2] <- exp(ans[2])
  
  names(ans) <- c('xi','alpha','kappa')
  
  return(ans)
 
}

##############################################################################
## GLO ##
##############################################################################

#' @export
#' @rdname Amax
pglo <- function (x, xi, alf, kap){

  p <- vector(mode = "numeric", length = length(x))
  y <- (x - xi)/alf
  if (kap == 0) {
      p <- 1/(1 + exp(-y))
  }
  else {
      y <- suppressWarnings(-log(1 - kap * y)/kap)
      p <- 1/(1 + exp(-y))
  }
  if (kap < 0) {
      p[!is.finite(p)] <- 0
  }
  else if (kap > 0) {
      p[!is.finite(p)] <- 1
  }
  
  names(p) <- NULL
  return(p)
}

#' @export
#' @rdname Amax
qglo <- function (p, xi, alf, kap){

  y <- suppressWarnings(log(p/(1 - p)))

  if (abs(kap) > sqrt(.Machine$double.eps)) 
      y <- (1 - exp(-kap * y))/kap
    
  x <- xi + alf * y
  z <- xi + alf/kap

  id0 <- (p == 0 & kap < 0)
  id1 <- (p == 1 & kap > 0)
  
  if(length(z) == 1){
    x[id0] <- z
    x[id1] <- z
  
  } else{
    x[id0] <- z[id0]
    x[id1] <- z[id1]
  }
  
  names(x) <- NULL
  return(x)
}

#' @export
#' @rdname Amax
dglo <- function (x, xi, alf, kap, log = FALSE){

    y <- (x - xi)/alf
    
    if (kap != 0) 
        y <- suppressWarnings(-log(1 - kap * y)/kap)
    
    d <-  exp(-(1 - kap) * y) / (1 + exp(-y))^2 / alf
    
    names(d) <- NULL
    d[!is.finite(d)] <- NA
    d[is.na(d)] <- 0
    
    if(log)
      d <- log(d)
    
    return(d)
}

#' @export
#' @rdname Amax
rglo <- function(n, xi, alf, kap)
  qglo(runif(n), xi, alf, kap)

#' @export
#' @rdname Amax
fglo <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dglo(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0)){
    m <- mean(x)
    s <- sd(x)
    p0 <- c(m, log(1.81*s), -.1)
  }
    
  out <- try(optim(p0, nllik, ...), silent = TRUE)
  
  if(class(out) == 'try-error'){
    p0[3] <- -p0[3]
    out <- optim(p0, nllik, ...)  
  }
    
  if(out$convergence != 0)
    warning('The solution may not have converged.')
  
  ans <- out$par
  ans[2] <- exp(ans[2])
  
  names(ans) <- c('xi','alpha','kappa')
  
  return(ans)
 
}

###############################################################################
## PE3
###############################################################################

#' @export
#' @rdname Amax
ppe3 <- function (x, mu, sig, g){
  
    if (abs(g) <= sqrt(.Machine$double.eps)){ 
        ans <- pnorm((x - mu)/sig)
        names(ans) <- NULL
        return(ans)
    }
      
    za <- 4/g^2
    zb <- 0.5 * sig * abs(g)
    xi <- mu - 2 * sig/g
    
    if (g > 0){ 
      ans <- pgamma((x - xi)/zb, za)
    } else{
      ans <- 1 - pgamma((xi - x)/zb, za)
    }

    names(ans) <- NULL
    return(ans)
}

#' @export
#' @rdname Amax
qpe3 <- function (p, mu, sig, g){

  x <- vector(mode = "numeric", length = length(p))
  
  if (abs(g) <= sqrt(.Machine$double.eps)) {
      x <- mu + sig * qnorm(p)
  
  } else {
    za <- 4/g^2
    zb <- abs(0.5 * sig * g)
    
    if (g > 0) {
      x <- mu - za * zb + qgamma(p, za, scale = zb)
    } else {
      x <- mu + za * zb - qgamma(1 - p, za, scale = zb)
    }
  
  }
  
  z <- mu - 2 * sig/g
  id0 <- (p == 0 & g > 0)
  id1 <-(p == 0 & g > 0)
  
  if(length(z) == 1){
    x[id0] <- z
    x[id1] <- z
  
  } else{
    x[id0] <- z[id0]
    x[id1] <- z[id1]
  }
  
  names(x) <- NULL
  return(x)
}

#' @export
#' @rdname Amax
dpe3 <- function (x, mu, sig, g, log = FALSE){

    if (abs(g) <= sqrt(.Machine$double.eps)) 
        return(dnorm(x, mean = mu, sd = sig))
    
    za <- 4/g^2
    zb <- (1/2) * sig * abs(g)
    xi <- mu - 2 * sig/g
    y <- sign(g) * (x - xi)
    d <- dgamma(y/zb, za)/zb
    
    names(d) <- NULL
    d[!is.finite(d)] <- NA
    d[is.na(d)] <- 0
    
    if(log)
      d <- log(d)
    
    return(d)
}

#' @export
#' @rdname Amax
rpe3 <- function(n, mu, sig, g)
  qpe3(runif(n), mu, sig, g)

#' @export
#' @rdname Amax
fpe3 <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dpe3(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0)){
    #m <- mean(x)
    #s <- sd(x)
    #para <- c(m,s, -.1)
    pw <- lmomco::pwm(x,3)
    p0 <- lmomco::lmom2par(lmomco::pwm2lmom(pw), 'pe3')$para
  }
    
  out <- try(optim(p0, nllik, ...), silent = TRUE)
  
  if(class(out) == 'try-error'){
    p0[3] <- -p0[3]
    out <- optim(p0, nllik, ...)  
  }
    
  if(out$convergence != 0)
    warning('The solution may not have converged.')
  
  ans <- out$par
  ans[2] <- exp(ans[2])
  
  names(ans) <- c('mu','sigma','gamma')
  
  return(ans)
 
}

###############################################################################

#' @export
#' @rdname Amax
pgno <- function (x, xi, alf, kap){
  
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    RTHALF <- 0.707106781186548

    y <- (x - xi)/alf
    
    if (kap != 0) 
        y <- suppressWarnings(-log(1 - kap * y)/kap)
    
    p <- 0.5 + 0.5 * erf(y * RTHALF)
    if (kap < 0) {
        p[!is.finite(p)] <- 0
    }
    else if (kap > 0) {
        p[!is.finite(p)] <- 1
    }
    
    names(p) <- NULL
    return(p)
}

#' @export
#' @rdname Amax
qgno <- function (p, xi, alf, kap){

  y <- qnorm(p)
    
  if (abs(kap) > sqrt(.Machine$double.eps)) 
    y <- (1 - exp(-kap * y))/kap
    
  x <- xi + alf * y
  z <- xi + alf/kap

  id0 <- (p == 0 & kap < 0)
  id1 <- (p == 1 & kap > 0)
  
  if(length(z) == 1){
    x[id0] <- z
    x[id1] <- z
  
  } else{
    x[id0] <- z[id0]
    x[id1] <- z[id1]
  }
  
  names(x) <- NULL
  return(x)
}

#' @export
#' @rdname Amax
dgno <- function (x, xi, alf, kap, log = FALSE){

    y <- (x - xi)/alf
    if (kap != 0) {
        z <- 1 - kap * y
        y <- suppressWarnings(-log(z)/kap)
    }
    
    d <- exp(kap * y - y^2/2)/(alf * sqrt(2 * pi))
    
    names(d) <- NULL
    d[!is.finite(d)] <- NA
    d[is.na(d)] <- 0
    
    if(log)
      d <- log(d)
    
    return(d)
}

#' @export
#' @rdname Amax
rgno <- function(n, xi, alf, kap)
  qgno(runif(n), xi, alf, kap)

#' @export
#' @rdname Amax
fgno <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dgno(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0)){
    m <- mean(x)
    s <- sd(x)
    p0 <- c(m,s, -.1)
  }
    
  out <- try(optim(p0, nllik, ...), silent = TRUE)
  
  if(class(out) == 'try-error'){
    p0[3] <- -p0[3]
    out <- optim(para, nllik, ...)  
  }
    
  if(out$convergence != 0)
    warning('The solution may not have converged.')
  
  ans <- out$par
  ans[2] <- exp(ans[2])
  
  names(ans) <- c('xi','alpha','kappa')
  
  return(ans)
 
}
