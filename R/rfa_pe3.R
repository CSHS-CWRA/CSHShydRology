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