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
fgno <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dgno(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0))
    p0 <- fAmax(x, 'gno')
    
  p0[2] <- log(p0[2])  
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
rgno <- function(n, xi, alf, kap)
  qgno(runif(n), xi, alf, kap)

#' @export
#' @rdname Amax
fnor <- function(x){
  x <- as.numeric(x)
  mu <- sum(x)/length(x)
  sigma <- sqrt(sum((x - mu)^2)/length(x))
 
  return(c(mu = mu, sigma = sigma))
  
}
