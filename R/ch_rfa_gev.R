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
rgev <- function(n, xi, alf, kap)
  qgev(runif(n), xi, alf, kap)
