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
fglo <- function(x, p0 = NULL, ...){
  
  x <- as.numeric(x)
  
  nllik <- function(para){
    -sum(dglo(x,para[1],  exp(para[2]), para[3], log = TRUE))
  }
  
  if(is.null(p0))
    p0 <- fAmax(x, 'glo')
    
  p0[2] <- log(p0[2]) + 1
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
rglo <- function(n, xi, alf, kap)
  qglo(runif(n), xi, alf, kap)