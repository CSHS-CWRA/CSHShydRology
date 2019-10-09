#' @export
#' @rdname FitNsAmax
predict.nsamax <- 
  function(object, 
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE){
    
  if(is.null(newdata))
    newdata <- object$data
    
  if(!reliability){
    ans <- .predict.nsamax.rt(object, p, newdata)
  } else {
    ans <- .predict.nsamax.rel(object, p, newdata)
  }
  
  return(ans)  
}

.predict.nsamax.rel <- function(object, p, newdata){
 
  ## Evaluate trend
  xmat <- model.matrix(object$formula, model.frame(object$formula, newdata))
  mu <- as.numeric(xmat %*% object$beta)
  
  if(object$type == 'mult')
    mu <- exp(mu)
  
  ## evaluate the reliability level
  rel <- log(p) * nrow(newdata)

  ## evaluate the quantile using the index-flood equation
  q0 <- lmomco::qlmomco(p, object$para)

  if(object$type == 'add'){
    Frel <- function(z, jj) rel[jj]-sum(log(lmomco::plmomco(z-mu, object$para)))
    q0 <-  outer(mu, q0, '+')
  
  } else if(object$type == 'mult'){
    Frel <- function(z, jj) rel[jj]-sum(log(lmomco::plmomco(z/mu, object$para)))
    q0 <- outer(mu, q0, '*')
  }
  
  ## determine Bound for searching
  bnd <- apply(q0, 2, range)  
  bnd[1,] <- bnd[1,] - sqrt(.Machine$double.eps)
  bnd[2,] <- bnd[2,] + sqrt(.Machine$double.eps)
  
  ans <- rep(0,length(rel))
  for(jj in seq_along(rel))
    ans[jj] <- uniroot(Frel, bnd[,jj], jj)$root
  
  return(ans)
}

.predict.nsamax.rt <- 
  function(object, 
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL){
    
  xmat.new <- model.matrix(object$formula, newdata)
  
  if(object$type == 'mult'){
      zhat <- lmomco::qlmomco(p,object$para)
      yhat <- as.numeric(exp(xmat.new %*% object$beta))
      ans <- outer(yhat, zhat, '*')
    
  } else if(object$type == 'add'){
      zhat <- lmomco::qlmomco(p, object$para)
      yhat <- as.numeric(xmat.new %*% object$beta)
      ans <- outer(yhat, zhat, '+')
  }
  
  return(ans)
}


#' @export
#' @rdname FitNsAmax
BootNsAmax <- 
  function(object,
           p = c(0.5, 0.8, 0.9, 0.95, 0.98, 0.99),
           newdata = NULL,
           reliability = FALSE,
           nsim = 1000, 
           alpha = 0.95, 
           verbose = TRUE){
  
  if(is.null(newdata))
    newdata <- object$data  
  
  paras <- matrix(0, nsim, length(object$para$para))
  betas <- matrix(0, nsim, length(object$beta))
  
  if(reliability){
    quas <- matrix(0, nsim, length(p))
  } else {
    quas <- array(0, dim = c(nrow(newdata), length(p), nsim))
  }
  
  xjj <- object$data
  
  if(verbose)
    bar <- txtProgressBar()
  
  for(jj in 1:nsim){
    
    if(verbose)
      setTxtProgressBar(bar, jj / nsim)
    
    ## Simulate a bootstrap sample
    xjj[,1] <- simulate(object)
    
    ## Fit the model
    fit <- FitNsAmax(form = object$formula, x = xjj,
                     distr = object$para$type, type = object$type) 
    
    paras[jj,] <- fit$para$para
    betas[jj,] <- fit$beta
    
    if(reliability){
      quas[jj,] <- predict(fit, p, newdata, TRUE)
    } else{
      quas[,,jj] <- predict(fit, p, newdata, FALSE)
    }
  }    
  
  colnames(paras) <- names(fit$para$para)
  colnames(betas) <- names(fit$beta)
  colnames(quas) <- as.character(round(p,3))
  
  ans <- list(para = paras,
              beta = betas,
              qua = quas)
  
  class(ans) <- 'bootns'
  
  return(ans)
}

#' @export
#' @rdname FitNsAmax
summary.bootns <- function(object, variable = 'para', alpha = 0.05){
 
  if(variable == 'qua' & length(dim(object$qua)) == 3 ){
    id <- 1:2
  } else{
    id <- 2
  }
      
  vmu <- apply(object[[variable]], id, mean)
  vse <- apply(object[[variable]], id, sd)
  vlb <- apply(object[[variable]], id, quantile, alpha/2)
  vub <- apply(object[[variable]], id, quantile, 1-alpha/2)
  
  ans <- data.frame(mean = vmu, se = vse, lower = vlb, upper = vub)
  
  return(ans)
}